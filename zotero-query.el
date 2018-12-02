;; for database schema, refer to
;; https://github.com/zotero/zotero/blob/master/resource/schema/userdata.sql

(require 'cl)
(require 'sql)
(require 'dash)

(require 'hydra)

;; zotero query
(require 'json)

;; UTILITY
(defmacro comment (&rest body)
  nil)

(defun zotero--chomp (s)
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defun zotero--quote-% (str)
  (replace-regexp-in-string "%" "%%" str))

(defconst zotero-database-filename "zotero.sqlite")
(defun zotero--find-library-filepath ()
  (when (getenv "UserProfile")
    (let ((maybe-zotero-directory
           (concat
            (file-name-as-directory
             (getenv "UserProfile"))
            "Zotero")))
      (when (file-exists-p maybe-zotero-directory)
        (file-name-as-directory maybe-zotero-directory)))))

(setq zotero-db
      (concat
       (file-name-as-directory
        (zotero--find-library-filepath))
       zotero-database-filename))

(defun zotero--concat-sql-statements
    (&rest statements)
  (let ((debug-mode (eq :debug
                        (first statements))))
    (mapconcat
     'identity
     (if debug-mode
         (rest statements)
       statements)
     (if debug-mode
         "\n"
       " "))))
(defun zotero--read-result-line-to-plist (prop-names result-line)
  (comment
   (zotero--read-result-line-to-plist
    ;; prop-names
    (list 'itemID 'key 'fieldName 'value) 
    ;; result-line
    "8	ABCD1234	DOI	10.0000/journal.xyz.12345"))
  (-interleave
   prop-names
   (split-string result-line "\t")))

(defun zotero--exec-sqlite-query
    (sql-query)
  (zotero--chomp
   (shell-command-to-string
    (format "%s -separator \"\t\" \"file:///%s?mode=ro\" \"%s\""
            sql-sqlite-program
            (replace-regexp-in-string "^/" "" zotero-db)
            sql-query))))

(defun zotero--exec-sqlite-query-to-plist-items
    (sql-query prop-names)
  (let ((result-string (zotero--exec-sqlite-query sql-query)))
    (when (< 0 (length result-string))
      (if (equal "Error: database is locked" result-string)
          (error result-string)
        (mapcar
         (lambda (line)
           (zotero--read-result-line-to-plist prop-names line))
         (split-string result-string "\n"))))))

(defun zotero--aggregate-items-by-key
    (item-property-row-list)
  (comment
   (zotero--aggregate-items-by-key
    '((:key "abc" :title "some title")
      (:key "abc" :date "2018-02-03")
      (:key "xyz" :title "other title")))))

(defun zotero-query-by-tags-and-creators
    (query-string)
  ;; looks for matching substring in *last name* and tags.
  ;; returns a list of property-list items like
  ;; (list (itemID 5 key "abcd" ...)
  ;;       (itemID 29 key "zxcv" ...) ...)
  (let ((sql-query
         (format
          (zotero--concat-sql-statements
           " SELECT"
           "  items.itemID"
           " ,items.key,items.dateModified"
           " ,itemDataValues.value AS title"
           " ,tags"
           " ,creators"
           " FROM items"
           ;; get the entry title
           " JOIN itemData ON itemData.itemID = items.itemID"
           " JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID"
           " JOIN fields ON fields.fieldID = itemData.fieldID"
           ;; get tags
           " LEFT OUTER JOIN"
           " (SELECT itemTags.itemID, GROUP_CONCAT(tags.name, ', ') AS tags"
           "  FROM tags"
           "  JOIN itemTags ON itemTags.tagID = tags.tagID"
           "  GROUP BY itemTags.itemID)"
           " AS tagsQ ON tagsQ.itemID = items.itemID"
           ;; get authors (creators)
           " LEFT OUTER JOIN"
           " (SELECT itemCreators.itemID, GROUP_CONCAT(creators.lastName, ', ') AS creators"
           "  FROM itemCreators"
           "  JOIN creators ON creators.creatorID = itemCreators.creatorID"
           "  GROUP BY itemCreators.itemID)"
           " AS creatorsQ on creatorsQ.itemID = items.itemID"
           " WHERE (   LOWER(tags)     LIKE LOWER('%%%s%%')"
           "        OR LOWER(creators) LIKE LOWER('%%%s%%'))"
           " AND fields.fieldName = 'title'"
           " LIMIT 20")
          query-string
          query-string
          )))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query
     '(itemID key dateModified title tags creators))))

(defun zotero-query-by-attributes
    (query-string)
  ;; looks for matching substring by any of zotero's itemDataValues.value
  ;; returns a property list of (item-key -> item-plist) mappings, like
  ;; (1 (itemID 1 key ABCD
  ;;     doi doi:1234.567 ...)
  ;;  9 (itemID 9 key ZXCV
  ;;     title "tied toll" ...) ...)
  (let ((sql-query
         (format
          (zotero--concat-sql-statements
           " SELECT"
           "  items.itemID"
           " ,items.key"
           " ,fields.fieldName"
           " ,itemDataValues.value"
           " FROM items"
           " JOIN itemData ON itemData.itemID = items.itemID"
           " JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID"
           " JOIN fields ON fields.fieldID = itemData.fieldID"
           " JOIN itemTags ON itemTags.itemID = items.itemID"
           (concat
            ;; TODO: possibly support doi-only
            ;; " AND fields.fieldName = 'DOI'"
            " AND LOWER(itemDataValues.value) LIKE LOWER('%%%s%%')")
           " GROUP BY items.itemID, fields.fieldName"
           " ORDER BY items.itemID ASC"
           " LIMIT 20")
          query-string)))
    (let ((item-property-row-list
           (zotero--exec-sqlite-query-to-plist-items
            sql-query
            '(itemID key fieldName value)))
          out)
      (dolist (item-property-row item-property-row-list out)
        ;; WARNING!
        ;; (eq (intern "asdf") 'asdf)      ;; t
        ;; (eq (make-symbol "asdf") 'asdf) ;; nil! make-symbol makes a NEW symbol
        (let* ((item-id (string-to-number (plist-get item-property-row 'itemID)))
               (item-data (plist-get out item-id)))
          (setq out
                (plist-put
                 out
                 item-id
                 (plist-put
                  item-data
                  (intern (plist-get item-property-row 'fieldName))
                  (plist-get item-property-row 'value)))))))))


(defun zotero-query-by-fulltext
    (search-word)
  (let ((sql-query
         (format
          (zotero--concat-sql-statements
           :debug
           " SELECT"
           ;; NOTE, the target item is the PARENT, not the attachment item!
           "  parentItems.itemID"
           " ,parentItems.key,parentItems.dateModified"
           " ,itemDataValues.value AS title"
           " ,fulltextWords.word"
           " FROM fulltextWords"
           " JOIN fulltextItemWords ON fulltextWords.wordID = fulltextItemWords.wordID"
           " JOIN items AS attachmentItems ON fulltextItemWords.itemID = attachmentItems.itemID"
           " JOIN itemTypes ON attachmentItems.itemTypeID = itemTypes.itemTypeID"
           ;; get the parent item
           " JOIN itemAttachments ON itemAttachments.itemID = attachmentItems.itemID"
           " JOIN items AS parentItems ON itemAttachments.parentItemID = parentItems.itemID"
           ;; get the title of the PARENT item
           " JOIN itemData ON parentItems.itemID = itemData.itemID"
           " JOIN itemDataValues ON itemData.valueID = itemDataValues.valueID"
           " JOIN fields ON itemData.fieldID = fields.fieldID"
           " WHERE 1"
           " AND itemTypes.typeName = 'attachment'"
           " AND fields.fieldName = 'title'"
           " AND fulltextWords.word = LOWER('%s')"
           " GROUP BY parentItems.itemID"
           " LIMIT 20")
          search-word)))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query
     '(itemID key dateModified title word))))

(defun zotero-query-any
    (query-string)
  (let* ((tags-and-creators-matches
          (zotero-query-by-tags-and-creators query-string))
         (full-text-matches
          (zotero-query-by-fulltext
           query-string))
         (attributes-matches
          (zotero-query-by-attributes query-string)))
    (dolist (item-property-row
             (append tags-and-creators-matches
                     full-text-matches)
             attributes-matches)
      (let* ((item-id (string-to-number (plist-get item-property-row 'itemID)))
             (item-attribute-data (plist-get attributes-matches item-id)))
        (setq attributes-matches
              (plist-put
               attributes-matches
               item-id
               (append
                item-attribute-data
                item-property-row)))))))

(defun zotero-get-attachments
    (item-id)
  ;; returns a list of property list items like
  ;; ((key "ASDF" itemId "9" parentItemId "8" contentType "application/pdf" path "storage:foo and bar - publication.pdf") ...)
  (let* ((properties '(itemId parentItemId contentType path))
         (sql-query (format
                     (zotero--concat-sql-statements
                      " SELECT"
                      " items.key,"
                      (mapconcat (lambda (sym)
                                   (concat
                                    "itemAttachments."
                                    (symbol-name sym))) properties ",")
                      " FROM itemAttachments"
                      " JOIN items ON items.itemID = itemAttachments.ItemID"
                      " WHERE itemAttachments.parentItemID = %s")
                     item-id)))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query
     (cons 'key properties))))

(defun zotero-get-attachment-plist-filepath
    (attachment-plist)
  (let ((zotero-internal-path (plist-get attachment-plist 'path)))
    (if (not (string-match-p "^storage:" zotero-internal-path))
        (message "ERROR: don't know how to handle this path: %s"
                 zotero-internal-path)
      (concat
       (file-name-as-directory
        (concat
         (file-name-as-directory
          (zotero--find-library-filepath))
         "storage/"
         (plist-get attachment-plist 'key)))
       (replace-regexp-in-string
        "^storage:" ""
        zotero-internal-path)))))

;; tells us where to look for qnotero_util.py
(defconst zotero-query-dir
  (if load-file-name
      (file-name-directory load-file-name)
    (file-name-directory (buffer-file-name))))

(defun format-org-zotero-link (zotero-key)
  (format "[[zotero:%s]]" zotero-key))

(org-add-link-type "zotero" 'org-zotero-link-open 'org-zotero-link-export)

(defun org-zotero-link-open (link)
  "Open zotero styled link."
  (zotero-query link))

(defun org-zotero-link-export (link description format)
  "FIXME: stub function"
  (concat "link in zotero: " link " (" description ")"))

(defvar zotero-result-buf)
(defvar zotero-output-buf)

(defhydra zotero-insert-menu (:color pink)
  "what to do with string?"
  ("i" (lambda ()
         (interactive)
         (save-excursion
           (insert (format "%s" zotero-output-buf))))
   "insert at point"
   :exit t)
  
  ("c" (lambda ()
         (interactive)
         (kill-new (format "%s" zotero-output-buf))
         (message (format "copied \"%s\"" zotero-output-buf)))
   "copy to clipboard"
   :exit t)

  ("q" nil "cancel"))

(defun set-zotero-active-result (return-key)
  (let ((return-val (plist-get zotero-result-buf return-key)))
    (setq zotero-output-buf
          (format
           "%s"
           (if (eq 'vector (type-of return-val))
               (mapconcat 'identity return-val ", ")
             return-val)))))

(defhydra zotero-result-menu (:color pink :hint nil)
  "what do you want?"
  
  ("o" (lambda ()
         (interactive)
         (org-open-file (plist-get zotero-result-buf :fulltext) t))
   "open with emacs!" :exit t)
  
  ("O" (lambda ()
         (interactive)
         (org-open-file (plist-get zotero-result-buf :fulltext)))
   "open EXTERNALLY" :exit t)
  
  ("l" (lambda ()
         (interactive)
         (save-excursion
           (insert (format "[[pdfview:%s][%s]]"
                           (plist-get zotero-result-buf :fulltext)
                           (plist-get zotero-result-buf :title)))))
   "insert org-pdfview link at point" :exit t)

  ("z" (lambda ()
         (interactive)
         (save-excursion
           (insert
            (format-org-zotero-link
             (if (plist-get zotero-result-buf :doi)
                 (concat "doi:" (plist-get zotero-result-buf :doi))
               (plist-get zotero-result-buf :title))))))
   "insert zotero link at point" :exit t)
  
  ("i" (lambda ()
         (interactive)
         (set-zotero-active-result :id)
         (zotero-insert-menu/body)) "id"
         :exit t)

  ("k" (lambda ()
         (interactive)
         (set-zotero-active-result :key)
         (zotero-insert-menu/body)) "key"
         :exit t)

  ("a" (lambda ()
         (interactive)
         (set-zotero-active-result :authors)
         (zotero-insert-menu/body)) "authors"
         :exit t)

  ("p" (lambda ()
         (interactive)
         (set-zotero-active-result :publication)
         (zotero-insert-menu/body)) "publication"
         :exit t)

  ("t" (lambda ()
         (interactive)
         (set-zotero-active-result :title)
         (zotero-insert-menu/body)) "title"
         :exit t)

  ("g" (lambda ()
         (interactive)
         (set-zotero-active-result :tags)
         (zotero-insert-menu/body)) "tags"
         :exit t)

  ("s" (lambda ()
         (interactive)
         (set-zotero-active-result :simple_format)
         (zotero-insert-menu/body)) "simple format"
         :exit t)

  ("f" (lambda ()
         (interactive)
         (set-zotero-active-result :fulltext)
         (zotero-insert-menu/body)) "file path"
         :exit t)

  ("q" nil "cancel"))

(defun zotero-author-list-string (author-list)
  (propertize
   (let ((nitem (length author-list)))
     (cond ((= 1 nitem)
            (elt author-list 0))
           ((= 2 nitem)
            (mapconcat 'identity author-list ", "))
           (t (concat (elt author-list 0) " et al"))))
   'face '(:foreground "yellow")))

(defun zotero-choose-result (item-list)
  (let* ((counter 0)
         (nres (length item-list))
         (nshow (min 9 nres)))
    (cond ((= 1 nres)
           (setq zotero-result-buf (elt item-list 0))
           (zotero-result-menu/body))
          ((< 0 nres)
           (message nil)
           (let ((selection (string-to-int
                             (char-to-string
                              ;; simple simple menu
                              (read-char
                               (concat (format
                                        "%s results (%s shown), what do?\n"
                                        nres nshow)
                                       (mapconcat #'(lambda (item)
                                                      (incf counter)
                                                      (format "[%s] %s (%s)"
                                                              (propertize (number-to-string counter)
                                                                          'face '(:foreground "SkyBlue"))
                                                              (plist-get item :title)
                                                              (zotero-author-list-string
                                                               (plist-get item :authors))))
                                                  (subseq item-list 0 nshow) "\n")))))))
             (if (and (< 0 selection)
                      (<= selection nres))
                 (progn
                   (setq zotero-result-buf (elt item-list (1- selection)))
                   (zotero-result-menu/body))
               (message "invalid selection")))))))

;; json return structure contains keys:
;; :id
;; :key
;; :authors
;; :publication
;; :title
;; :tags
;; :simple_format   ;;; basic citation
;; :fulltext        ;;; this is actually pdf filepath
(defun zotero-query (&optional input-string)
  (interactive)
  (let* ((default-string (if mark-active (replace-regexp-in-string "[\s\n]+$" "" (buffer-substring (mark) (point)))))
         (search-string (or input-string
                            (read-string
                             (format "search string%s: "
                                     (if default-string
                                         (concat " [" default-string "]")
                                       "")) nil nil default-string)))
         (return-key :fulltext))
    (let ((item-list
           (let ((json-object-type 'plist))
             (json-read-from-string
              (base64-decode-string
               (first
                (last
                 ;; qnotero outputs other operational text; we want the line from the script execution
                 (split-string
                  ;; strip trailing whitespace
                  (replace-regexp-in-string
                   "[\s\n]+$" ""
                   (shell-command-to-string
                    (format "python3 '%s' '%s'"
                            (concat zotero-query-dir "qnotero_util.py")
                            search-string)))
                  "\n"))))))))
      (zotero-choose-result item-list))))



;; PDF annotation interaction code
;; NOTE this isn't zotero-specific, so this will likely be moved in the future

;; pdf annotation code ref http://matt.hackinghistory.ca/2015/11/11/note-taking-with-pdf-tools/

;; temp vars
(defvar pdf-annot-filepath)
(defvar pdf-annot-buf)

(defun format-org-quote (quote-text &optional linktext)
  (format "\n#+BEGIN_QUOTE\n%s\n%s#+END_QUOTE\n"
          quote-text
          (if linktext
              (format "    -- %s\n" linktext)
            "")))

(defun format-org-pdfview-link (filepath &optional text pagenum height)
  (format "[[pdfview:%s%s%s]%s]"
          filepath
          (if pagenum (format "::%d" pagenum) "")
          (if height (format "++%.3f" height) "")
          (if text (format "[%s]" text) "")))

(defun pdf-annot-insert-all ()
  (interactive)
  (save-excursion
    (forward-line)
    (let ((counter 0)
          (ink-page-set '()))
      (mapc
       (lambda (annot) ;; traverse all annotations
         (let ((annot-type (assoc-default 'type annot))
               (page (assoc-default 'page annot)))
           (cond ((eq 'highlight annot-type)
                  (let* (;; use pdf-annot-edges-to-region to get correct boundaries of highlight
                         (real-edges (pdf-annot-edges-to-region
                                      (pdf-annot-get annot 'markup-edges)))
                         (text (or (assoc-default 'subject annot)
                                   (replace-regexp-in-string
                                    "\n" " "
                                    (pdf-info-gettext page real-edges nil pdf-annot-filepath)) ))

                         (comment (assoc-default 'contents annot))
                         
                         (height (nth 1 real-edges)) ;; distance down the page
                         )
                    (insert (format-org-quote
                             text
                             (format-org-pdfview-link pdf-annot-filepath "source" page height)))
                    (when (and comment (< 0 (length comment)))
                      (insert (format "\n# COMMENT:\n  %s\n" comment)))
                    (setq counter (1+ counter))
                    ))
                 ((eq 'text annot-type)
                  (let ((comment (assoc-default 'contents annot))
                        (edges (pdf-annot-get annot 'edges)))
                    (insert (format
                             "\n#+BEGIN_COMMENT\n%s\n#+END_COMMENT\n# COMMENT:\n  referring to %s:\n  %s\n"
                             (pdf-info-gettext page edges nil pdf-annot-filepath)
                             (format-org-pdfview-link pdf-annot-filepath "in text" page (nth 1 edges))
                             comment))))
                 ((eq 'ink annot-type)
                  ;; (insert (format "%s" annot))
                  (add-to-list 'ink-page-set page))
                 (t
                  (progn
                    (message (format "unhandled type: %s" annot-type)))))))
       pdf-annot-buf)
      (message (format "%s" ink-page-set))
      ;; insert the pages with inking
      (dolist (page ink-page-set)
        (insert
         (concat "\n# - "
                 (format-org-pdfview-link pdf-annot-filepath
                                          (format "freehand note on page %s" page)
                                          page)
                 "\n")))
      (message (format "%s items" counter)))))

(defhydra pdf-annot-menu (:color blue)
  "do what?"
  
  ("a" pdf-annot-insert-all
   "insert all"
   :exit t)
  
  ("i" (lambda ()
         (interactive)
         (let ((match
                (helm-comp-read
                 "match: "
                 (let ((helm-completion-list))
                   (mapc
                    (lambda (annot) ;; traverse all annotations
                      (let ((annot-type (assoc-default 'type annot))
                            (page (assoc-default 'page annot)))
                        (cond ((eq 'highlight annot-type)
                               (let* ( ;; use pdf-annot-edges-to-region to get correct boundaries of highlight
                                      (real-edges (pdf-annot-edges-to-region
                                                   (pdf-annot-get annot 'markup-edges)))
                                      (text (or (assoc-default 'subject annot)
                                                (replace-regexp-in-string
                                                 "\n" " "
                                                 (pdf-info-gettext page real-edges nil pdf-annot-filepath)) ))

                                      (comment (assoc-default 'contents annot))
                                      
                                      (height (nth 1 real-edges)) ;; distance down the page
                                      
                                      (full-string (format
                                                    "%s (%s) %s\n" text
                                                    (format-org-pdfview-link pdf-annot-filepath "source" page height)
                                                    (if (and comment (< 0 (length comment)))
                                                        (format "\n# COMMENT:\n  %s" comment)
                                                      ""))))
                                 (setq helm-completion-list
                                       (cons (cons text full-string) helm-completion-list))))
                              ((eq 'text annot-type)
                               (let ((comment (assoc-default 'contents annot))
                                     (edges (pdf-annot-get annot 'edges)))
                                 (setq helm-completion-list
                                       (cons
                                        (cons (format "COMMENT: %s" comment)
                                              (format "# COMMENT %s:\n  %s\n"
                                                      (format-org-pdfview-link
                                                       pdf-annot-filepath
                                                       "in text"
                                                       page
                                                       (nth 1 edges))
                                                      comment))
                                        
                                        helm-completion-list))))
                              ((eq 'ink annot-type)
                               (message "skipping ink item..."))
                              (t
                               (message (format "unhandled type: %s" annot-type))))))
                    pdf-annot-buf)
                   helm-completion-list))))
           ;; this seems to be never false
           (when match
             (forward-line)
             (save-excursion
               (insert match)))))
   "insert one"
   :exit t)

  ("q" nil "cancel"))

(defun pdf-annot-trigger ()
  (interactive)
  (let* ((ctx (org-element-context))
         (filepath (org-element-property :path ctx)))
    (if (not (and (eq 'link (first ctx))
                  ;; for pdfview only
                  ;; (eq 'pdfview (org-element-property :type ctx))
                  (string-match "\\.pdf\\b" (or filepath ""))))
        (message "not a pdf link")
      (progn
        (setq pdf-annot-filepath filepath)
        (setq pdf-annot-buf
              ;; get and sort all annots
              (sort (pdf-info-getannots nil filepath)
                    'pdf-annot-compare-annotations))
        (pdf-annot-menu/body)))))
