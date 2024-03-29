;;; zotero-query.el --- query the Zotero database from emacs

;; Copyright (C) 2014- whacked

;; Author: whacked <whacked@users.noreply.github.com>
;; Version: 0.0.1
;; Package-Requires: (esqlite s dash f hydra)

;; Keywords: zotero, cite, ref, reference manager, database
;; URL: https://github.com/whacked/zotero-query.el

;;; Commentary:

;; Provides convenience functions to query the zotero database

;;; Code:


;; for database schema, refer to
;; https://github.com/zotero/zotero/blob/master/resource/schema/userdata.sql

;; to enable annotations integration
;; if this throws, you must run `pdf-tools-install` to success
;; (pdf-info-check-epdfinfo)
;; the error would be
;; Debugger entered--Lisp error: (error "pdf-info-epdfinfo-program is not executable")
;; to check manually, make sure (likely candidate) exists:
;; $HOME/.emacs.d/elpa/pdf-tools-*.*/epdfinfo
;; and it is executable

(require 'dash)
(require 'esqlite)
(require 'hydra)
(require 's)  ;; https://github.com/magnars/s.el
(require 'f)  ;; https://github.com/rejeep/f.el
(require 'sql)
(load-file (concat (file-name-directory (or load-file-name (buffer-file-name)))
                   "external/yesql/yesql.el"))
(require 'yesql)
(unless (require 'ini nil t)
  ;; (use-package ini :quelpa ((ini :fetcher github :repo "daniel-ness/ini.el")))
  (load (concat (file-name-directory (or load-file-name (buffer-file-name)))
                "external/ini.el/ini")))


(defmacro comment (&rest body) nil)

(defconst zotero-database-filename "zotero.sqlite")

(defun zotero--get-existing-directory-path (candidate-path)
  "returns the directory path if it exists, else returns nil"
  (when (file-exists-p candidate-path)
    (file-name-as-directory candidate-path)))

(defun zotero--get-data-dir-from-prefs-js (prefs-js-file-path)
  (with-temp-buffer
    (insert-file-contents
     (concat
      (file-name-as-directory candidate-dir)
      (file-name-as-directory
       (cdr (assoc "Path"
                   (cdr (assoc "Profile0" zotero-ini)))))
      "prefs.js"))
    (let ((matcher "user_pref(\"extensions.zotero.dataDir\", \"\\(.+\\)\");"))
      (dolist (line
               (split-string (buffer-string) "\n")
               zotero-data-dir)
        (when (string-match matcher line)
          (setq zotero-data-dir (match-string 1 line)))))))

(defun zotero--find-library-filepath ()
  (catch 'found-path
    (dolist
        (candidate-evaluator
         '((lambda ()
             (let* ((candidate-dir (expand-file-name "~/.zotero/zotero"))
                    (candidate-ini (concat
                                    (file-name-as-directory
                                     candidate-dir)
                                    "profiles.ini")))
               (when (file-exists-p candidate-ini)
                 (let ((zotero-ini (ini-decode
                                    (with-temp-buffer
                                      (insert-file-contents candidate-ini)
                                      (buffer-string)))))
                   (zotero--get-data-dir-from-prefs-js
                    (concat
                     (file-name-as-directory candidate-dir)
                     (file-name-as-directory
                      (cdr (assoc "Path"
                                  (cdr (assoc "Profile0" zotero-ini)))))
                     "prefs.js"))))))
           (lambda ()
             (let ((user-profile-path
                    (or
                     (getenv "UserProfile") ;; windows
                     (getenv "HOME")        ;; posix
                     )))
               (zotero--get-existing-directory-path
                (concat (file-name-as-directory user-profile-path) "Zotero"))))))
      (setq candidate-path (funcall candidate-evaluator))
      (when candidate-path
        (throw 'found-path candidate-path)))))

(defvar zotero-db
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

(defun zotero--exec-sqlite-query-to-plist-items
    (sql-query prop-names)
  (mapcar (lambda (record) (-interleave prop-names record))
          (esqlite-read
           (format
            "file:///%s?immutable=1"  ;; this allow RO access to a locked database  "file:///%s?mode=ro"
            (replace-regexp-in-string "^/" "" zotero-db))
           sql-query)))

(defun zotero--aggregate-items-by-key
    (item-property-row-list)
  (comment
   (zotero--aggregate-items-by-key
    '((:key "abc" :title "some title")
      (:key "abc" :date "2018-02-03")
      (:key "xyz" :title "other title")))))

(setq zotero--yesql-mappings
      (parse-yesql (f-read-text
                    (f-join
                     (file-name-directory (or load-file-name (buffer-file-name)))
                     "resources" "sql" "default.sql")
                    'utf-8)))

(setq zotero--sql-tags-join-string (gethash "tagsJoinString" zotero--yesql-mappings))

(setq zotero--sql-creators-join-string (gethash "creatorsJoinString" zotero--yesql-mappings))

(defun get-plist-keys (input-plist)
  (let ((out (list)))
    (loop for (k v)
          on input-plist by (function cddr)
          do
          (setq out (cons k out)))
    out))

(setq zotero--special-properties-tables-query-plist
      ;; table-name --> join query on itemID
      (list
       'example (zotero--concat-sql-statements
                 "SELECT itemTags.itemID"
                 ", CASE WHEN tags.name LIKE 'example-%' THEN"
                 "'CUSTOM-EXAMPLE'"
                 "ELSE"
                 "  ' '"
                 "END AS example"
                 "FROM tags"
                 "JOIN itemTags ON itemTags.tagID = tags.tagID"
                 "WHERE tags.name IN ('example-1', 'example-2')"
                 "GROUP BY itemTags.itemID")
       ))

(defun zotero-get-special-properties-tables-selects
    (mapping-plist)
  (let ((out ""))
    (loop for (table-name _query)
          on mapping-plist by (function cddr)
          do
          (setq out (concat out (format ", %s" table-name))))
    out))

(defun zotero-get-special-properties-joins
    (mapping-plist)
  (let ((out ""))
    (loop for (table-name query)
          on mapping-plist by (function cddr)
          do
          (setq out
                (concat out
                        (format " JOIN (%s) AS %sQ on %sQ.itemID = items.itemID"
                                query table-name table-name))))
    out))

(setq zotero--id-date-key-title-select
      (zotero--concat-sql-statements
       (gethash "baseSelectFragment" zotero--yesql-mappings)))

(setq zotero--id-date-key-title-select-fields
      '(itemID key dateAdded dateModified title tags creators attachmentKey attachmentPath))

(setq zotero--base-query-item-select-fields
      (append
       zotero--id-date-key-title-select-fields
       (get-plist-keys zotero--special-properties-tables-query-plist)))

(setq zotero--base-query-item-select-from
      (gethash "baseQueryItemSelectFrom" zotero--yesql-mappings))

(defun zotero-query-by-item-id
    (item-id)
  (let ((sql-query
         (zotero--concat-sql-statements
          zotero--base-query-item-select-from
          (format "AND items.itemID = %s" item-id))))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query zotero--base-query-item-select-fields)))

(defun zotero-escape-elisp-percent (s)
  (replace-regexp-in-string
   "%" "%%"
   s))

(defun zotero--yesql-substring-tokenify (query-substring)
  (concat "'%" query-substring "%'"))

(defun zotero-query-by-tags-and-creators-and-filenames
    (query-string)
  ;; looks for matching substring in
  ;; - author *last name*s
  ;; - tags
  ;; - filename of the attachment
  ;; returns a list of property-list items like
  ;; (list (itemID 5 key "abcd" ...)
  ;;       (itemID 29 key "zxcv" ...) ...)
  (let* ((query-string-format-pattern
          (zotero-escape-elisp-percent
           (zotero--yesql-substring-tokenify query-string)))
         (sql-query
          (format
           (zotero--concat-sql-statements
            (zotero-escape-elisp-percent
             zotero--base-query-item-select-from)
            (render-yesql
             (gethash "tagsAndCreatorsAndFilenames" zotero--yesql-mappings)
             query-string-format-pattern
             query-string-format-pattern
             query-string-format-pattern)))))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query zotero--base-query-item-select-fields)))

(defun zotero-query-by-attributes
    (query-string &optional field-name)
  ;; looks for matching substring by any of zotero's itemDataValues.value
  ;; returns a property list of (item-key -> item-plist) mappings, like
  ;; (1 (itemID 1 key ABCD
  ;;     doi doi:1234.567 ...)
  ;;  9 (itemID 9 key ZXCV
  ;;     title "tied toll" ...) ...)
  (let* ((field-name-query
          (cond ;; support for specific fields
           ((null field-name) "'NULL'")
           (t (format "'%s'" field-name))))
         (sql-query
          (render-yesql
           (gethash "queryByAttributes" zotero--yesql-mappings)
           field-name-query
           field-name-query
           (zotero--yesql-substring-tokenify
            query-string))))
    (let ((item-property-row-list
           (zotero--exec-sqlite-query-to-plist-items
            sql-query
            (append
             zotero--id-date-key-title-select-fields
             '(fieldName value))))
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
                  (apply
                   'append
                   item-data
                   (mapcar (lambda (k)
                             (list k (plist-get item-property-row k)))
                           zotero--base-query-item-select-fields))
                  (intern (plist-get item-property-row 'fieldName))
                  (plist-get item-property-row 'value)))))))))

(defun zotero-query-by-fulltext
    (search-word)
  (let ((sql-query
         (format
          (render-yesql (gethash "queryByFulltextFragment" zotero--yesql-mappings) "'%s'")
          search-word)))
    (zotero--exec-sqlite-query-to-plist-items
     sql-query
     (append zotero--base-query-item-select-fields '(word)))))

(defun zotero-query-any
    (query-string)
  (let* ((tags-and-creators-and-filenames-matches
          (zotero-query-by-tags-and-creators-and-filenames query-string))
         (full-text-matches
          (zotero-query-by-fulltext
           query-string))
         (attributes-matches
          (zotero-query-by-attributes query-string)))
    (dolist (item-property-row
             (append tags-and-creators-and-filenames-matches
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

(defun zotero--get-attachment-key-path-filepath
    (item-key item-path)
  (concat
   (file-name-as-directory
    (concat
     (file-name-as-directory
      (zotero--find-library-filepath))
     "storage/"
     item-key))
   (replace-regexp-in-string
    "^storage:" ""
    item-path)))

(defun zotero-get-attachment-plist-filepath
    (attachment-plist)
  (let ((zotero-internal-path (plist-get attachment-plist 'path)))
    (if (not (string-match-p "^storage:" zotero-internal-path))
        (message "ERROR: don't know how to handle this path: %s"
                 zotero-internal-path)
      (zotero--get-attachment-key-path-filepath
       (plist-get attachment-plist 'key)
       zotero-internal-path))))

(defun format-org-zotero-link (zotero-key)
  (format "[[zotero:%s]]" zotero-key))

(org-add-link-type "zotero" 'org-zotero-link-open 'org-zotero-link-export)

(defun zotero-link-url ()
  "returns the textual component of the zotero:-type link enclosed in org-style double brackets"
  (interactive)
  (let ((org-context (org-element-context)))
    (plist-get (cadr org-context) :path)))

;; ref markdown-mode.el::defun markdown-link-p
(defun zotero-link-p ()
  "Return non-nil when `point' is not a zotero link"
  (interactive)
  (let ((org-context (org-element-context)))
    (and (eq 'link (first org-context))
         (string= "zotero"
                  (plist-get
                   (cadr org-context)
                   :type)))))

(defun org-zotero-link-open (link-text)
  "Open zotero styled link."
  ;; (zotero-query "title or author or fulltext string... but probably title/author")  ;;  see zotero-query-any()
  (let* ((spl (split-string link-text "\\(::\\|#\\)"))  ;; link-path::org-style-locator, link-path#url-style-locator
         (zlink (car spl)))
    (when (cadr spl)
      (let* ((pg-spl (split-string (cadr spl) "++"))
             (pnum (string-to-number (car pg-spl))))
        (setq zotero-query-buf
              (plist-put zotero-query-buf :page pnum))))
    (zotero-query zlink)))

(defun org-zotero-link-export (link description format)
  "FIXME: stub function"
  (concat "link in zotero: " link " (" description ")"))

;; TODO de-uglify
(defvar zotero-query-buf nil)
(defvar zotero-result-buf nil)
(defvar zotero-output-buf nil)
(defvar zotero-result-header-text nil)

(defun swap-zotero-result-buf (zotero-item)
  (setq zotero-result-buf zotero-item)
  (setq zotero-result-header-text
        (let ((filename (file-name-nondirectory
                         (plist-get zotero-result-buf 'filepath)))
              (num-annotations
               (zotero-query--get-total-annotations
                (plist-get zotero-result-buf 'filepath))))
          (format "[%d annotations]: %s" num-annotations filename))))

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

(defhydra zotero-result-menu (:hint nil :exit t)
  "
%s`zotero-result-header-text

^Insert^                       ^Open^
org-pdfview _l_ink at point    _o_ open with emacs
_z_otero link at point         _O_ open externally
_i_d                           _A_ annotation browser
_k_ey
_a_uthors
_p_ublication
_t_itle
ta_g_s
_f_ile path
_q_uit
"

  ("o" (lambda ()
         (interactive)
         (let* ((fpath (plist-get zotero-result-buf 'filepath))
                (pnum (plist-get zotero-query-buf 'page)))
           (org-open-file fpath t)
           (when pnum
             (pdf-view-goto-page pnum)
             (setq zotero-query-buf nil)))))

  ("O" (lambda ()
         (interactive)
         (org-open-file (plist-get zotero-result-buf 'filepath))))

  ("A" (lambda ()
         (interactive)
         (zotero-query--show-annotations-selector
          (plist-get zotero-result-buf 'filepath))))

  ("l" (lambda ()
         (interactive)
         (save-excursion
           (insert (format "[[pdfview:%s][%s]]"
                           (plist-get zotero-result-buf 'filepath)
                           (plist-get zotero-result-buf 'title))))))

  ("z" (lambda ()
         (interactive)
         (save-excursion
           (insert
            (format-org-zotero-link
             (if (plist-get zotero-result-buf 'doi)
                 (concat "doi:" (plist-get zotero-result-buf 'doi))
               (plist-get zotero-result-buf 'title)))))))

  ("i" (lambda ()
         (interactive)
         (set-zotero-active-result 'id)
         (zotero-insert-menu/body)))

  ("k" (lambda ()
         (interactive)
         (set-zotero-active-result 'key)
         (zotero-insert-menu/body)))

  ("a" (lambda ()
         (interactive)
         (set-zotero-active-result 'creators)
         (zotero-insert-menu/body)))

  ("p" (lambda ()
         (interactive)
         (set-zotero-active-result 'publicationTitle)
         (zotero-insert-menu/body)))

  ("t" (lambda ()
         (interactive)
         (set-zotero-active-result 'title)
         (zotero-insert-menu/body)))

  ("g" (lambda ()
         (interactive)
         (set-zotero-active-result 'tags)
         (zotero-insert-menu/body)))

  ("f" (lambda ()
         (interactive)
         (set-zotero-active-result 'filepath)
         (zotero-insert-menu/body)))

  ("q" nil))

(defun zotero--get-attachment-extension (zotero-entry)
  (file-name-extension (plist-get zotero-entry 'attachmentPath)))

(defun zotero--choose-result (item-list)
  (let* ((counter 0)
         (nres (length item-list)))
    (cond ((= 1 nres)
           (swap-zotero-result-buf (elt item-list 0))
           (zotero-result-menu/body))
          ((< 0 nres)
           (helm
            :sources `((name . ,(format "query choice from %s matches: " nres))
                       (candidates . ,(mapcar #'(lambda (item)
                                                  (incf counter)
                                                  (let ((extension (zotero--get-attachment-extension item))
                                                        (creators (plist-get item 'creators)))
                                                    (cons (format "%2s (%5s) %4s %s (%s)"
                                                                  (propertize (number-to-string counter)
                                                                              'face '(:foreground "blue"))
                                                                  (propertize (plist-get item 'itemID)
                                                                              'face '(:foreground "SkyBlue"))
                                                                  (propertize extension 'face '(:foreground "red"))
                                                                  (plist-get item 'title)
                                                                  (if (not (eq creators :null))
                                                                      (propertize creators 'face '(:foreground "orange"))
                                                                    (propertize "N/A" 'face '(:foreground "DarkGray"))))
                                                          item)))
                                              item-list))
                       (action . (lambda (selection)
                                   (swap-zotero-result-buf selection)
                                   (zotero-result-menu/body)))))))))

(defun zotero--combined-query-result-to-choice-list
    (query-result)
  (let (out)
    (loop for (item-id item-data)
          on query-result
          by (function cddr)
          do
          ;; filter out items that don't have a title or an attachment
          (when (and (plist-get item-data 'title)
                     (plist-get item-data 'attachmentKey)
                     (plist-get item-data 'attachmentPath)
                     (not (eq (plist-get item-data 'attachmentPath)
                              :null)))
            (setq out
                  (cons
                   (plist-put
                    (plist-put item-data 'id item-id)
                    'filepath
                    (zotero--get-attachment-key-path-filepath
                     (plist-get item-data 'attachmentKey)
                     (plist-get item-data 'attachmentPath)))
                   out))))
    out))

;; interesting property keys after zotero--combined-query-result-to-choice-list
;; id
;; itemID
;; title
;; attachmentKey
;; attachmentPath
;; url
;; key
;; dateAdded
;; dateModified
;; title
;; tags
;; creators
;; attachmentKey
;; attachmentPath
;; filepath
(defun zotero-query (&optional input-string)
  (interactive)
  (let* ((default-string (if mark-active
                             (replace-regexp-in-string
                              "[\s\n]+$" ""
                              (buffer-substring (mark) (point)))))
         (search-string (or input-string
                            (read-string
                             (format "search string%s: "
                                     (if default-string
                                         (concat " [" default-string "]")
                                       ""))
                             nil nil default-string)))
         (return-key :filepath))
    (let ((item-list (zotero--combined-query-result-to-choice-list
                      (zotero-query-any search-string))))
      (zotero--choose-result item-list))))

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

(defvar zotero--display-headers
  '[("dateModified" 12 t)
    ("itemID" 6 t)
    ("attachmentPath" 5 t)
    ("creators" 40 t)
    ("title" 0 t)])

(define-derived-mode zotero-query-quick-browser-mode
  tabulated-list-mode
  "zotero-query-quick-browser"
  "Quick list of entries from the Zotero database"

  (setq tabulated-list-format zotero--display-headers)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "dateModified" t))
  (tabulated-list-init-header))

(defun zotero-load-quick-browser ()
  (interactive)
  (pop-to-buffer "*zotero-query-quick-browser*" nil)
  (zotero-query-quick-browser-mode)
  (define-key zotero-query-quick-browser-mode-map (kbd "j") 'next-line)
  (define-key zotero-query-quick-browser-mode-map (kbd "k") 'previous-line)
  (define-key zotero-query-quick-browser-mode-map (kbd "RET")
    (lambda ()
      (interactive)

      (let* ((selected-entry (tabulated-list-get-entry))
             (attachment-path-index
              (catch 'index
                (let ((item-index 0))
                  (dolist (header-spec (append zotero--display-headers nil))
                    (if (string= (first header-spec) "attachmentPath")
                        (throw 'index item-index)
                      (setq item-index (1+ item-index)))))))
             (selected-entry-extension
              (aref selected-entry attachment-path-index))
             (maybe-best-match
              (first
               (-filter
                (lambda (rec)
                  (string= selected-entry-extension
                           (zotero--get-attachment-extension rec)))
                (zotero-query-by-item-id (tabulated-list-get-id))))))
        (when maybe-best-match
          (swap-zotero-result-buf
                (plist-put
                 maybe-best-match
                 'filepath
                 (zotero--get-attachment-key-path-filepath
                  (plist-get maybe-best-match 'attachmentKey)
                  (plist-get maybe-best-match 'attachmentPath))))
          (zotero-result-menu/body)))))

  (setq tabulated-list-entries
        (mapcar
         (lambda (record)
           (list (plist-get record 'itemID)
                 (vconcat
                  (mapcar (lambda (header-spec)
                            (let* ((key (intern (car header-spec)))
                                   (value (plist-get record key)))
                              (cond ((eq value :null)
                                     "")
                                    ((or (eq key 'dateAdded)
                                         (eq key 'dateModified))
                                     (substring value 0 10))
                                    ((eq key 'attachmentPath)
                                     (zotero--get-attachment-extension record))
                                    (t value))))
                          zotero--display-headers))))
         (zotero--combined-query-result-to-choice-list
          (zotero--exec-sqlite-query-to-plist-items
           (zotero--concat-sql-statements
            zotero--base-query-item-select-from
            "AND fields.fieldName = 'title'"
            "ORDER BY items.itemID DESC")
           zotero--base-query-item-select-fields))))
  (tabulated-list-print t))


(defun zotero-get-number-of-items
    ()
  ;; FIXME: items != entry that is selectable from the zotero browser
  "get the total number of items in the zotero database"
  (interactive)
  (let ((sql-result
         (first
          (zotero--exec-sqlite-query-to-plist-items
           "SELECT COUNT(*) FROM items"
           '(count)))))
    (string-to-number (plist-get sql-result 'count))))

(defun zotero-query--pdf-tools-extract-highlight-text (pdf-tools-annotation-item)
  ;; ref org-noter.el:org-noter-create-skeleton()
  ;; [[file:~/.emacs.d/elpa/org-noter-20191020.1212/org-noter.el::(defun org-noter-create-skeleton ()][org-noter.el:org-noter-create-skeleton]]
  (let* ((item pdf-tools-annotation-item)
         (type  (alist-get 'type item))
         (page  (alist-get 'page item))
         (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                    (alist-get 'edges item)))
         (top (nth 1 edges)))
    (if (eq type 'link)
        ""
      (pdf-info-gettext page edges))))

(defun zotero-query--get-pdf-annotations (pdf-file-path)
  (let ((cur-buffer (current-buffer))
        (annots (list)))
    (save-excursion
      (condition-case nil
          (dolist (item (progn
                          (find-file pdf-file-path)
                          (pdf-info-getannots)))
            (when (not (eq (alist-get 'type item) 'link))
              (add-to-list
               'annots
               (push (cons 'highlight-text
                           (zotero-query--pdf-tools-extract-highlight-text item))
                     item))))
        (error (list)))
      (switch-to-buffer cur-buffer)
      (sort
       annots
       (lambda (a b)
         (< (alist-get 'page a)
            (alist-get 'page b)))))))

(defun zotero-query--get-annotations-summary (pdf-file-path)
  (let ((annots (zotero-query--get-pdf-annotations pdf-file-path)))
    (list
     (cons 'total (length annots)))))

(defun zotero-query--get-total-annotations (pdf-file-path)
  (alist-get 'total (zotero-query--get-annotations-summary pdf-file-path)))

(defun zotero-query--show-annotations-selector (pdf-file-path)
  (helm
   :sources `((name . ,(file-name-nondirectory pdf-file-path))
              (candidates . ,(mapcar (lambda (pdf-tools-annotation-item)
                                       (let* ((highlight-text
                                               (zotero-escape-elisp-percent
                                                (s-collapse-whitespace
                                                 (alist-get 'highlight-text pdf-tools-annotation-item))))
                                              (maybe-note-text (alist-get 'contents pdf-tools-annotation-item))
                                              (note-text
                                               (if (< 0 (length maybe-note-text))
                                                   (format "%s / "
                                                           (propertize
                                                            (s-replace
                                                             "%" "%%"
                                                             (s-trim maybe-note-text))
                                                            'face '(:foreground "orange")))
                                                 "")))
                                         (cons
                                          (format
                                           "%s%s"
                                           note-text
                                           (propertize highlight-text
                                                       'face '(:foreground "DimGray")))
                                          pdf-tools-annotation-item)))
                                     (zotero-query--get-pdf-annotations pdf-file-path)))
              (action . (lambda (selection)
                          (find-file-other-window pdf-file-path)
                          (pdf-view-goto-page (alist-get 'page selection)))))))



(provide 'zotero-query)
