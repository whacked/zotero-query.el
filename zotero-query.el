;; TODO: derive profile directory
;; (defvar zotero-root-dir (expand-file-name "~/Library/Application Support/Zotero/Profiles/{PID}.default/zotero"))
(defvar zotero-db (concat zotero-root-dir "/zotero.sqlite"))
(defvar zotero-storage-dir (concat (file-name-as-directory zotero-root-dir) "storage"))
(setq zotero-db "/Volumes/ramdisk/zotero-query.el/zotero.sqlite")

(defun sqlite3-chomp (s)
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defun sqlite3-query (sql-query)
  (interactive)
  (shell-command-to-string
   (format "%s -separator '\t' '%s' '%s'" sql-sqlite-program zotero-db sql-query)))

(let* ((argstring "mechanism")
       (attachment-typeID (sqlite3-chomp (sqlite3-query "SELECT itemTypeID FROM itemTypes WHERE typeName = '\\''attachment'\\'' LIMIT 1")))
       (query-result (sqlite3-query 
                      (concat "SELECT "
                              "  it.itemID"
                              ", it.key"
                              ", itdv.value"
                              " FROM "
                              "  items           AS it"
                              ", itemData        AS itd"
                              ", itemDataValues  AS itdv"
                              ", fields          AS fld"
                              " WHERE "
                              ;; do not match attachment type
                              "     it.itemTypeID <> " (format "%s" attachment-typeID)
                              " AND it.itemID = itd.itemID"
                              " AND itdv.valueID = itd.valueID"
                              " AND itd.fieldID = fld.fieldID"
                              " AND itdv.value LIKE '\\''%%"
                              (format "%s" (downcase argstring))
                              "%%'\\''"
                              " AND fld.fieldName = '\\''title'\\''"
                              " ORDER BY "
                              "    it.itemID DESC"
                              " LIMIT 1"
                              ))))
  (if (= 0 (length query-result))
      (message "nothing found.")
    (let ((res (zotero-query-to-alist query-result)))
      ;; (let ((opr (char-to-string (read-char
      ;;                             ;; render menu text here
      ;;                             (concat "[" (getattr res :title) "] found ... what do?\n"
      ;;                                     (mapconcat #'(lambda (handler-list)
      ;;                                                    (let ((hotkey      (elt handler-list 0))
      ;;                                                          (description (elt handler-list 1))
      ;;                                                          (handler-fn  (elt handler-list 2)))
      ;;                                                      ;; ULGY BANDAIT HACK
      ;;                                                      ;; replace "insert" with "copy to clipboard" if mark-active
      ;;                                                      (format " %s :   %s"
      ;;                                                              hotkey
      ;;                                                              (if mark-active
      ;;                                                                  (replace-regexp-in-string "insert \\(.*\\)" "copy \\1 to clipboard" description)
      ;;                                                                description)))
      ;;                                                    ) zotero-handler-alist "\n")
      ;;                                     )))))
      ;;   (funcall
      ;;    (elt (if (null (assoc opr zotero-handler-alist)) (assoc "q" zotero-handler-alist)
      ;;           (assoc opr zotero-handler-alist)) 2) res))

      ;; look for files
      (let ((match-atta-list (mapcar
                              (lambda (line)
                                (sqlite3-destructure-line-to-alist '(:key :mimeType :path) line))
                              (split-string
                               (sqlite3-chomp
                                (sqlite3-query
                                 (concat
                                  " SELECT"
                                  "   itm.key"
                                  ",  atta.mimeType"
                                  ",  atta.path"
                                  " FROM"
                                  "  items AS itm "
                                  ", itemAttachments AS atta "
                                  " WHERE "
                                  "      atta.sourceItemID = " (format "%s" (getattr res :itemID))
                                  " AND itm.itemID = atta.itemID"
                                  )))
                               "\n"))))
        (if (< 0 (length match-atta-list))
            (progn

              (let* ((first-item (nth 0 match-atta-list))
                     (item-path
                      (concat
                       (file-name-as-directory
                        (concat (file-name-as-directory zotero-storage-dir)
                                (getattr first-item :key)))
                       ;; strip the "storage:" prefix
                       (substring (getattr first-item :path) 8))))

                (if (file-exists-p item-path)
                    (progn
                      item-path
                      (start-process "shell-process" "*Messages*" zotero-default-opener item-path)
                      ))
                )
              )
            )
        )
      )
    )
  )



(defvar zotero-default-opener
  (cond ((eq system-type 'gnu/linux)
         ;; HACK!
         ;; "xdg-open"
         ;; ... but xdg-open doesn't seem work as expected! (process finishes but program doesn't launch)
         ;; appears to be related to http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg00279.html
         ;; you're better off replacing it with your exact program...
         ;; here we run xdg-mime to figure it out for *pdf* only. So this is not general!
         (zotero-chomp
          (shell-command-to-string
           (concat
            "grep Exec "
            (first
             ;; attempt for more linux compat, ref
             ;; http://askubuntu.com/questions/159369/script-to-find-executable-based-on-extension-of-a-file
             ;; here we try to find the location of the mimetype opener that xdg-mime refers to.
             ;; it works for okular (Exec=okular %U %i -caption "%c"). NO IDEA if it works for others!
             (delq nil (let ((mime-appname (zotero-chomp (replace-regexp-in-string
                                                           "kde4-" "kde4/"
                                                           (shell-command-to-string "xdg-mime query default application/pdf")))))

                         (mapcar
                          #'(lambda (dir) (let ((outdir (concat dir "/" mime-appname))) (if (file-exists-p outdir) outdir)))
                          '("~/.local/share/applications" "/usr/local/share/applications" "/usr/share/applications")))))
            "|awk '{print $1}'|cut -d '=' -f 2"))))
        ((eq system-type 'windows-nt)
         ;; based on
         ;; http://stackoverflow.com/questions/501290/windows-equivalent-of-the-mac-os-x-open-command
         ;; but no idea if it actuall works
         "start")
        ((eq system-type 'darwin)
         "open")
        (t (message "unknown system!?"))))

(defun number-sequence-0 (excluded-end)
  (number-sequence 0 (1- excluded-end)))

(defun sqlite3-destructure-line-to-alist (field-keyword-list query-result-line)
  (let ((spl-query-result (split-string (zotero-chomp query-result-line) "\t")))
    (mapcar* 'list field-keyword-list
             (mapcar
              (lambda (idx)
                (nth idx spl-query-result))
              (number-sequence-0 (length field-keyword-list))))))

(defun zotero-query-to-alist (query-result)
  "builds alist out of a full zotero-query query record result"
  (if query-result
      (let ((spl-query-result (split-string (zotero-chomp query-result) "\t")))
        `((:itemID                 ,(nth 0 spl-query-result))
          (:key                    ,(nth 1 spl-query-result))
          (:title                  ,(nth 2 spl-query-result))))))

(defun zotero-build-default-query (whereclause &optional limit)
  (concat "SELECT "
          "b.id, b.author_sort, b.path, d.name, d.format, b.pubdate, b.title"
          " FROM data AS d "
          "LEFT OUTER JOIN books AS b ON d.book = b.id "
          whereclause
          (when limit
            (format "LIMIT %s" limit))
          ))

(defun zotero-query-by-field (wherefield argstring)
  (concat "WHERE lower(" wherefield ") LIKE '\\''%%"
          (format "%s" (downcase argstring))
          "%%'\\''"
          ))

(defun zotero-read-query-filter-command ()
  (interactive)
  (let* ((default-string (if mark-active (zotero-chomp (buffer-substring (mark) (point)))))
         ;; prompt &optional initial keymap read history default
         (search-string (read-string (format "search string%s: "
                                             (if default-string
                                                 (concat " [" default-string "]")
                                               "")) nil nil default-string))
         (spl-arg (split-string search-string ":")))
    (if (and (< 1 (length spl-arg))
             (= 1 (length (first spl-arg))))
        (let* ((command (downcase (first spl-arg)))
               (argstring (second spl-arg))
               (wherefield
                (cond ((string= "a" (substring command 0 1))
                       "b.author_sort")
                      ((string= "t" (substring command 0 1))
                       "b.title")
                      )))
          (zotero-query-by-field wherefield argstring))
      (format "WHERE lower(b.author_sort) LIKE '\\''%%%s%%'\\'' OR lower(b.title) LIKE '\\''%%%s%%'\\''"
              (downcase search-string) (downcase search-string)))))

(defun quote-% (str)
  (replace-regexp-in-string "%" "%%" str))

(defun zotero-list ()
  (interactive)
  (message (quote-% (zotero-query
            (concat "SELECT b.path FROM books AS b "
                    (zotero-read-query-filter-command))))))

(defun zotero-get-cached-pdf-text (pdf-filepath)
  (let ((found-text (shell-command-to-string
                     (format "%s -separator '\t' '%s' 'SELECT content FROM pdftext WHERE filepath = '%s'" sql-sqlite-program zotero-text-cache-db pdf-filepath))))
    (if (< 0 (length found-text))
        found-text
      (let ((text-extract (shell-command-to-string
                           (format "pdftotext '%s' -" pdf-filepath))))
        (message "supposed to insert this!")
        ))))

;; FIXME the <= part looks wrong. should be next year's 01-01
(defun zotero-open-citekey ()
  (interactive)
  (if (word-at-point)
      (let ((where-string
             (replace-regexp-in-string
              ;; capture all up to optional "etal" into group \1
              ;; capture 4 digits of date          into group \2
              ;; capture first word in title       into group \3
              "\\b\\(.+?\\)\\(?:etal\\)?\\([[:digit:]]\\\{4\\\}\\)\\(.*\\)\\b"
              "WHERE lower(b.author_sort) LIKE '\\\\''%\\1%'\\\\'' AND lower(b.title) LIKE '\\\\''\\3%'\\\\''AND b.pubdate >= '\\\\''\\2-01-01'\\\\'' AND b.pubdate <= '\\\\''\\2-12-31'\\\\'' LIMIT 1" (word-at-point))))
        ;; (mark-word)
        ;; (zotero-find (zotero-build-default-query where-string))
        (insert where-string)
        )
    (message "nothing at point!")))

 WHERE lower(b.author_sort) LIKE '\''%asdf%'\'' AND lower(b.title) LIKE '\''zxcv%'\''AND b.pubdate >= '\''2009-01-01'\'' AND b.pubdate <= '\''2009-12-31'\'' LIMIT 1
select

(insert                      (concat "SELECT "
                               "  it.itemID"
                               ", it.key"
                               ", itdv.value"
                               " FROM "
                               "  items           AS it"
                               ", itemData        AS itd"
                               ", itemDataValues  AS itdv"
                               ", fields          AS fld"
                               " WHERE "
                               ;; do not match attachment type
                               "     it.itemTypeID <> 14 " 
                               " AND it.itemID = itd.itemID"
                               " AND itdv.valueID = itd.valueID"
                               " AND itd.fieldID = fld.fieldID"
                               " AND itdv.value LIKE '\\''%%"
                               "mechanism"
                               "%%'\\''"
                               " AND fld.fieldName = '\\''title'\\''"
                               " ORDER BY "
                               "    it.itemID DESC"
                               " LIMIT 1"
                               ))
SELECT
  it.itemID
, it.key
, itdv.value FROM   items           AS it
, itemData        AS itd
, itemDataValues  AS itdv
, fields          AS fld
WHERE      it.itemTypeID <> 14 
 AND it.itemID = itd.itemID
 AND itdv.valueID = itd.valueID
 AND itd.fieldID = fld.fieldID
 AND itdv.value LIKE '\''%%mechanism%%'\''
 AND fld.fieldName = '\''title'\''
 AND itdv.value LIKE '%2014'
ORDER BY     it.itemID DESC LIMIT 1

SELECT fieldID FROM fields WHERE fieldName = 'title'; ;; 110
SELECT fieldID FROM fields WHERE fieldName = 'date'; ;; 14


SELECT
  it.itemID
, it.key
, itdv.value
FROM
  items           AS it
, itemData        AS itd
, itemDataValues  AS itdv
WHERE
    it.itemTypeID <> 14
AND it.itemID = itd.itemID
AND itd.fieldID = 14
AND itd.valueID = itdv.valueID
AND itdv.value LIKE '2012%'

(let* ((sql-query (concat

 SELECT
 it.itemID, it.key
 , itdv_titl.value
 , itdv_date.value
 , crtrd.lastName
 , itd_crtr.orderIndex
 FROM
 items           AS it
 , itemData        AS itd_titl, itemDataValues  AS itdv_titl
 , itemData        AS itd_date, itemDataValues  AS itdv_date
 , itemCreators    AS itd_crtr, creators AS crtr, creatorData AS crtrd
 WHERE
 it.itemTypeID <> 14
 AND it.itemID = itd_titl.itemID AND itd_titl.fieldID = 110
 AND itd_titl.valueID = itdv_titl.valueID
 AND it.itemID = itd_date.itemID AND itd_date.fieldID = 14
 AND itd_date.valueID = itdv_date.valueID
 AND it.itemID = itd_crtr.itemID AND itd_crtr.creatorID = crtr.creatorID
 AND crtr.creatorDataID = crtrd.creatorDataID
 AND itdv_titl.value LIKE '%cess%'
 AND itdv_date.value LIKE '2012%'
 -- AND LOWER(crtrd.lastName) LIKE 'balestra%'
 ORDER BY
 itd_crtr.orderIndex ASC
 LIMIT 1

                  )))

 )


(defun zotero-make-text-cache-path-from-citekey (citekey)
  (concat zotero-text-cache-dir "/" citekey "/text.org"))
(defun zotero-make-note-cache-path-from-citekey (citekey)
  (concat zotero-text-cache-dir "/" citekey "/note.org"))

(defun getattr (my-alist key)
  (cadr (assoc key my-alist)))

(defun zotero-make-citekey (zotero-res-alist)
  "return some kind of a unique citation key for BibTeX use"
  (let* ((spl (split-string (zotero-chomp (getattr zotero-res-alist :author-sort)) "&"))
         (first-author-lastname (first (split-string (first spl) ",")))
         (first-word-in-title (first (split-string (getattr zotero-res-alist :book-title) " "))))
    (concat
     (downcase (replace-regexp-in-string  "\\W" "" first-author-lastname))
     (if (< 1 (length spl)) "etal" "")
     (substring (getattr zotero-res-alist :book-pubdate) 0 4)
     (downcase (replace-regexp-in-string  "\\W.*" "" first-word-in-title)))))

(defun mark-aware-copy-insert (content)
  "copy to clipboard if mark active, else insert"
  (if mark-active
      (progn (kill-new content)
             (deactivate-mark))
    (insert content)))

;; define the result handlers here in the form of (hotkey description handler-function)
;; where handler-function takes 1 alist argument containing the result record
(setq zotero-handler-alist '(("o" "open"
                               (lambda (res) (find-file-other-window (getattr res :file-path))))
                              ("O" "open other frame"
                               (lambda (res) (find-file-other-frame (getattr res :file-path))))
                              ("v" "open with default viewer"
                               (lambda (res)
                                 (start-process "shell-process" "*Messages*" zotero-default-opener (getattr res :file-path))))
                              ("x" "open with xournal"
                               (lambda (res) (start-process "xournal-process" "*Messages*" "xournal"
                                                            (let ((xoj-file-path (concat zotero-root-dir "/" (getattr res :book-dir) "/" (getattr res :book-name) ".xoj")))
                                                              (if (file-exists-p xoj-file-path)
                                                                  xoj-file-path
                                                                (getattr res :file-path))))))
                              ("s" "insert calibre search string"
                               (lambda (res) (mark-aware-copy-insert (concat "title:\"" (getattr res :book-title) "\""))))
                              ("c" "insert citekey"
                               (lambda (res) (mark-aware-copy-insert (zotero-make-citekey res))))
                              ("i" "get book information (SELECT IN NEXT MENU) and insert"
                               (lambda (res)
                                 (let ((opr (char-to-string (read-char
                                                             ;; render menu text here
                                                             (concat "What information do you want?\n"
                                                                     "i : values in the book's `Ids` field (ISBN, DOI...)\n"
                                                                     "d : pubdate\n"
                                                                     "a : author list\n")))))
                                   (cond ((string= "i" opr)
                                          ;; stupidly just insert the plain text result
                                          (mark-aware-copy-insert
                                                   (zotero-chomp
                                                    (zotero-query (concat "SELECT "
                                                                           "idf.type, idf.val "
                                                                           "FROM identifiers AS idf "
                                                                           (format "WHERE book = %s" (getattr res :id)))))))
                                         ((string= "d" opr)
                                          (mark-aware-copy-insert
                                                   (substring (getattr res :book-pubdate) 0 10)))
                                         ((string= "a" opr)
                                          (mark-aware-copy-insert
                                                   (zotero-chomp (getattr res :author-sort))))
                                         (t
                                          (deactivate-mark)
                                          (message "cancelled"))))

                                 ))
                              ("p" "insert file path"
                               (lambda (res) (mark-aware-copy-insert (getattr res :file-path))))
                              ("t" "insert title"
                               (lambda (res) (mark-aware-copy-insert (getattr res :book-title))))
                              ("j" "insert entry json"
                               (lambda (res) (mark-aware-copy-insert (json-encode res))))
                              ("X" "open as plaintext in new buffer (via pdftotext)"
                               (lambda (res)
                                 (let* ((citekey (zotero-make-citekey res))
                                        (cached-text-path (zotero-make-text-cache-path-from-citekey citekey))
                                        (cached-note-path (zotero-make-note-cache-path-from-citekey citekey)))
                                   (if (file-exists-p cached-text-path)
                                       (progn
                                         (find-file-other-window cached-text-path)
                                         (when (file-exists-p cached-note-path)
                                           (split-window-horizontally)
                                           (find-file-other-window cached-note-path)
                                           (org-open-link-from-string "[[note]]")
                                           (forward-line 2)))
                                     (let* ((pdftotext-out-buffer (get-buffer-create (format "pdftotext-extract-%s" (getattr res :id)))))
                                       (set-buffer pdftotext-out-buffer)
                                       (insert (shell-command-to-string (concat "pdftotext '" (getattr res :file-path) "' -")))
                                       (switch-to-buffer-other-window pdftotext-out-buffer)
                                       (beginning-of-buffer))))))
                              ("q" "(or anything else) to cancel"
                               (lambda (res)
                                 (deactivate-mark)
                                 (message "cancelled")))))

(defun zotero-find (&optional custom-query)
  (interactive)
  (let* ((sql-query (if custom-query
                        custom-query
                      (zotero-build-default-query (zotero-read-query-filter-command) 1)))
         (query-result (zotero-query sql-query)))
    (if (= 0 (length query-result))
        (message "nothing found.")
      (let ((res (zotero-query-to-alist query-result)))
        (if (file-exists-p (getattr res :file-path))
            (let ((opr (char-to-string (read-char
                                        ;; render menu text here
                                        (concat "[" (getattr res :book-name) "] found ... what do?\n"
                                                (mapconcat #'(lambda (handler-list)
                                                               (let ((hotkey      (elt handler-list 0))
                                                                     (description (elt handler-list 1))
                                                                     (handler-fn  (elt handler-list 2)))
                                                                 ;; ULGY BANDAIT HACK
                                                                 ;; replace "insert" with "copy to clipboard" if mark-active
                                                                 (format " %s :   %s"
                                                                         hotkey
                                                                         (if mark-active
                                                                             (replace-regexp-in-string "insert \\(.*\\)" "copy \\1 to clipboard" description)
                                                                           description)))
                                                               ) zotero-handler-alist "\n"))))))
              (funcall
               (elt (if (null (assoc opr zotero-handler-alist)) (assoc "q" zotero-handler-alist)
                      (assoc opr zotero-handler-alist)) 2) res))
          (message "didn't find that file"))))))

(global-set-key "\C-cK" 'zotero-open-citekey)

(provide 'zotero-mode)
