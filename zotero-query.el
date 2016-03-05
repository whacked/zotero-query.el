(require 'cl)

;; zotero query
(require 'json)

(defconst zotero-query-dir
  (if load-file-name
      (file-name-directory load-file-name)
    (file-name-directory (buffer-file-name))))

;; json return structure contains keys:
;; :id
;; :key
;; :authors
;; :publication
;; :title
;; :tags
;; :simple_format   ;;; basic citation
;; :fulltext        ;;; this is actually pdf filepath
(defun zotero-query ()
  (interactive)
  (let* ((default-string (if mark-active (calibre-chomp (buffer-substring (mark) (point)))))
         (search-string (read-string
                         (format "search string%s: "
                                 (if default-string
                                     (concat " [" default-string "]")
                                   "")) nil nil default-string))
         (return-key :simple_format))
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
      (insert
       (plist-get
        (elt item-list 0)
        return-key)))))
