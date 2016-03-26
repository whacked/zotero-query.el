(require 'cl)
(require 'hydra)

;; zotero query
(require 'json)

;; tells us where to look for qnotero_util.py
(defconst zotero-query-dir
  (if load-file-name
      (file-name-directory load-file-name)
    (file-name-directory (buffer-file-name))))

;; TODO change to defvar
(defvar zotero-result-buf)
(defvar zotero-output-buf)

(defhydra zotero-insert-menu (:color pink)
  "what to do with string?"
  ("i" (lambda ()
         (interactive)
         (insert (format "%s" zotero-output-buf)))
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

(defun zotero-choose-result (item-list)
  (let ((counter 0))
    (when (< 0 (length item-list))
      (let ((selection (string-to-int
                        (char-to-string
                         ;; simple simple menu
                         (read-char
                          (concat (format "%s results, what do?\n"
                                          (length item-list))
                                  (mapconcat #'(lambda (item)
                                                 (incf counter)
                                                 (format "[%d]  %s %s" counter (plist-get item :title) (plist-get item :authors)))
                                             item-list "\n")
                                  ))))))
        (if (and (< 0 selection)
                 (<= selection (length item-list)))
            (progn
              (setq zotero-result-buffer (elt item-list (1- selection)))
              (zotero-result-menu/body))
          (message "invalid selection"))))))

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

