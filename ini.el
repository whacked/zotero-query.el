"Emacs .ini file parser. Converts between association lists and .ini files"

(defun ini-decode (ini_text) 
  ;; text -> alist
  (interactive)
  (if (not (stringp ini_text))
      (error "Must be a string"))
  (let ((lines (split-string ini_text "\n"))
	(section)
	(section-list)
	(alist))
    (dolist (l lines)
      ;; skip comments
      (unless (or (string-match "^;" l)
		  (string-match "^[ \t]$" l))
	;; catch sections
	(if (string-match "^\\[\\(.*\\)\\]$" l)
	    (progn 
	      (if section
		  ;; add as sub-list
		  (setq alist (cons `(,section . ,section-list) alist))
		(setq alist section-list))
	      (setq section (match-string 1 l))
	      (setq section-list nil)))
	      ;; catch properties
	      (if (string-match "^\\(.+\\)=\\(.+\\)$" l)
		  (let ((property (match-string 1 l))
			(value (match-string 2 l)))
		    (progn 
		      (setq section-list (cons `(,property . ,value) section-list)))))))
    (if section
	;; add as sub-list
	(setq alist (cons `(,section . ,section-list) alist))
      (setq alist section-list))
    alist))


(defun ini-encode (ini_alist)
  ;; alist -> text
  (interactive)
  (if (not (listp ini_alist))
      (error "ini_alist is not a list"))
  (let ((txt ""))
    (dolist (element ini_alist)
      (let ((key (car element))
	    (value (cdr element)))
	(when (not (stringp key))
	  (error "key is not a string"))
	(if (listp value)
	    (setq txt 
		  (concat txt 
			  (format "[%s]\n" key)
			  (ini-encode value)))
	  (setq txt 
		(concat txt (format "%s=%s\n" key value))))))
    txt))


