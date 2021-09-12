;;; this file is a convenience library to improve cross language maintainability;
;;; currently it is NOT a true implemntation of yesql.
;;; it should be extracted into a separate library
(require 's)
(require 'mustache)  ;; provides {{...}} rendering


(defun parse-yesql
    (yesql-string)
  (let ((lookup (make-hash-table :test 'equal)))
    (dolist (sql-block (s-slice-at "^--\s+.+\n" yesql-string))
      (let* ((label-fragment
              (s-split-up-to "\n" sql-block 1))
             (sql-label (cadr (s-split "\s+" (car label-fragment))))
             (sql-fragment (s-trim (cadr label-fragment))))
        (puthash sql-label
                 (mustache-render sql-fragment lookup)
                 lookup)))
    lookup))

(defun render-yesql
    (yesql-template &rest parameters)
  (let* ((rendered-sql "")
         (parameter-positions (s-matched-positions-all "\\?" yesql-template))
         (num-parameters (length parameters))
         (previous-final-index (length yesql-template)))
    (when (not (= (length parameter-positions)
                  num-parameters))
      (error (format "SQL template has (%s) params but got (%s) inputs"
                     (length parameter-positions)
                     num-parameters)))
    ;; replace from end
    (dolist (i (number-sequence (1- num-parameters) 0 -1))
      (let* ((parameter (nth i parameters))
             (position (nth i parameter-positions))
             (start-index (car position))
             (end-index (cdr position)))
        (setq rendered-sql
              (format "%s%s%s"
                      parameter
                      (substring yesql-template end-index previous-final-index)
                      rendered-sql))
        (setq previous-final-index start-index)))
    (concat
     (substring yesql-template 0 previous-final-index)
     rendered-sql)))

(provide 'yesql)

