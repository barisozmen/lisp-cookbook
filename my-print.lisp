(defun concat-atoms (&rest atoms)
  (with-output-to-string (s)
    (dolist (atom atoms)
      (format s "~A" atom))))

(defun my-print (&rest atoms)
  (write-line (apply #'concat-atoms atoms)))

; usage
(my-print "my-print casts various types into string and writes them like " 1 " and " 3.14 " and " #\a " " (+ 1 2) " even " (* (+ 3 5) 6))