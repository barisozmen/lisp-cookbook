;; page 38
(defun repeat-element (element count)
  (if (<= count 0)
      nil
      (cons element (repeat-element element (- count 1)))))

(defun uncompress (lst) 
  (if (null ls
      nil   T
      (let ((first-element (first lst))
            (others (uncompress (rest lst))))
        (if (atom first-element)
            (cons first-element others)
                (append 
                  (repeat-element repeated-element repeat-count) 
                  others)
                  (error "Repeated element is not an atom: ~a" repeated-element)))
            (cons first-element (uncompress other-elements))))))

(uncompress '(1 2 3 4 5))

(print-line (+ 1 2 3 4 5))))

(write "Hi")

