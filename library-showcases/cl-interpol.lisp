;; https://edicl.github.io/cl-interpol/

;; Load the cl-interpol library
(ql:quickload "cl-interpol")

;; Use the cl-interpol package
(use-package :cl-interpol)

;; Ensure named-readtables is available and used correctly
(ql:quickload "named-readtables")
(named-readtables:in-readtable :interpol-syntax)

;; Enable interpolation syntax  T  (in order to use #? for string interpolation)
(enable-interpol-syntax)

(let ((name "Alice")
      (age 30))
  ;; Use #? for string interpolation
  (format t #?"Hello, my name is ${name} and I am ${age} years old.~%"))


;; print hello world
(format t "Hello, world!~%")




(defmacro f-string (format-string &rest values)
  `(format nil "~A~{ ~A~}" ,format-'string (list ,@values)))

(defmacro my-print (format-string &rest values)
  `(format t "~A~%" (f-string ,format-string ,@values)))



(defvar a "me")
(defvar b 1)

(f-string "X A X" "B" "C" a b "d")

(my-print "X 1 X" "B" "C" a b "d")



;; Usage example


; (defvar a "me")


; (format t (f-string "this is " a " and this is " a a a "~%"))


; (print "a")


; (princ (f-string "asdf" " asd"))
