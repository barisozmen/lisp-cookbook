(load "my-print.lisp")
(load "generate-webpage.lisp")

(write-line "Hello, World!")

(write "Hi")

(print "Hey")

(format t "Hi")



(write-line (concat-atoms #\n "hello" 42 3.14 'symbol #\a))
(write-line (concat-atoms "hello" 42 3.14 'symbol #\a " " 5))




(my-print "my-print casts various types into string and writes them like " 1 " and " 3)