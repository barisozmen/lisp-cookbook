
Reading the blog post: [A Road to Common Lisp](https://stevelosh.com/blog/2018/08/)
- "My advice is this: as you learn Common Lisp and look for libraries, try to suppress the voice in the back of your head that says "This project was last updated six years ago? That's probably abandoned and broken." The stability of Common Lisp means that sometimes libraries can just be done, not abandoned, so don't dismiss them out of hand."
- String interpolation library in Lisp > https://edicl.github.io/cl-interpol/
- 




Screamer. Non-deterministic programming library
```
(require :screamer)

(defun solve-puzzle ()
  (let ((x (amb 1 2 3 4 5 6 7 8 9 10))
        (y (amb 1 2 3 4 5 6 7 8 9 10)))
    (assert (= (+ x y) 10))
    (assert (> x y))
    (list x y)))

(screamer:all-solutions #'solve-puzzle)
```

[cl-interpol](https://edicl.github.io/cl-interpol/) - String interpolation
Usage: by #?
```
#?"The result of 2 + 2 is ${(+ 2 2)}."
```

https://github.com/guicho271828/trivia Pattern matching library

https://github.com/stylewarning/cl-algebraic-data-type Algebraic data types library