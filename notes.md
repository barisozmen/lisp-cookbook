
## Notes: [A Road to Common Lisp](https://stevelosh.com/blog/2018/08/)

- "My advice is this: as you learn Common Lisp and look for libraries, try to suppress the voice in the back of your head that says "This project was last updated six years ago? That's probably abandoned and broken." The stability of Common Lisp means that sometimes libraries can just be done, not abandoned, so don't dismiss them out of hand."

- String interpolation library in Lisp > https://edicl.github.io/cl-interpol/

"In most languages the development process looks something like this:

Edit some code in the project with an editor.
Compile the project (some languages skip this step).
Run the project (or the tests).
Observe the output (in the console, a browser, etc).
Go to 1.
This is not how most Common Lisp users interact with the language. In Common Lisp, the development cycle looks more like this:

Start a Lisp process.
Load the project.
Edit some code with your editor.
Tell the running process to compile only the code you edited.
Interact with the changed code in the process via the REPL, an HTTP request, etc.
Observe the output (in the console, a browser, etc).
Go to 3."



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


## Debugging
[video: common lisp debugging: essential tips and tricks](https://www.youtube.com/watch?v=HI1PHUDN5As&t=390s)
(trace <function name>)
(untrace <function name>)


## All Lisp in Lisp
"I mean the famous example is of course,
```
    (define (eval exp env)
      (cond ((number? exp) exp)
            ((string? exp) exp)
            ((symbol? exp) (lookup exp env)
            ((eq? (car exp) 'quote) (cadr exp))
            ((eq? (car exp) 'lambda)
             (list 'closure (cdr exp) env))
            ((eq? (car exp) 'cond) (eval-cond (cdr exp))
            (else (apply 
                (eval (car exp) env)
                (eval-list (cdr exp) env)))))
```    
which uses a Lisp to define itself. This means roughly that if you understand enough Lisp to understand this program (and the little recursive offshoots like eval-cond), there is nothing else that you have to learn about Lisp. You officially have read the whole language reference and it is all down to libraries after that." from [hackernews discussion](https://news.ycombinator.com/item?id=33600941 )

## All Lisp in Python
```
import math
import operator as op

def standard_env() -> Env:
    "An environment with some Scheme standard procedures."
    env = Env()
    env.update(vars(math)) # sin, cos, sqrt, pi, ...
    env.update({
        '+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv, 
        '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq, 
        'abs':     abs,
        'append':  op.add,  
        'apply':   lambda proc, args: proc(*args),
        'begin':   lambda *x: x[-1],
        'car':     lambda x: x[0],
        'cdr':     lambda x: x[1:], 
        'cons':    lambda x,y: [x] + y,
        'eq?':     op.is_, 
        'expt':    pow,
        'equal?':  op.eq, 
        'length':  len, 
        'list':    lambda *x: List(x), 
        'list?':   lambda x: isinstance(x, List), 
        'map':     map,
        'max':     max,
        'min':     min,
        'not':     op.not_,
        'null?':   lambda x: x == [], 
        'number?': lambda x: isinstance(x, Number),  
		'print':   print,
        'procedure?': callable,
        'round':   round,
        'symbol?': lambda x: isinstance(x, Symbol),
    })
    return env

global_env = standard_env()
```
From: https://norvig.com/lispy.html
Also see: https://norvig.com/lispy2.html
