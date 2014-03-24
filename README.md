UnlambdaLisp
============

A toy Lisp interpreter in Unlambda.

How to Use
----------

    $ unlambda lisp.unl
    > (car '(a b c))
    a
    > (cdr '(a b c))
    (b c)
    > (cons 1 (cons 2 (cons 3 ())))
    (1 2 3)
    > (defun fact (n) (if (eq n 0) 1 (* n (fact (- n 1)))))
    fact
    > (fact 8)
    40320
    > (defun fib (n) (if (eq n 1) 1 (if (eq n 0) 1 (+ (fib (- n 1)) (fib (- n 2))))))
    fib
    > (fib 10)
    89
    > (cons 1 (call/cc (lambda (c) (cons 2 (c (cons 3 nil))))))
    (1 3)

Builtin Functions
-----------------

- car
- cdr
- cons
- eq (can compare numbers)
- atom
- +, -, *, /, mod
- call/cc

Special Forms
-------------

- quote
- if
- lambda
- defun

Links
-----

- [Unlambda homepage](http://www.madore.org/~david/programs/unlambda/)
- [Online Unlambda interpreter](http://inazz.jp/unlambda/)
- [Fast Unlambda interpreter](http://www.math.cas.cz/~jerabek/unlambda/unl.c)
