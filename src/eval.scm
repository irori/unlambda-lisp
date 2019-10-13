#!/usr/bin/gosh

;; Unlambda-Lisp: main script

(add-load-path "." :relative)
(use unlambda.compiler)
(use unlambda.prelude)
(use lib)
(use read)

;; Utilities

(defmacro (Lambda? e) (eqsym? (Car e) idLambda))
(defmacro (Lambda-vars e) (Car (Cdr e)))
(defmacro (Lambda-body e) (Car (Cdr (Cdr e))))
(defmacro (Quote? e) (eqsym? (Car e) idQuote))
(defmacro (If? e) (eqsym? (Car e) idIf))
(defmacro (If-cond e) (Car (Cdr e)))
(defmacro (If-then e) (Car (Cdr (Cdr e))))
(defmacro (If-else e) (Car (Cdr (Cdr (Cdr e)))))
(defmacro (Defun? e) (eqsym? (Car e) idDefun))
(defmacro (Defun-name e) (Car (Cdr e)))
(defmacro (Defun-vars e) (Car (Cdr (Cdr e))))
(defmacro (Defun-body e) (Car (Cdr (Cdr (Cdr e)))))

(defrecmacro (Map f lst)
  (lst (lambda (hd tl)
         (Cons (f hd) (Map f tl)))
       (lambda (_ _) lst)))

;; Builtin functions

(defmacro PrimCar
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Car args)))
         (errexit "car: invalid argument")
       (Car (Car args))))))

(defmacro PrimCdr
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Car args)))
         (errexit "cdr: invalid argument")
       (Cdr (Car args))))))

(defmacro PrimAtom
  (Procedure
   (lambda (args)
     (cond ((Atom? args)
            (errexit "atom: invalid argument"))
           ((Atom? (Car args)) AtomT)
           (else AtomNil)))))

(defmacro PrimEq
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args)))
         (errexit "eq: invalid argument")
       (cond ((and (Int? (Car args)) (Int? (Cadr args)))
              (*if* (= (Int-value (Car args)) (Int-value (Cadr args)))
                    AtomT
                    AtomNil))
             ((Procedure? (Car args))
              AtomNil)
             ((eqId? (Car args) (Cadr args))
              AtomT)
             (else AtomNil))))))

(defmacro Prim+
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args))
             (not (and (Int? (Car args)) (Int? (Cadr args)))))
         (errexit "+: invalid argument")
       (Int (add (Int-value (Car args)) (Int-value (Cadr args))))))))

(defmacro Prim-
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args))
             (not (and (Int? (Car args)) (Int? (Cadr args)))))
         (errexit "-: invalid argument")
       (Int (sub (Int-value (Car args)) (Int-value (Cadr args))))))))

(defmacro Prim*
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args))
             (not (and (Int? (Car args)) (Int? (Cadr args)))))
         (errexit "*: invalid argument")
       (Int (mul (Int-value (Car args)) (Int-value (Cadr args))))))))

(defmacro Prim/
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args))
             (not (and (Int? (Car args)) (Int? (Cadr args)))))
         (errexit "/: invalid argument")
       (if (zero? (Int-value (Cadr args)))
           (errexit "/: division by zero")
         (Int (div (Int-value (Car args)) (Int-value (Cadr args)))))))))

(defmacro PrimMod
  (Procedure
   (lambda (args)
     (if (or (Atom? args)
             (Atom? (Cdr args))
             (not (and (Int? (Car args)) (Int? (Cadr args)))))
         (errexit "mod: invalid argument")
       (if (zero? (Int-value (Cadr args)))
           (errexit "mod: division by zero")
         (Int (mod (Int-value (Car args)) (Int-value (Cadr args)))))))))

(defmacro PrimCall/cc
  (Procedure
   (lambda (args)
     (if (and (Pair? args)
              (Procedure? (Car args)))
         (call/cc (lambda (cont)
                    (apply-procedure (Car args) (List1 (Procedure (lambda (args) (cont (Car args))))))))
       (errexit "call/cc: invalid argument")))))

;; Evaluator

; Env = Atom -> Value
(defmacro initial-env
  (lambda (var)
    (cond ((eqsym? var idNil) AtomNil)
          ((eqsym? var idPrint) (Procedure (lambda (e) (K AtomNil (#\newline (Print V (Car e)))))))
          ((eqsym? var idCons) (Procedure (lambda (e) (Cons (Car e) (Cadr e)))))
          ((eqsym? var idCar) PrimCar)
          ((eqsym? var idCdr) PrimCdr)
          ((eqsym? var idAtom) PrimAtom)
          ((eqsym? var idEq) PrimEq)
          ((eqsym? var id+) Prim+)
          ((eqsym? var id-) Prim-)
          ((eqsym? var id*) Prim*)
          ((eqsym? var id/) Prim/)
          ((eqsym? var idMod) PrimMod)
          ((eqsym? var idCall/cc) PrimCall/cc)
          (else (exit ((print "unbound variable ")
                       (Atom-data var I)
                       (#\newline I)))))))

(defrecmacro (extend-env vars vals env)
  (if (Pair? vars)
      (if (Pair? vals)
          (lambda (v)
            (if (eqId? v (Car vars))
                (Car vals)
              (extend-env (Cdr vars) (Cdr vals) env v)))
        (errexit "too few arguments"))
    (if (Pair? vals)
        (errexit "too many arguments")
      env)))

(defmacro (make-procedure evl e env)
  (Procedure
   (lambda (args)
     (evl (Lambda-body e)
          (extend-env (Lambda-vars e) args env)))))

(defrecmacro (apply evl operator args)
  (if (Procedure? operator)
      (apply-procedure operator args)
    (errexit "apply: unknown procedure type")))

(defrecmacro (eval e env)
  (cond ((Int? e) e)
        ((Atom? e) (env e))
        ((Quote? e) (Cadr e))
        ((Lambda? e) (make-procedure eval e env))
        ((If? e)
         (if (eqsym? (eval (If-cond e) env) idNil)
             (eval (If-else e) env)
           (eval (If-then e) env)))
        (else (let ((es (Map (lambda (x) (eval x env)) e)))
                (apply eval (Car es) (Cdr es))))))

; (eval-defun Defun env) -> env
(defrecmacro (eval-defun evl e env)
  (extend-env (List1 (Defun-name e))
              (List1 (make-procedure evl (List3 AtomLambda (Defun-vars e) (Defun-body e))
                                     (eval-defun evl e env)))
              env))

;; REPL

(defmacro (prompt) (#\> #\space I))

(defrecmacro (repl symtbl env)
  ((read V symtbl)
   (lambda (e symtbl2)
     (let ((evl eval))
       (if (Defun? e)
	   (let ((newenv (eval-defun evl e env)))
	     ((Atom-data (Defun-name e)) I (#\newline I) (prompt)
	      repl symtbl2 newenv))
         ((apply-procedure (env AtomPrint) (List1 (evl e env)))
	  ((prompt)
	   repl symtbl2 env)))))))

(defmacro main
  ((prompt)
   @ I repl initial-symtbl initial-env))

(define (main args)
  (print-as-unl 'main)
  0)
