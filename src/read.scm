;; Lisp reader / printer

(define-module read
  (use gauche.sequence)
  (use unlc)
  (use lib)
  )
(select-module read)

(defmacro idInt c0)
(defmacro idProcedure c1)
(define reserved-ids 2)

; Capitalized functions (Cons, Atom, etc.) are for lisp-level data structures

; Value = Cons | Atom
; Cons x y = (lambda (f _) (f x y))
(defmacro Cons (lambda (e1 e2 f g) (f e1 e2)))
(defmacro (iCons e1 e2) (lambda (f g) (f e1 e2)))
(defmacro (Pair? e) (e (lambda (_ _) I) V))
(defmacro (Car x) (x (lambda (a _) a) V))
(defmacro (Cdr x) (x (lambda (_ b) b) V))
(defmacro (Cadr x) (Car (Cdr x)))

; Atom = (lambda (_ f) (f id data))
;  id=0 : Integer, data is a church-numeral
;    =1 : Procedure, data is a function from a List of arguments to a Value
;   >=2 : Symbol, data is a print function
(defmacro Atom (lambda (id data f g) (g id data)))
(defmacro (iAtom id data) (lambda (f g) (g id data)))
(defmacro (Atom? e) (e V (lambda (_ _) I)))
(defmacro (Atom-data e) (e V (lambda (_ data) data)))
(defmacro (eqsym? e symId)
  (e V (lambda (id _) (= id symId))))
(defmacro (eqId? atom1 atom2)
  (atom1 V (lambda (id1 _) (atom2 V (lambda (id2 _) (= id1 id2))))))

(defmacro Int (lambda (n f g) (g idInt n)))
(defmacro (Int? e) (e V (lambda (id _) (zero? id))))
(defmacro (Int-value e) (e V (lambda (_ n) n)))

(defmacro (Procedure f) (lambda (_ g) (g idProcedure f)))
(defmacro (Procedure? e) (eqsym? e idProcedure))
(defmacro (apply-procedure proc args) (proc V (lambda (_ f) (f args))))

;; Symbol table

(define character-table
  "abcdefghijklmnopqrstuvwxyz0123456789-+?!\"\#$%&'*,./:<=>@[\\]^_`{|}~")

(define (ord c)
  (+ 1 (string-scan character-table (char-downcase c))))

(defrecmacro (ord-rec lst)
  (if (or (lst ->car ->car I) (lst ->car ->cdr I))
      c1
    ((succ) (ord-rec (lst ->cdr)))))

(add-unl-macro! 'ord '()
  `(ord-rec
    (list
     ,@(map (lambda (c)
              (if (eq? c (char-upcase c))
                  `(lambda (f) (f (? ,c) V))
                `(icons (? ,(char-upcase c)) (? ,c))))
            (string->list character-table))
     (lambda (_) ((print-and-return "parser: invalid character " (! I)) #\newline exit I)))))

(defrecmacro (streql lhs rhs)
  (if (null? lhs)
      (null? rhs)
    (and (pair? rhs)
         (= (lhs ->car) (rhs ->car))
         (streql (lhs ->cdr) (rhs ->cdr)))))

; Symtbl = (Int, [Char] -> Int|V)
(defmacro (symtbl-add str symtbl)
  (cons (succ (symtbl ->car))
        (lambda (s)
          (if (streql str s)
              (symtbl ->car)
            ((symtbl ->cdr) s)))))

; (intern Symtbl [Char]) -> (Int, Symtbl)
(defmacro (intern symtbl str)
  (let ((r ((symtbl ->cdr) str)))
    (if (churchnum? r)
        (cons r symtbl)
      (cons (symtbl ->car)
            (symtbl-add str symtbl)))))

(define predefined-symbols
  (list "nil" "lambda" "quote" "if" "defun" "t"
        "print" "cons" "car" "cdr" "eq" "atom" "+" "-" "*" "/" "mod" "call/cc"))

(for-each-with-index
 (lambda (index name)
   (let ((idname (string-append "id" name)))
     (begin
      (string-set! idname 2 (char-upcase (string-ref name 0)))
      (add-unl-macro! (string->symbol idname)
                      '()
                      (churchnum (+ reserved-ids index))))))
 predefined-symbols)

(add-unl-macro! 'empty-symtbl '()
  `(cons ,(churchnum reserved-ids) V))

(add-unl-macro! 'initial-symtbl '()
  `(let ((a symtbl-add))
     ,(fold (lambda (name e)
              `(a (list ,@(map (lambda (c) (churchnum (ord c)))
                               (reverse (string->list name))))
                  ,e))
            'empty-symtbl
            predefined-symbols)))

(defmacro AtomNil (Atom idNil (string "nil")))
(defmacro AtomLambda (Atom idLambda (string "lambda")))
(defmacro AtomQuote (Atom idQuote (string "quote")))
(defmacro AtomIf (Atom idIf (string "if")))
(defmacro AtomDefun (Atom idDefun (string "defun")))
(defmacro AtomPrint (Atom idPrint (string "print")))
(defmacro AtomT (Atom idT (string "t")))
(defmacro (List1 a) (Cons a AtomNil))
(defmacro (List2 a b) (Cons a (Cons b AtomNil)))
(defmacro (List3 a b c) (Cons a (Cons b (Cons c AtomNil))))

;; Reader

; (read-symbol nil I) -> ([Char], print-func)
(defrecmacro (read-symbol a pf)
  (if (or (read-char=? #\space)
          (read-char=? #\newline)
          (read-char=? #\()
          (read-char=? #\)))
      (cons a pf)
    (let ((a2 (cons (ord) a))
          (pf2 (! (S pf))))
      ((@ I) (read-symbol a2 pf2)))))

(defrecmacro (read-number a)
  (let ((d (parse-digit1)))
    (if (d I I)
        ((@ I)
         (read-number ((if (a I I) (add (mul a c10)) I) d)))
      a)))

(defmacro (skip-whitespaces)
  (K I
     ((call/cc I)
      (call/cc ((or (read-char=? #\space)
                    (read-char=? #\newline))
                @ I)))))

; (read V Symtbl) -> (Sexp, Symtbl)
(defrecmacro (read inlist symtbl)
  ((skip-whitespaces)
   (if inlist
       (if (read-char=? #\))
           (@ I (cons AtomNil symtbl))
         ((read V symtbl)
          (lambda (e symtbl2)
            ((read I symtbl2)
             (lambda (es symtbl3)
               (cons (Cons e es) symtbl3))))))
     (cond ((read-char=? #\()
            ((@ I) (read I symtbl)))
           ((read-char=? #\')
            ((@ I) ((read V symtbl)
                    (lambda (e symtbl2)
                      (cons (List2 AtomQuote e) symtbl2)))))
           (else
            (let ((num (read-number V)))
              (if (num I I)
                  (cons (Int num) symtbl)
                ((read-symbol nil I)
                 (lambda (str pf)
                   ((intern symtbl str)
                    (lambda (id new-symtbl)
                      (cons (Atom id pf) new-symtbl))))))))))))

;; Printer

(defrecmacro (Print inlist e)
  (if inlist
      (e (lambda (hd tl)
           ((#\space Print V hd) (Print I tl)))
         (lambda (id pf)
           (if (= id idNil) (#\) I)
             ((print " . ") (Print V e) #\) I))))
    (e (lambda (hd tl)
         (#\( I (Print V hd) (Print I tl)))
       (lambda (id pf)
         (cond ((= id idInt) (print-digit pf I))
               ((= id idProcedure) (print "<procedure>"))
               (else (pf I)))))))

; (defmacro (read-and-print)
;   ((@ I)
;    ((read V initial-symtbl)
;     (lambda (e _) (Print V e)))))
;
; (define (main args)
;   (print-as-unl '(read-and-print)))
