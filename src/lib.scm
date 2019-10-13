;; Library macros & syntaxes

(define-module lib
  (use unlambda.compiler)
  (use unlambda.prelude)
  )
(select-module lib)

(defmacro (errexit s)
  ((print-and-return s exit) I))

(defmacro parse-digit1-fn
 (lambda (q)
   (((((((((((((? #\0) I) q) c0)
	    ((((? #\1) I) q) c1))
	   ((((? #\2) I) q) c2))
	  ((((? #\3) I) q) c3))
	 ((((? #\4) I) q) c4))
	((((? #\5) I) q) c5))
       ((((? #\6) I) q) c6))
      ((((? #\7) I) q) c7))
     ((((? #\8) I) q) c8))
    ((((? #\9) I) q) c9))))

(defmacro parse-digit1
  (call/cc parse-digit1-fn))
