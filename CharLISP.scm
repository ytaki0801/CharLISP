;;;;
;;;; CharLISP.scm: Scheme version of CharLISP
;;;; 
;;;; This code is Licensed under CC0.
;;;; https://creativecommons.org/publicdomain/zero/1.0/
;;;;

(define (csyn1 s p r)
  (if (eq? (car s) #\() (cons r (cdr s))
      (let ((z (csyn0 s)))
	(csyn1 (cdr z) p (cons (car z) r)))))

(define (csyn0 s)
  (if (eq? (car s) #\))
      (csyn1 (cdr s) (car s) '())
      (cons (car s) (cdr s))))

(define (csyn s) (car (csyn0 (reverse (string->list s)))))

(define (ceval e a)
  (if (pair? e)
      (let ((p (car e)) (c (cdr e)))
	(cond ((eq? p #\')
	       (let ((f (lambda (x) (string->symbol (list->string `(,x)))))
		     (d (car c)))
		 (cond ((null? d) '()) ((pair? d) (map f d)) (else (f d)))))
	      ((eq? p #\?)
	       (if (eq? (ceval (car c) a) (ceval (cadr c) a))
		   (ceval (caddr c) a) (ceval (cadddr c) a)))
	      ((eq? p #\:)
	       (let* ((r (reverse c)) (b (car r)) (v (reverse (cdr r))))
		 (list v b a)))
	      (else (let ((f (ceval p a))
			  (v (map (lambda (x) (ceval x a)) c)))
		      (if (pair? f)
			  (let ((a (car f)) (g (cadr f)) (e (caddr f)))
			    (ceval g (append (map cons a v) e)))
			  (cond ((eq? f #\+) (+ (car v) (cadr v)))
				((eq? f #\-) (- (car v) (cadr v)))
				((eq? f #\*) (* (car v) (cadr v)))
				((eq? f #\/) (/ (car v) (cadr v)))
				((eq? f #\^) (expt (car v) (cadr v)))
				((eq? f #\%) (modulo (car v) (cadr v)))
				((eq? f #\$) (cons (car v) (cadr v)))))))))
      (cond ((char-numeric? e) (- (char->integer e) (char->integer #\0)))
	    ((member e '(#\+ #\- #\* #\/ #\^ #\% #\$)) e)
	    (else (cdr (assq e a))))))

(let loop ()
  (display "CharLISP> ")
    (let ((r (ceval (csyn (read)) '())))
      (newline) (display r)
      (newline) (newline)
      (loop)))

#|
Examples:
"('(Hello!!))"
"((:nr($nr))12)"
"((:n(?(%n3)0('Y)('N)))6)"
"(((:g(gg))(:g(:nr(?n0r((gg)(-n1)(*rn))))))51)"
"(((:g(gg))(:g(:nr(?n(-01)r((gg)(-n1)($(((:g(gg))(:g(:nab(?n0a((gg)(-n1)b(+ab))))))n01)r))))))(*37)('()))"
"(((:g(gg))(:g(:nr(?n1r(?(((:g(gg))(:g(:nx(?nx1(?(%nx)0(-01)((gg)n(+x1)))))))n2)1((gg)(-n1)($nr))((gg)(-n1)r))))))(^(*25)2)('()))"
"(((:g(gg))(:g(:nr(?n0r(?(%n(*35))0((gg)(-n1)($('(FizzBuzz))r))(?(%n3)0((gg)(-n1)($('(Fizz))r))(?(%n5)0((gg)(-n1)($('(Buzz))r))((gg)(-n1)($nr)))))))))(*56)('()))"
|#
