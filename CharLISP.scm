;;;;
;;;; CharLISP.scm: Scheme version of CharLISP
;;;; 
;;;; This code is Licensed under CC0.
;;;; https://creativecommons.org/publicdomain/zero/1.0/
;;;;

(define BTOKENS `(,#\( ,#\) ,#\. ,#\' ,#\? ,#\:
		  ,#\+ ,#\- ,#\* ,#\% ,#\= ,#\< ,#\$ ,#\[ ,#\]))

(define (csyn1 s r)
  (cond ((eq? (car s) #\() (cons r (cdr s)))
	((eq? (car s) #\.)
	 (let ((z (csyn0 (cdr s))))
	   (csyn1 (cdr z) (cons (car z) (car r)))))
	(else
	 (let ((z (csyn0 s)))
	   (csyn1 (cdr z) (cons (car z) r))))))

(define (csyn0 s) (if (eq? (car s) #\)) (csyn1 (cdr s) '()) s))

(define (csyn s)
  (car (csyn0
	(map (lambda (x)
	       (cond ((char-numeric? x)
		      (- (char->integer x) (char->integer #\0)))
		     ((not (member x BTOKENS))
		      (string->symbol (list->string `(,x))))
		     (else x)))
	     (reverse (string->list s))))))

(define (ceval e a)
  (if (pair? e)
      (let ((p (car e)) (c (cdr e)))
	(cond ((eq? p #\') (car c))
	      ((eq? p #\:)
	       (let* ((r (reverse c)) (b (car r)) (v (reverse (cdr r))))
		 (list v b a)))
	      (else (let ((f (ceval p a))
			  (v (map (lambda (x) (ceval x a)) c)))
		      (if (pair? f)
			  (let ((a (car f)) (g (cadr f)) (e (caddr f)))
			    (ceval g (append (map cons a v) e)))
			  (capply f v))))))
      (cond ((or (number? e) (member e BTOKENS)) e)
	    (else (cdr (assq e a))))))

(define T '((a b) (a) ()))
(define F '((a b) (b) ()))

(define (capply f v)
  (cond ((eq? f #\+) (+ (car v) (cadr v)))
	((eq? f #\-) (- (car v) (cadr v)))
	((eq? f #\*) (* (car v) (cadr v)))
	((eq? f #\%) (modulo (car v) (cadr v)))
	((eq? f #\=) (if (eq? (car v) (cadr v)) T F))
	((eq? f #\<) (if (<   (car v) (cadr v)) T F))
	((eq? f #\$) (cons (car v) (cadr v)))
	((eq? f #\[) (car (car v)))
	((eq? f #\]) (cdr (car v)))))

;;;; (define (CharLISP s) (display (ceval (csyn s) '())) (newline))
(let loop ()
  (display "CharLISP> ")
   (let ((r (ceval (csyn (read)) '())))
      (newline) (display r) (newline) (newline)
      (loop)))

#|
Examples:
"('(Hello!!))" => (H e l l o ! !)
"((:nr($nr))12)" => (1 . 2)
"((:n((=(%n3)0)(:('Y))(:('N))))6)" => Y
"((:x($([(]x))($([x)(](]x)))))('(abc)))" => (b a c)
|#
