(define lwp-list '())
(define lwp
  (lambda (thunk)
	(set! lwp-list (append lwp-list (list thunk)))))

(define start
  (lambda ()
	(let ((p (car lwp-list)))
	  (set! lwp-list (cdr lwp-list))
	  (p))))

(define pause
  (lambda ()
	(call/cc
	 (lambda (k)
	   (lwp (lambda () (k #f)))
	   (start)))))

(define (f0) (pause) (display "h") (f0))
(define (f1) (pause) (display "e") (f1))
(define (f2) (pause) (display "y") (f2))
(define (f3) (pause) (display "!") (f3))
(define (f4) (pause) (newline) (flush-output) (f4))

(lwp f0)
(lwp f1)
(lwp f2)
(lwp f3)
(lwp f4)