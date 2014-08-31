;(call/cc call/cc) ; 返回#<cont> 第一个callcc用它的继续当参数调用第二个callcc函数
;((call/cc call/cc) (call/cc call/cc))	;; 死循环.左边子表达式是当前继续，右边也是当前继续（但是是右边的）
;; ([] [])

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

;(lwp (lambda () (let f () (pause) (display "h") (f))))
;(lwp (lambda () (let f () (pause) (display "e") (f))))
;(lwp (lambda () (let f () (pause) (display "y") (f))))
;(lwp (lambda () (let f () (pause) (display "!") (f))))
;(lwp (lambda () (let f () (pause) (newline) (f))))

;; memory leak?
(define (leak-test1 identity-thunk)
       (let loop ((id (lambda (x) x)))
           (loop (id (identity-thunk)))))

(let* ((yin ((lambda (foo) (newline) foo)
                          (call/cc (lambda (bar) bar))))
              (yang ((lambda (foo) (write-char #\*) foo)
                                   (call/cc (lambda (bar) bar)))))
    (yin yang))
