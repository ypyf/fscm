(define (is a b) (display (equal? a b)) (newline))

; 应该等价于(list 1)，所以输出的是 ==> (1)
; 在某些错误的实现中(比如tinyscheme 1.41)，等价于(quote if)，所以输出的是 ==> if
(is ((lambda (quote if) (quote if)) list 1) '(1))

; procedure application
(is 1 (call/cc (lambda (c) (0 (c 1)))))

;; shadowing syntatic keywords, bug in MIT Scheme?
(is '(x)
 ((lambda lambda lambda) 'x))

(is '(1 2 3)
 ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))
 
(is '(1)
    ((lambda (quote if) (quote if)) list 1))

;; '1 => (- 1)
;(is #f
;    (let ((quote -)) (eqv? '1 1)))

;;
(define a 100)
(define func +)
(define (foo func) (lambda (a b) (set! a 100) (func a b)))
(is ((foo *) 1 2) 200)

;;
(define bar
  (lambda (x)
    (+ x ((lambda () (set! x 11) x)) x)))
(is (bar 1) 23)

(is ((lambda (a b) (define a 1) a) 99 98) 1)

;;
(define f
  (lambda (x)
    (g x)))
(define g
  (lambda (x)
    (+ x x)))
(is (f 3) 6)

;; test defineVar bug
(define count 0)
(define jj (lambda () 
	(if (< count 1000)
		(begin (set! count (+ count 1)) (jj))
		count)))
(jj)
(define count 0)
(jj)
(is count 1000)