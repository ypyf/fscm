(length '(a b c)) ; => 3
(length '((x) ())) ; => 2

; => 3
(car (cdr (car (cdr '(1 (2 3 4) 5 6 7)))))


; 在某些错误的实现中(比如tinyscheme 1.41)，等价于(quote if)
; 应该等价于(list 1)
; => (1) ok
; => if => error
((lambda (quote if) (quote if)) list 1) 

; 闭包作用域测试
; => 2 ok
; => 3 error
(define func +)
(define (foo func) (lambda (a b) (func a b)))
((foo *) 1 2)

(define (foo) (lambda (a b) (func a b)))
; => 3 ok
; => func未定义 error
((foo) 1 2) 

(define (foo func) (lambda (a b) (set! a 100) (func a b)))
((foo *) 1 2) ; => 200

; => (a b c)
(append '(a b c) '())