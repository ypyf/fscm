; 正确 ==> 3
(car (cdr (car (cdr '(1 (2 3 4) 5 6 7)))))

; 应该等价于(list 1)，所以输出的是 ==> (1)
; 在某些错误的实现中(比如tinyscheme 1.41)，等价于(quote if)，所以输出的是 ==> if
((lambda (quote if) (quote if)) list 1) 

; 闭包作用域测试
(define func +)
(define (foo func) (lambda (a b) (func a b)))
((foo *) 1 2) ; => 2

(define (foo) (lambda (a b) (func a b)))
((foo) 1 2) ; => 错误: func未定义
(define funcx +)
((foo) 1 2) ; => 3

; 
(define (foo func) (lambda (a b) (set! a 100) (func a b)))
((foo *) 1 2) ; => 200

; => (a b c)
(append '(a b c) '())