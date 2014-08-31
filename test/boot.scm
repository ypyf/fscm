(define repl (lambda () (begin (display "lisp> ") (flush-output) (display (eval (read))) (newline)(repl))))
(repl)

; 应该等价于(list 1)，所以输出的是 ==> (1)
; 在某些错误的实现中(包括tinyscheme 1.41)，等价于(quote if)，所以输出的是 ==> if
; 原因是安装标准，这些语法关键词应该是环境的一部分，所以可以被闭包lambda的参数覆盖
((lambda (quote if) (quote if)) list 1) 