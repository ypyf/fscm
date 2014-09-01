(define (foo)
    (call/cc
         (lambda (return)
                (define (f) (return "return from foo from inside proc"))
                     (f) ; control leaves foo here
                          (return "return from foo"))))

(define (bar)
    (call/cc
         (lambda (return)
                (define (f) (call/cc (lambda (return) (return "return from lambda"))))
                     (f) ; control does not leave bar here
                          (return "return from bar"))))

(display (foo)) ; prints "return from foo from inside proc"
(newline)
(display (bar))
(newline) ; prints "return from bar"
