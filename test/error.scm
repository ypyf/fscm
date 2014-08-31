(call/fc (lambda () (call/fc (lambda () (+ (display "hello" "xx") 3)) (lambda (message ok fk) (fk message ok)))) (lambda (message ok fk) (ok 4)))

(call/fc (lambda ()(error "bad things happened"))(lambda (message error-continuation parent-fk)(parent-fk message error-continuation)))

