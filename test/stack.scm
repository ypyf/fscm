(define stack '())

(define push!
    (lambda (x)
    (set! stack (cons x stack))))
    
(define pop!
    (lambda ()
        (let ((temp (car stack)))
            (set! stack (cdr stack))
            temp)))
            
(push! 9)
(push! 8)
(push! 7)            