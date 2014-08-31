                            
;(define (eval-formula formula)
;    (eval `(let ([x 2]
;                 [y 3])
;             ,formula)))

(define (broken-eval-formula formula)
    (let ((x 2)
          (y 3))
      (eval formula (interaction-environment))))
(broken-eval-formula '(+ x y))