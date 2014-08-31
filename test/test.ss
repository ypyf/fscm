(define expt (lambda (b n)
  (define expt-iter (lambda (b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product)))))
  (expt-iter b n 1)))

;(define fib (trace-lambda (n)
;                          (if (< n 2)
;                              n
;                              (+ (fib (- n 1))
;                                 (fib (- n 2))))))

(define fib (lambda (n)
                          (if (< n 2)
                              n
                              (+ (fib (- n 1))
                                 (fib (- n 2))))))

;(letrec ((repl (lambda () 
;                 (write (eval (read)))
;                 (repl))))
;  (repl))
  