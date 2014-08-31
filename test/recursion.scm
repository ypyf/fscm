(define (fib n) (if (or (= n 1) (= n 2)) 
                    1 
                    (fibTail n 1 1)))
                    
(define (fibTail n a b) (if (= n 3) 
                            (+ a b)
                            (fibTail (- n 1) b (+ a b))))

(define foo
  (lambda (x) (if (> x 100) (+ x 10)
				  (+ x 100))))
							
(define add-all
	(lambda (n lst)
		(map (lambda (x) (+ x n)) lst)))

; add-all最后一句中的 (lambda (x) (+ x n))可以单独拿出来:
(define make-adder
	(lambda (n)
		(lambda (x) (+ x n))))

; 此时可以这样调用:
; (map (make-adder 1) '(10 20 30)) => (11 12 13)

(define f
	(lambda (n x)
		(if (= n 0)
			(car x)
			(f (- n 1) (cons (cdr x) (+ (car x) (cdr x)))))))
			
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))