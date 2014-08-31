;;Not really an error to fail this (Matthias Radestock)
;;If this returns (0 1 0), your map isn't call/cc safe, but is probably
;;tail-recursive.  If its (0 0 0), the opposite is true.
(let ((result 
       (let ()
         (define executed-k #f)
         (define cont #f)
         (define res1 #f)
         (define res2 #f)
         (set! res1 (map (lambda (x)
                           (if (= x 0)
                               (call/cc (lambda (k) (set! cont k) 0))
                               0))
                         '(1 0 2)))
         (if (not executed-k)           
             (begin (set! executed-k #t) 
                    (set! res2 res1)
                    (cont 1)))
         res2)))
  (if (eqv? result '(0 0 0))
      (display "Map is call/cc safe, but probably not tail recursive or inefficient.")
      (display "Map is not call/cc safe, but probably tail recursive and efficient."))
  (newline))
  
; Infinit Tail Recursion
(define f (lambda () (f)))
(define g (lambda (a b) ((lambda () (+ 1 3)(g a b)))))
(define h (lambda (a b) (if 1 (begin (+ 1 2) (h a b)) a)))

; 有穷递归
(define j (lambda (a b) 
	(if (< a b)
		(j (+ a 1) b)
		a)))

;; (+ 1 2 3 (j 1 2))

;; 有问题
(define count 0)
(define jj (lambda () 
	(if (< count 1000000)
		(begin (set! count (+ count 1))(jj))
		count)))

(define b0 (lambda() (set! count (+ count 1))(f0)))
(define kk (lambda () 
	(if (< count 1000000)
		(begin (set! count (+ count 1))(kk))
		1)))
		
;; 互递归
(define f0 (lambda()(f1)))
(define f1 (lambda()(f2)))
(define f2 (lambda()(f0)))

(define (fact n)
  (define (product min max)
    (if (= min n)
	max
	(product (+ 1 min)
		 (* min max))))
  (product 1 n))
  
(define (fib n) (if (= n 1)
                    1 
                    (if (= n 2) 1(fibTail n 1 1))))
                    
(define (fibTail n a b) (if (= n 3) 
                            (+ a b)
                            (fibTail (- n 1) b (+ a b))))

;;   