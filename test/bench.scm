(define (build counter list)
    (if (= 0 counter) list
        (build (- counter 1) (cons counter list))))
        
(time (build 1000000 '()))

(define count 0)
(define jj (lambda () 
	(if (< count 1000000)
		(begin (set! count (+ count 1))(jj))
		count)))
(time (jj))        