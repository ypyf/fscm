(define mvar 11)

(define (f n)
    (define (g m)
        (+ m m))
    (g n))