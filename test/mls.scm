(define (how-many-digits n)
       (if (< n 10) 1 (+ 1 (how-many-digits (quotient n 10))))) 

(define (first-digit n)
       (quotient n (expt 10 (- (how-many-digits n) 1)))) 

(define (del-first-digit n)
       (modulo n (expt 10 (- (how-many-digits n) 1)))) 

(define (max_x-1_9s n) ;; n > 9
       (- (- n (del-first-digit n)) 1)) 

(define (all-digits-is-9? n)
       (if (< n 10) (if (= n 9) #t #f)
                            (if (= (modulo n 10) 9) (all-digits-is-9? (quotient n 10))
                                                                         #f))) 

(define (all-digits-is-9-ignore-first? n) ;; n > 9
       (all-digits-is-9? (del-first-digit n))) 

(define (f n)
       (cond ((< n 10) (if (> n 0) 1 0))
                        ((all-digits-is-9? n) (quotient (* (+ n 1) (how-many-digits n)) 10))
                               ((all-digits-is-9-ignore-first? n)
                                           (+ (* (+ (first-digit n) 1)
                                                                     (f (del-first-digit n)))
                                                               (expt 10 (- (how-many-digits n)
                                                                                         1))))
                                         (else (+ (f (max_x-1_9s n))
                                                                     (f-from-a-zeros-to-n (+ (max_x-1_9s n) 1) n))))) 

(define (f-from-a-zeros-to-n a-zeros n)
       (if (= (first-digit a-zeros) 1) (+ (del-first-digit n) (f (del-first-digit n)) 1)
                     (f (del-first-digit n))))

(display  (f 322566932356566))
