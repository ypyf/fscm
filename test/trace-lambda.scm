#lang racket
; 当输入是1000的时候，guile会stack-overflow
; 我的JScheme则没有问题，但10000就不行了
(define sum
  (lambda (n)
    (if (= n 1)
	1
	(+ n (sum (- n 1))))))

(define sum
  (trace-lambda (n)
    (if (= n 1)
	1
	(+ n (sum (- n 1))))))

;(define factorial 
;  (lambda (n) (if (> n 0) (* n (factorial (- n 1))) 1)))


;(letrec ((count 0) (ret 0)
;         (print-call 
;          (lambda (name arg c)
;            (if (> c 0) 
;                (begin (set! c (- c 1)) (display " |") (print-call name arg c))
;                (begin (display (string-append "call: " name (number->string arg) "\n"))
;                       (set! count (+ count 1))))))
;         (print-ret 
;          (lambda (ret-value c)
;            (if (> c 1) 
;                (begin (set! c (- c 1)) (display " |") (print-ret ret-value c))
;                (begin (display (string-append "return: " (number->string ret-value) "\n"))
;                       (set! count (- count 1))))
;            ret-value))
;         (factorial
;          (lambda (n) 
;            (if (> n 0) (begin (print-call "factorial: " n count)
;                               (print-ret (* n (factorial (- n 1))) count)) 
;                1))))
; (factorial 5))

(letrec ((count 0) (ret 0)
         (print-call 
          (lambda (name arg c)
            (if (> c 0) 
                (begin (set! c (- c 1)) (display " |") (print-call name arg c))
                (begin (display (string-append "call: " name (number->string arg) "\n"))
                       (set! count (+ count 1))))))
         (print-ret 
          (lambda (ret-value c)
            (if (> c 1) 
                (begin (set! c (- c 1)) (display " |") (print-ret ret-value c))
                (begin (display (string-append "return: " (number->string ret-value) "\n"))
                       (set! count (- count 1))))
            ret-value))
         (factorial
          (lambda (n) 
            (if (> n 0)
                (* n (t-fn (- n 1)))
            1)))
         (t-fn (lambda (n) 
                 (print-call "factorial: " n count)
                 (factorial n)
                 (print-ret n count))))
 (factorial 5))

;(define expt (trace-lambda (b n)
;   (if (= n 0)
;       1
;       (* b (expt b (- n 1))))))

(define expt (trace-lambda (b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1)))