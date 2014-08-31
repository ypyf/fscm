(define-syntax let/cc
  (syntax-rules ()
    ((let/cc k body) (call/cc (lambda (k) body)))))

(+ 1 (let/cc x (+ (lambda () "hell") (x 12))))

;
;(letrec ((factorial
;          (lambda (n)
;            (if (> n 0)
;                (* n (factorial (- n 1)))
;                1))))
;  (factorial 3))

(define (f x) (lambda (a) (+ x a)))
(define (g y) (+ y y))

(let ((fx (f 2)))
  (let ((gy (g 2)))
    (fx gy)))
        
(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))

;(+ (call/cc (lambda (cont) (cont 4))) 5) 

; call/cc用于大规模退出（non-local escapes）
(define (map-/ lst)
  (call/cc
   (lambda (return)
     (map (lambda (x)
            (if (= x 0)
                (return "Zero Division Error")
                (/ 1 x)))
          lst))))

; call/cc用于回溯
(define fail
  (lambda () 
    (display "no solution")
    (newline)))

(define in-range
  (lambda (a b)
    (call/cc  ; 创建当前延续体
     (lambda (cont)
       (enumerate a b cont)))))

(define n 0)
(define enumerate
  (lambda (a b cont)
    (set! n (+ n 1))
    (if (> a b)
        (fail)
        (let ((save fail)) ; 保存前一次fail过程
          (set! fail ; 定义新的fail过程
                (lambda ()
                  (set! fail save) ; 回溯
                  (enumerate (+ a 1) b cont)))
          (cont a)))))

(define backtrack (lambda () (let ((x (in-range 2 100))
      (y (in-range 2 100))
      (z (in-range 2 100)))
  (if (= (* x x)
         (+ (* y y) (* z z)))
      ((display (list x y z))
       (newline)
       (fail))
      (begin ;;(display (list x y z))
       ;;(newline)
       (fail))))))

;(display "迭代次数: ")
;(display n)

;;(lambda (x y z)
;;  (let ((f (lambda (a b)
;;             (+ (* a x) (* b y)))))
;;    f))

(define cont 0)
(define (baz a)(call/cc (lambda (cc)
                          (let ((s 1))
                            (set! cont cc)
                            (cc (+ s a))))))
(define (foo a) 
  (+ 100 
     (call/cc 
      (lambda (cc) ((set! cont cc)
                    (cc a))))))

;; 延续作为generator/coroutine
(define cont #f)
(define generator (let ((x 0)) 
    (call/cc (lambda (k) 
               (set! cont k)))
    (set! x (+ x 1)) 
    x))

(define looper (lambda ()
                 (begin 
                   (display "> ") 
                   (display (eval (read) (interaction-environment))) 
                   (newline) 
                   (looper))))
;(looper)



; std lib
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce fold)

; generate html
(define markup (lambda (name inner) (display
  (string-append 
   (string-append "<" (symbol->string name) ">") 
   (fold string-append "" inner)
   (string-append "</" (symbol->string name) ">")))))

;(define html (lambda inner (markup 'html inner)))
;(define head (lambda inner (markup 'head inner)))
;(define title (lambda inner (markup 'title inner)))
;(define body (lambda inner (markup 'body inner)))
(define a (lambda inner (markup 'a inner)))
;(define span (lambda inner (markup 'span inner)))

; 惰性延迟求值（按需调用）
(define (delayCL exp) 
  (let ((done #f)
        (val #f))
    (lambda () 
      (cond (done val)
            (else (set! val exp)  ; 第一次求值
                  (set! done #t)
                  val)))))
; 求值
(define (forceCL exp) (exp))

; (-> x a b c) => (c (b (a x)))
(define-syntax ->
  (syntax-rules ()
    ((-> x) x)
    ((-> x a b ...) (-> (a x) b ...))))

; begin
(define-syntax beginx
  (syntax-rules ()
    ((beginx exp ...)
     ((lambda () exp ...)))))

; arithmetic and decimal test case
(define (sqrt2 n)
  (define (abs x) (if (< x 0) (- x) x))
  (define (good-enough? guess)
    (< (abs (- n (square guess))) 0.0001))
  (define (average x y) (/ (+ x y) 2))
  (define (square x) (* x x))
  (define (improve guess)
    (average (/ n guess) guess))
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))))
  (iter 1.0))

(display (sqrt2 2))
(newline)

; very hard test case
((lambda (quote if) (quote if)) list 1)