;; => let: bad syntax in: (+ a b)
(let ((a 1) (b (+ 1 2)) (+ a b)))


;; => (a b c) 
(let ((x 'a) (y '(b c)))
  (cons x y))

; => (1 0)
(let ((x 0) (y 1))
  (let ((x y) (y x))
    (list x y)))


; 错误 lambda中的egg未定义
(let ((egg 100)(fn (lambda (x) (+ x egg)))) (fn 2))

; 正确  ==> 102
(letrec ((cow 100)(fn (lambda (x) (+ x cow)))) (fn 2))
(let* ((a 100)(fn (lambda (x) (+ x a)))) (fn 2))
(let* ((koop 1) (boop koop) (toop (+ koop boop))) toop) ; ==> 2

; 错误 factorial未定义
(let ((factorial
	(lambda (n)
        (if (> n 0)
				(* n (factorial (- n 1)))
				1))))
	(factorial 3))
	
; 正确 ==> 6
(letrec ((factorial
	(lambda (n)
        (if (> n 0)
				(* n (factorial (- n 1)))
				1))))
	(factorial 3))

; 错误 factorial未定义
(let* ((factorial
	(lambda (n)
        (if (> n 0)
				(* n (factorial (- n 1)))
				1))))
	(factorial 3))
	
; 正确 ==> 4
(letrec ((a 1) (b (+ 1 2))) (+ a b))

; racket正确   ==> 2
(letrec ((bird 1) (duck bird)) (+ bird duck)) ; R5RS错误(bird被letrec初始化为#undefined)，Racket正确（因为采取的从左到右的顺序求值）
; R5RS的letrec首先在新环境中将所有key都绑定到#undefined
; 然后在新环境对value求值（无序）

; 测试性能 fact 1000
(letrec ((factorial (lambda (n) (if (> n 0) (* n (factorial (- n 1))) 1)))) (factorial 1000))

; 错误
(let ((a 5)
	(b (* a 2))	; a未定义
	(c (- b 3)))
	c)
	
; r5rs错误 racket正确	==> 7
(letrec ((a 5)
	(b (* a 2))
	(c (- b 3)))
	c)
    
; ==> 199
(let* ((a (let* ((t 100)) (+ t 1)))
	(b (* a 2))
	(c (- b 3)))
	c)	
	
; 正确 ==> 7
(let ((a 5))
	(let ((b (* a 2)))
		(let ((c (- b 3)))
			c)))

; 正确 ==> -80 通过平行赋值来进行变量交换
(let ((a 100) (b 20)) 
	(let ((a b)(b a)) 
		(- a b)))

; ==> 70	
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))

(define factorial
	(lambda (n)
		(if (> n 0)
			(* n (factorial (- n 1)))
			1)))