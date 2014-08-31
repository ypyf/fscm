

;'(. ()) => ()
(quote (. ())) ; ()
(quote (1 . ())) ; (1)
(let ((a 100)) `,a) ; 100
(let ((a 100)) `(1 unquote a)) ; (1 . 100)
(let ((a 100)) `(1 . (unquote a))) ; (1 . 100)
(let ((a 100)) `(1 2 unquote a)) ; (1 2 . 100)
(let ((a 100)) `(1 2 . (unquote a))) ; (1 2 . 100)

; => (a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) ; r5rs中的范例

(let ((name1 'x)(name2 'y))`(a `(b ,,name1 ,',name2 d) e))

; 0
(let ((* -))(* 3 3))

; 9
(let ((* -))
(eval '(* 3 3)))

; set! => 10
(let* ((a 5)
	   (dummy (set! a 10)))
a)

; (1 2)
(let* ((mlet
		(letrec (
				(map
					(lambda (op lst)
						(if (null? lst)
							'()
							(cons (op (car lst))
								  (map op (cdr lst))))))
				(cars
					(lambda (lst)
						(map car lst)))
				(cadrs
					(lambda (lst)
						(map (lambda (x) (car (cdr x))) lst)))
				)
			(macro (bindings body)
					(let* ((names (cars bindings))
						   (values (cadrs bindings)))
						(cons (list (quote lambda) names body) values))))))
(mlet ((a 1)
	    (b 2))
	(list a b)))
	
(define map
	(lambda (op lst)
		(if (null? lst)
			'()
			(cons (op (car lst))
				 (map op (cdr lst))))))

; 3
(let* ((mlet
		(letrec (
				(map
					(lambda (op lst)
						(if (null? lst)
							'()
							(cons (op (car lst))
								  (map op (cdr lst))))))
				(cars
					(lambda (lst)
						(map car lst)))
				(cadrs
					(lambda (lst)
						(map (lambda (x) (car (cdr x))) lst)))
				)
			(macro (bindings body)
					(let* ((names (cars bindings))
						   (values (cadrs bindings)))
						(cons (list (quote lambda) names body) values))))))
(mlet ((a 1)
	    (b 2))
	(+ a b)))	