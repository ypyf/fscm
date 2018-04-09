;(the "rain" in "spain" falls mainly on the "plain")
(let ((x "rain")
(y "spain")
 (z "plain"))
 (quasiquote
 (the (unquote x)
 in (unquote y)
 falls mainly on the (unquote z))))
 
(define x (list 1 2 3))
`(+ ,@x) ; => (+ 1 2 3)
`(+ (+ ,@x)) ; => (+ (+ 1 2 3))
`(+ 1 2 `,@x) ; => (+ 1 2 (quasiquote (unquote-splicing x)))
`(+ 1 2 `,@x ,@x) ; => (+ 1 2 (quasiquote (unquote-splicing x)) 1 2 3)