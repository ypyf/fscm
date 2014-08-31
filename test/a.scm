(let ((result '(0 0 0)))
(if (eqv? result '(0 0 0))
      (display "Map is call/cc safe, but probably not tail recursive or inefficient.")
      (display "Map is not call/cc safe, but probably tail recursive and efficient."))
)

