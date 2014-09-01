(define start #f)

(if (not start)
    (call/cc (lambda (cc)
                            (set! start cc))))

(display "Going to invoke (start)\n")

(start #f)

