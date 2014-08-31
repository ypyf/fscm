; Demonstrating the memory leak caused by capturing the full -- longer than
; needed -- continuation
; The code is R5RS Scheme, tested on Scheme48 and Petite Chez Scheme

; The most straightforward implementation of generators
(define gen-ret #f)
(define (invoke thunk)
  (call-with-current-continuation (lambda (k)
    (set! gen-ret k)
    (thunk)
    '())))
(define (yield v)
  (call-with-current-continuation (lambda (k)
    (gen-ret (cons v (lambda () (k #f)))))))

; Two simple generators
(define (ones)
  (yield 1)
  (ones))

(define (from n)
  (lambda ()
    (yield n)
    ((from (+ n 1)))))

; Convert a generator to a lazy stream
(define (gen->stream gen)
  (delay 
    (let ((v (invoke gen)))
      (cond
	((null? v) '())
	(else (cons (car v) (gen->stream (cdr v))))))))

; Print first n elements of a stream, or all stream if n is #f
(define (print-stream n stream)
  (if (and n (zero? n)) '()
    (let ((v (force stream)))
      (cond
	((null? v) '())
	(else 
	  ;; (display (car v)) (newline)
	  (print-stream (and n (- n 1)) (cdr v)))))))

; A simple test case
(print-stream 5 (gen->stream (from 1)))

; Should run in constant memory -- but it runs out of memory within seconds
(print-stream #f (gen->stream ones))


; Scheme48 contains a leak-free implementation of shift/reset
;   ,open signals escapes
;   ,load =scheme48/misc/shift-reset.scm

; We now implement generators in terms of shift-reset
(define (invoke thunk) (thunk))
(define (yield v)
  (shift k (cons v (lambda () (k #f)))))
(define (init-gen thunk)
  (lambda ()
    (reset (begin (thunk) '()))))

(print-stream 5 (gen->stream (init-gen (from 1))))

; seems to run in constant memory
(print-stream #f (gen->stream (init-gen ones)))

