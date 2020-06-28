; Continuation
(define call/cc call-with-current-continuation)
;(define call/fc call-with-failure-continuation)

(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
; alternative `list'
(define list (lambda x x))

(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda args (f (apply g args))))


(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))


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

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))


(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
; library syntax
;(define (and . lst) (fold && #t lst))
;(define (or . lst) (fold || #f lst))


(define (max first . rest) (foldl (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (foldl (lambda (old new) (if (< old new) old new)) first rest))


(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))


(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) car) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))


(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
;;(define map
;;    (lambda (f lst)
;;        (if (null? lst)
;;            '()
;;            (cons (f (car lst)) (map f (cdr lst))))))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

;; from Common Lisp
(define list-cdr
    (lambda (x)
        (if (null? x)
            '()
            (cdr x))))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k) (car (list-tail lst k)))

; (define (append . lst)
  ; (define append-2
          ; (lambda (inlist alist)
                  ; (foldr (lambda (ap in) (cons ap in)) alist inlist)))
  ; (if (null? lst)
      ; lst
      ; (if (null? (cdr lst))
          ; (car lst)
          ; (foldl (lambda (a b) (append-2 b a)) (car lst) (cdr lst)))))
(define (append . lists) (foldr (lambda (x y) (foldr cons y x)) '() lists))
; (define append
  ; (lambda args
    ; (let f ((ls '()) (args args))
      ; (if (null? args)
          ; ls
          ; (let g ((ls ls))
            ; (if (null? ls)
                ; (f (car args) (cdr args))
                ; (cons (car ls) (g (cdr ls)))))))))


;; I/O Section

(define newline
  (lambda args
    (apply write-char #\newline args)))

(define println
	(lambda (a) (display a)(newline)))

;; Óï·¨¹Ø¼ü×Ö
(define-syntax and
    (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
            (if test1 (and test2 ...) #f))))

;(define-syntax begin
;  (syntax-rules ()
;	((begin exp ...)
;	 ((lambda () exp ...)))))

;(define-syntax let/cc
;  (syntax-rules ()
;    ((let/cc k body) (call/cc (lambda (k) body)))))

