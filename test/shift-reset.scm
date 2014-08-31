; Danvy/Filinski implementation of shift/reset, the Christmas present
; to Scheme48 authors.
;
; $Id$

(define-syntax reset
  (syntax-rules ()
    ((_ ?e ?f ...) (*reset (lambda () ?e ?f ...)))))

(define-syntax shift
  (syntax-rules ()
    ((_ ?k ?e ?f ...) (*shift (lambda (?k) ?e ?f ...)))))

(define *meta-continuation*
  (lambda (v)
    (error "You forgot the top-level reset...")))

(define *abort
  (lambda (thunk)
    ;(with-continuation null-continuation ;JAR hack
      ;(lambda ()
	(let ((val (thunk)))
	  (*meta-continuation* val))));))

(define *reset
  (lambda (thunk)
    (let ((mc *meta-continuation*))
      (call-with-current-continuation
	(lambda (k)
	  (begin
	    (set! *meta-continuation*
		  (lambda (v)
		    (set! *meta-continuation* mc)
		    (k v)))
	    (*abort thunk)))))))

(define *shift
  (lambda (f)
    (call-with-current-continuation
      (lambda (k)
	(*abort (lambda ()
		  (f (lambda (v)
		       (reset (k v))))))))))
