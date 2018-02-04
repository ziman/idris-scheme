; (rts-unpack list-with-values names-to-bind)
(define-syntax rts-unpack
  (syntax-rules ()
    ((rts-unpack xs () rhs) rhs)
    ((rts-unpack xs (v . vs) rhs)
      (let ((v (car xs)) (rest (cdr xs)))
        (rts-unpack rest vs rhs)))))

; curried lambda
(define-syntax clambda
  (syntax-rules ()
    ((clambda () body) body)
    ((clambda (x . xs) body)
      (lambda (x) (clambda xs body)))))

; Chicken-Scheme-specific implementation below
; if you're using a different Scheme, you need to change what follows

(require "posix")

(define (rts-num-args)
  (+ 1 (length (command-line-arguments))))

(define (rts-get-arg i)
  (list-ref
    (cons "this-program" (command-line-arguments))
    i))

(define (rts-file-open fname mode)
  (cond
    ((string=? mode "r")
     (open-input-file fname))
    ((string=? mode "w")
     (open-output-file fname))
    (else
      (error "unsupported open mode: " mode))))
