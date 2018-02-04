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
