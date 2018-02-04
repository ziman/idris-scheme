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

; IORefs are represented as singleton lists
(define (cffi-idris_newRef x)
  (list x))

(define (cffi-idris_readRef ref)
  (car ref))

(define (cffi-idris_writeRef ref x)
  (set-car! ref x))


; Chicken Scheme's args don't include argv[0]
(define (cffi-idris_numArgs)
  (+ 1 (length (command-line-arguments))))

(define (cffi-idris_getArg i)
  (list-ref
    (cons "this-program" (command-line-arguments))
    i))

(define (cffi-fileOpen fname mode)
  (cond
    ((string=? mode "r")
     (open-input-file fname))
    ((string=? mode "w")
     (open-output-file fname))
    (else
      (error "unsupported open mode: " mode))))

(define (cffi-isNull f)
  0)  ; hackity-hack: if the file does not exist, it'll crash on open

(define (cffi-fileSize f)
  (file-size f))
