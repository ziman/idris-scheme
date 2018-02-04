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

; Files are singleton list (mutable cells/boxes)
; on EOF, we replace the contents with 'eof
(define (cffi-fileOpen fname mode)
  (cond
    ((string=? mode "r")
     (list (open-input-file fname)))
    ((string=? mode "w")
     (list (open-output-file fname)))
    (else
      (error "unsupported open mode: " mode))))

(define (cffi-isNull f)
  (if (eq? (car f) 'null) 1 0))  ; hackity-hack: if the file does not exist, it'll have crashed on fopen() already

(define (cffi-fileSize f)
  (file-size (car f)))

(define (cffi-fileEOF f)
  (if (eq? (car f) 'eof) 1 0))

(define (cffi-readChars _world count f)
  (define (chars n)
    (if (= n 0) '()
      (let ((char (read-char (car f))))
        (if (eof-object? char)
          (begin (set-car! f 'eof) '())
          (cons char (chars (- n 1)))))))
  (list->string (chars count)))

(define (cffi-fileError f)
  0) ; hackity-hack

(define (cffi-idris_makeStringBuffer len)
  "")  ; hackity-hack
