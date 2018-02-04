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

(define box-new list)
(define box-get car)
(define box-get-extra cadr)
(define box-get-exxtra caddr)
(define box-set! set-car!)
(define (box-set-extra! box x) (set-car! (cdr box) x))
(define (box-set-exxtra! box x) (set-car! (cddr box) x))

; IORefs are represented as singleton lists
(define (cffi-idris_newRef x)
  (box-new x))

(define (cffi-idris_readRef ref)
  (box-get ref))

(define (cffi-idris_writeRef ref x)
  (box-set! ref x))

; Chicken Scheme's args don't include argv[0]
(define (cffi-idris_numArgs)
  (+ 1 (length (command-line-arguments))))

(define (cffi-idris_getArg i)
  (list-ref
    (cons "this-program" (command-line-arguments))
    i))

; Files are mutable cells
;
(define (cffi-fileOpen fname mode)
  (cond
    ((string=? mode "r")
     (box-new (open-input-file fname) 'ok 'input))
    ((string=? mode "w")
     (box-new (open-output-file fname) 'ok 'output))
    (else
      (error "unsupported open mode: " mode))))

(define (cffi-isNull ptr)
  (if (eq? ptr 'null) 1 0))  ; hackity-hack: if the file does not exist, it'll have crashed on fopen() already

(define (cffi-fileSize f)
  (file-size (box-get f)))

(define (cffi-fileEOF f)
  (if (eq? (box-get-extra f) 'eof) 1 0))

(define (cffi-readChars _world count f)
  (define (chars n)
    (if (= n 0) '()
      (let ((char (read-char (box-get f))))
        (if (eof-object? char)
          (begin (box-set-extra! f 'eof) '())
          (cons char (chars (- n 1)))))))
  (list->string (chars count)))

(define (cffi-fileError f)
  (if (eq? (box-get-extra f) 'ok)
    0
    1)) ; hackity-hack

(define (cffi-fileClose f)
  (cond
    ((eq? (box-get-exxtra f) 'input)
     (close-input-port (box-get f)))
    ((eq? (box-get-exxtra f) 'output)
     (close-output-port (box-get f)))
    (else
      (error "fclose: unknown file type: " (box-get-exxtra f)))))

(define (cffi-idris_makeStringBuffer len)
  (box-new ""))  ; hackity-hack

(define (cffi-idris_addToString buf str)
  (box-set! buf (string-append (box-get buf) str)))

(define (cffi-idris_getString _vm buf)
  (box-get buf))
