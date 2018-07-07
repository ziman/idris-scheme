#lang racket

; (rts-unpack list-with-values names-to-bind)
(define-syntax rts-unpack
  (syntax-rules ()
    ((rts-unpack xs () rhs) rhs)
    ((rts-unpack xs (v . vs) rhs)
      (let ((v (vector-car xs)) (rest (vector-cdr xs)))
        (rts-unpack rest vs rhs)))))

; curried lambda
(define-syntax clambda
  (syntax-rules ()
    ((clambda () body) body)
    ((clambda (x . xs) body)
      (lambda (x) (clambda xs body)))))

; Racket-specific implementation below
; if you're using a different Scheme, you need to change what follows

(define (vector-car x)
  (vector-ref x 0))

(define (vector-cdr x)
  (vector-drop x 1))

(define (idris-op-LSystemInfo idx)
  (vector-ref #("c" "os" "target-triple") idx))

(define (cffi-idris_newRef x)
  (box x))

(define (cffi-idris_readRef ref)
  (unbox ref))

(define (cffi-idris_writeRef ref x)
  (set-box! ref x))

(define (cffi-idris_numArgs)
  (+ 1 (vector-length (current-command-line-arguments))))

(define (cffi-idris_getArg i)
  (if (= i 0)
      "this-program"
      (vector-ref
        (current-command-line-arguments)
        (- i 1))))

; Files are mutable cells
(define field-file-object 0)
(define field-file-state 1)
(define field-file-orientation 2)
(define field-file-name 3)

(define (cffi-fileOpen fname mode)
  (with-handlers
    ([exn:fail:filesystem:errno?
       (lambda (e) 'null)])
   (cond
      ((string=? mode "r")
       (vector (open-input-file fname) 'ok 'input fname))
      ((string=? mode "w")
       (vector (open-output-file fname) 'ok 'output fname))
      (else
        (error "unsupported open mode: " mode)))))

(define (cffi-isNull ptr)
  (if (eq? ptr 'null) 1 0))

(define (cffi-fileSize f)
  (file-size (vector-ref f field-file-name)))

(define (cffi-fileEOF f)
  (if (eq? (vector-ref f field-file-state) 'eof) 1 0))

(define (cffi-readChars _world count f)
  (define (chars n)
    (if (= n 0) '()
      (let ((char (read-char (vector-ref f field-file-object))))
        (if (eof-object? char)
          (begin (vector-set! f field-file-state 'eof) '())
          (cons char (chars (- n 1)))))))
  (list->string (chars count)))

(define (cffi-idris_mkFileError _vm)
  #('FileNotFound))

(define (cffi-fileError f)
  (if (eq? (vector-ref f field-file-state) 'ok)
    0
    1))

(define (cffi-fileClose f)
  (cond
    ((eq? (vector-ref f field-file-orientation) 'input)
     (close-input-port (vector-ref f field-file-object)))
    ((eq? (vector-ref f field-file-orientation) 'output)
     (close-output-port (vector-ref f field-file-object)))
    (else
      (error "fclose: unknown file type: " (vector-ref f field-file-orientation)))))

(define (cffi-idris_makeStringBuffer len)
  (box ""))  ; hackity-hack

(define (cffi-idris_addToString buf str)
  (set-box! buf (string-append (unbox buf) str)))

(define (cffi-idris_getString _vm buf)
  (unbox buf))

; a buffer is a vector of numbers in [0,255]
; resize returns a new buffer so we needn't box it
(define (cffi-idris_newBuffer size)
  (make-vector size 0))

(define (idris-extern-prim__registerPtr ptr len)
  ptr)  ; do nothing, all ptrs are GCed, anyway

(define cffi-idris_getBufferSize vector-length)

(define cffi-idris_setBufferByte vector-set!)

(define cffi-idris_getBufferByte vector-ref)

; little endian
(define (cffi-idris_setBufferInt buf idx val)
  (define (loop i x)
    (if (= i 4) 'done
      (begin
        (vector-set! buf (+ idx i) (bitwise-and 255 x))
        (loop (+ i 1) (arithmetic-shift x -8)))))
  (loop 0 val))

; does not include the NUL byte
(define (cffi-idris_setBufferString buf idx s)
  (define (loop i s)
    (if (null? s) 'done
      (begin
        (vector-set! buf i (char->integer (car s)))
        (loop (+ i 1) (cdr s)))))
  (loop idx (string->list s)))
