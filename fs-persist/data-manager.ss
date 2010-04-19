#lang web-server
; XXX: You could lock this or put it in a kill-safe thread.
(define-struct file-manager (write current))

(define (file-manager-load read write)
  (make-file-manager write (read)))
(define (file-manager-load/path read write path)
  (file-manager-load (lambda () (read path))
                     (lambda (o) (write path o))))

(define (file-manager-ize/write-op wop)
  (lambda (fm . args)
    (define old (file-manager-current fm))
    (define new (apply wop old args))
    (set-file-manager-current! fm new)
    ((file-manager-write fm) new)))

(define (file-manager-ize/read-op rop)
  (lambda (fm . args)
    (apply rop (file-manager-current fm) args)))

(define (file-manager/parameter f p)
  (lambda args
    (apply f (p) args)))

(provide file-manager?
         file-manager-load
         file-manager-load/path
         file-manager-ize/write-op
         file-manager-ize/read-op
         file-manager/parameter)