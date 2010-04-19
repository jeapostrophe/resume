#lang web-server
(require (lib "file.ss")
         (lib "list.ss"))
(require "fscache.ss")
(provide read-path
         read-path/default
         write-path
         bind-to-file
         bind-within-directory)

;; Utility
(define (dirname p)
  (apply build-path
         (reverse (rest (reverse (explode-path (build-path p)))))))

;; Caching
(define cache (make-fscache))
(define with-input-from-file/cache (make-with-input-from-file cache))
(define with-output-to-file/cache (make-with-output-to-file cache))

;; Generators
(define (read-path p)
  (with-input-from-file/cache p read))
(define (read-path/default p fail-thunk)
  (with-handlers ([exn:fail? (lambda (e) (fail-thunk))])
    (with-input-from-file/cache p read)))  
(define (write-path p v)
  (make-directory* (dirname p))
  (with-output-to-file/cache p (lambda () (write v)) 'truncate))
(define (bind-to-file path-f default-t)
  (values (lambda args
            (read-path/default
             (apply path-f args)
             default-t))
          (lambda args
            (define new (first (reverse args)))
            (define pargs (reverse (rest (reverse args))))
            (write-path
             (apply path-f pargs)
             new))))
(define (bind-within-directory directory-fn)
  (define (item-dir . args)
    (define new (first (reverse args)))
    (define pargs (reverse (rest (reverse args))))
    (string-append (apply directory-fn pargs) new "/"))
  (values directory-fn
          (lambda args
            (map path->string 
                 (with-handlers ([exn:fail? (lambda _ empty)])
                   (directory-list (apply directory-fn args)))))
          item-dir
          (lambda args
            (directory-exists? (apply item-dir args)))))