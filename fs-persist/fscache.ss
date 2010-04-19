#lang web-server
(require (lib "plt-match.ss")
         (lib "list.ss")
         (lib "etc.ss")
         (lib "file.ss"))
(provide fscache?
         (rename-out [ext:make-fscache make-fscache])
         make-with-input-from-file
         make-with-output-to-file)

(define-struct fscache (channel manager))

(define-struct request (reply-channel))
(define-struct (request:lookup request) (path))
(define-struct (request:replace request) (path new-bytes))
(define-struct (request:prune request) (proc))

(define-struct return ())
(define-struct (failure return) (exn))
(define-struct (success return) (value))
(define return-value
  (match-lambda
    [(struct success (v))
     v]
    [(struct failure (e))
     (raise e)]))

; ext:make-fscache : -> fscache?
(define (ext:make-fscache)
  (define cache-table
    (make-hash-table 'equal))
  (define (lookup p)
    (define last-mod
      (file-or-directory-modify-seconds p))
    (define cur
      (hash-table-get cache-table p
                      (lambda ()
                        (define new
                          (list (file-or-directory-modify-seconds p)
                                (with-input-from-file p
                                  (lambda ()
                                    (read-bytes (file-size p))))))
                        (hash-table-put! cache-table p new)
                        new)))
    (if (> last-mod (first cur))
        (begin (hash-table-remove! cache-table p)
               (lookup p))
        cur))
  (define (replace p nb)
    (hash-table-remove! cache-table p)
    (with-output-to-file p (lambda () (display nb)) 'truncate)
    #t)
  (define request-channel (make-channel))
  (define manager
    (thread/suspend-to-kill
     (lambda ()
       (let loop ()
         (sync
          (handle-evt
           request-channel
           (match-lambda
             [(struct request:lookup (rc p))
              (with-handlers ([exn:fail? (lambda (e)
                                      (channel-put rc (make-failure e)))])
                (channel-put rc (make-success (second (lookup p)))))
              (loop)]
             [(struct request:replace (rc p nb))
              (with-handlers ([exn:fail? (lambda (e)
                                      (channel-put rc (make-failure e)))])
                (channel-put rc (make-success (replace p nb))))
              (loop)])))))))
  (make-fscache request-channel manager))

; fscache-lookup : fscache? path? -> bytes?
(define (fscache-lookup an-fscache path)
  (define reply-channel (make-channel))
  (thread-resume (fscache-manager an-fscache))
  (channel-put (fscache-channel an-fscache)
               (make-request:lookup reply-channel path))
  (return-value (channel-get reply-channel)))

; fscache-replace : fscache? path? bytes? -> void
(define (fscache-replace an-fscache path bytes)
  (define reply-channel (make-channel))
  (thread-resume (fscache-manager an-fscache))
  (channel-put (fscache-channel an-fscache)
               (make-request:replace reply-channel path bytes))
  (return-value (channel-get reply-channel))
  (void))

; make-with-input-from-file : fscache? -> path? (-> alpha) [ignored] -> alpha
(define ((make-with-input-from-file an-fscache) path thunk . wiff-args)
  (define input
    (open-input-bytes (fscache-lookup an-fscache (normalize-path path))))
  (parameterize ([current-input-port input])
    (thunk)))

; make-with-output-to-file : fscache? -> path? (-> alpha) [ignored] -> alpha
; XXX: Should enforce that you are not using 'append, or implement it correctly.
(define ((make-with-output-to-file an-fscache) path thunk . wotf-args)
  (define output-bytes (open-output-bytes))
  ; Not in tail-position. This seems reasonable, however.
  (begin0
    (parameterize ([current-output-port output-bytes])
      (thunk))
    (fscache-replace an-fscache
                     (normalize-path path)
                     (get-output-bytes output-bytes))))

(define (test)
  (define an-fscache (ext:make-fscache))
  (define with-input-from-file (make-with-input-from-file an-fscache))
  (define with-output-to-file (make-with-output-to-file an-fscache))
  (define path "/tmp/blahblahblah")
  (with-output-to-file path
    (lambda () (write #t)))
  (build-list 100
              (lambda (i)
                (with-input-from-file path read)))
  (read)
  (build-list 100
              (lambda (i)
                (with-input-from-file path read)))
  (void))