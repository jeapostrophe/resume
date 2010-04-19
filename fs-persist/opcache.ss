#lang web-server
(require (lib "list.ss"))
(provide (all-defined-out))

; cache-op : proc? path? -> proc?
(define (cache-op op cache-path)
  (define the-ht (make-hash-table 'equal))
  (define (write-cache!)
    (with-output-to-file cache-path
      (lambda ()
        (write (hash-table-map the-ht list)))
      'truncate/replace))
  (for-each (lambda (k+v)
              (hash-table-put! the-ht (first k+v) (second k+v)))
            (if (file-exists? cache-path)
                (with-input-from-file cache-path read)
                empty))                    
  (lambda key
    (hash-table-get the-ht key
                    (lambda ()
                      (define value (apply op key))
                      (hash-table-put! the-ht key value)
                      (write-cache!)
                      value))))