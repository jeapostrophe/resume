#lang web-server
(require (lib "etc.ss")
         (lib "list.ss")
         (lib "kw.ss")
         (lib "plt-match.ss")
         (lib "etc.ss"))
(require web-server/servlet/bindings)
(provide (all-defined-out))

(define (generate-names l)
  (map symbol->string (map (lambda _ (gensym)) l)))

(define (generate-form:strings specs)
  (define names (generate-names specs))
  (define (form r)
    (define bs (request-bindings r))
    (for-each (match-lambda*
                [(list name (vector _l _g setter!))
                 (define nv (extract-binding/single (string->symbol name) bs))
                 (setter! nv)])
              names
              specs))
  (define (page embed/url)
    `(table
      ,@(map (match-lambda*
               [(list name (vector label getter _s))
                `(tr (td ([id "q-and-a-q"]) ,label)
                     (td ([id "q-and-a-a"]) (input ([type "text"] [size "40"] [name ,name] [value ,(getter)]))))])
             names
             specs)))
  (values form page))

(define (generate-list-form getter setter!
                            #:value=? [value=? equal?]
                            #:deleted! [deleted! (lambda (old-value) (void))]
                            #:editted! [editted! (lambda (old-value new-value) (void))]
                            #:default [default  ""]
                            #:add-default-okay? [add-default-okay? #f]
                            #:display [display
                                       (lambda (name value)
                                         `(input ([type "text"]
                                                  [name ,name]
                                                  [value ,value])))]
                            #:parse
                            [parse
                             (lambda (name bs)
                               (extract-binding/single (string->symbol name) bs))])
  (define id (symbol->string (gensym)))
  (define add-name (format "~a-add" id))
  (define edit-name (format "~a-edit" id))
  (define new-name (format "~a-new" id))
  (define (names cur)
    (map (lambda (i)
           (format "~a-~a" id i))
         (build-list (length cur)
                     number->string)))
  (define (delete return cur deleted-name)
    (setter!
     (map cdr
          (filter (match-lambda
                    [(list-rest name v)
                     (if (equal? name deleted-name)
                         (begin (deleted! v)
                                #f)
                         #t)])
                  (map cons (names cur) cur))))
    (return))
  (define (handle-submit r)
    (define cur (getter))
    (define bs (request-bindings r))
    (when (exists-binding? (string->symbol edit-name) bs)
      (setter!
       (map (lambda (n ov)
              (define nv (parse n bs))
              (if (value=? ov nv)
                  ov
                  (begin (editted! ov nv)
                         nv)))
            (names cur)
            cur)))
    (when (exists-binding? (string->symbol add-name) bs)
      (let ([nv (parse new-name bs)])
        (when (or add-default-okay?
                  (not (value=? default nv)))              
          (setter! (list* nv cur)))))
    (void))
  (values handle-submit
          (lambda (embed/url)
            (define (generate k)
              (define cur (getter))
              (k `(table
                   ,@(map (lambda (name v)
                            `(tr (td ,(display name v))
                                 (td (a ([href ,(url->string
                                                 (embed/url
                                                 (lambda _ 
                                                   (delete (lambda () (generate k))
                                                           cur name))))])
                                        "delete"))))
                          (names cur)
                          cur)
                   ,@(if (empty? cur)
                         `()
                         `((tr (td ([colspan "2"] [align "right"])
                                   (input ([type "submit"] [name ,edit-name] [value "Save"]))))))
                   (tr (td ,(display new-name default))
                       (td (input ([type "submit"] [name ,add-name] [value "Add"])))))))
            (let/cc k
              (generate k)))))