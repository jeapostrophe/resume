#lang web-server
(require (only-in (lib "file.ss") make-directory*))  
(require web-server/servlet/bindings)
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/pdftk.ss"
         "../lib/resume-data.ss"
         "../lib/identity.ss")
(provide (all-defined-out))

(define (upload sanid field)
  (define (get-file)
    (let ([r (send/suspend/url
              (lambda (k-url)
                (make-resume-page
                 (string-append "Upload " (field->field-name field))
                 `((form
                    ([action ,(url->string k-url)]
                     [enctype "multipart/form-data"]
                     [method "post"])
                    (p
                     (input ([type "file"]
                             [name "the-file"])))
                    (p
                     (input ([type "submit"]
                             [name "submit"]
                             [value "Upload"]))))
                   (p ([id "imp-note"])
                      "Please don't interrupt the browser while upload is "
                      "in progress.")))))])
      (let ([the-file
             (extract-binding/single 'the-file (request-bindings r))])
        (if (bytes=? #"" the-file)
            (begin
              (confirm empty-upload-page)
              (get-file))
            (if (pdf-document? the-file)
                the-file
                (begin
                  (confirm not-pdf-page)
                  (get-file)))))))
  (let ([f ((field->filename-constructor field) sanid)])
    (let ([the-file (get-file)])
      (make-directory* (dirname f))
      (with-output-to-file/cache f
                                 (lambda ()
                                   (display the-file))
                                 #:exists 'replace)
      (update-aggregate! sanid #t)
      (update-aggregate! sanid #f))
    (confirm
     (lambda ()
       (make-confirmation-page
        "Upload Complete"
        `((p "We have uploaded a document of size "
             ,(number->string (file-size f))
             " bytes."))))))
  (redirect/get)
  (present-application-console sanid))

(define (edit-letter-writers sanid)
  (let-values ([(n i e) (get-letter-writers sanid)])
    (let ([refcode (make-applicant-letter-writer! sanid n i e)])
      (email-refcode sanid refcode)))
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      "References Recorded"
      `((p "We have recorded your references.")
        (form
         ([method "post"]
          [action ,(url->string
                    (embed/url 
                    (lambda _
                      (redirect/get)
                      (edit-letter-writers sanid))))])
         (input ([id "proceed-button"]
                 [type "submit"] [name "enter"] [value "Add Another Reference"])))
        (form
         ([method "post"]
          [action ,(url->string
                    (embed/url 
                    (lambda _
                      (redirect/get)
                      (present-application-console sanid))))])
         (input ([id "proceed-button"]
                 [type "submit"] [name "enter"] [value "Return to Console"]))))))))

(define (edit-info page-generator
                   info-writer
                   confirmation-title
                   confirmation-body)
  (lambda (sanid)
    (let ([b
           (request-bindings (send/suspend (page-generator sanid)))])
      (info-writer sanid b))
    (confirm
     (lambda ()
       (make-confirmation-page
        confirmation-title
        confirmation-body)))
    (redirect/get)
    (present-application-console sanid)))

(define edit-contact-info
  (edit-info get-contact-info-page write-applicant-contact-info!
             "Modification Complete"
             `((p "We have recorded your contact information."))))

(define (present-application-console sanid)
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      (format "Application Console: ~a" sanid)
      `((p 
         (table ([id "console-table"])
                (tr
                 (th ([id "action-column"]) "Action")
                 (th nbsp "-" nbsp)
                 (th ([id "status-column"]) "Status"))
                (tr
                 (td ([id "console-action"])
                     (a ([href ,(url->string (embed/url (lambda _ (edit-contact-info sanid))))])
                        "Edit Contact Information")))
                ,@(map (lambda (field)
                         `(tr
                           (td ([id "console-action"])
                               (a ([href ,(url->string
                                           (embed/url
                                           (lambda _ (upload sanid field))))])
                                  ,(string-append
                                    "Edit "
                                    (field->field-name field))))
                           (td nbsp "-" nbsp)
                           (td ,(last-update sanid field #t))))
                       '(cover-letter cv research-stmt teaching-stmt))
                (tr
                 (td ([id "console-action"])
                     (a ([href ,(url->string (embed/url (lambda _ (edit-letter-writers sanid))))])
                        "Edit Letter Writers"))
                 (td nbsp "-" nbsp)
                 (td nbsp))))
        (p
         (center (a ([href "/static/im-done.html"]) "Application Complete?")))
        (p ([id "note"])
           "Your session will time out after a period "
           "of inactivity. Don't worry" 151 "your data are safe! "))))))

(define (apply-login)
  (let-values ([(id sanid password)
                (login-loop 'applicant-login
                            applicant-exists?
                            applicant-email
                            applicant-password
                            set-applicant-password!)])
    (make-identity:applicant
     sanid)))