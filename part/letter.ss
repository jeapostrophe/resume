#lang web-server
(require (only-in (lib "file.ss") make-directory*)
         (lib "date.ss"))
(require web-server/servlet/bindings)
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/pdftk.ss"
         "../lib/resume-data.ss"
         "../lib/resume-data-util.ss"
         "../lib/identity.ss")
(provide (all-defined-out))

(define (thanks-for-the-ref-letter-page)
  (make-output-page
   "Thanks!"
   `((p "We thank you for providing your letter of reference. "
        "You may use the same Reference Code to update the letter "
        "before we begin to evaluate applications."))))

(define (upload-reference-letter refcode)
  (define sanid (refcode-data refcode))
  (define writers-email (applicant-letter-writer-email sanid refcode))
  (define (get-file)
    (let ([r (send/suspend/url
              (lambda (k-url)
                (make-resume-page
                 "Upload Reference Letter"
                 `((p "Reference letter for application by "
                      (span ([id "alert"])
                            ,(applicant-fname sanid)
                            " "
                            ,(applicant-lname sanid))
                      " (" ,(applicant-email sanid) ")"
                      " to the "
                      ,(dept/program-at-univ)
                      ".")
                   ,@(if (file-exists?
                          (applicant/reference-letter sanid refcode))
                         `((p "We already have a reference letter "
                              "uploaded "
                              ,(date->string
                                (seconds->date
                                 (file-or-directory-modify-seconds
                                  (applicant/reference-letter sanid 
                                                              refcode)))
                                #t)
                              "."))
                         `())
                   (form
                    ([action ,(url->string k-url)]
                     [enctype "multipart/form-data"]
                     [method "post"])
                    (p
                     (input ([type "file"]
                             [name "the-file"])))
                    (p (strong "Additional Comments:") (br)
                       (textarea ([name "comments"]
                                  [cols "160"]
                                  [rows "24"])
                                 ,(applicant-letter-writer-comments sanid refcode)))
                    (p
                     (input ([type "submit"]
                             [name "submit"]
                             [value "Upload"]))))
                   (p ([id "imp-note"])
                      "Please submit reference letters in PDF.")))))])
      (redirect/get)
      (set-applicant-letter-writer-comments!
       sanid refcode
       (extract-binding/single 'comments (request-bindings r)))
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
  (let ([f (applicant/reference-letter sanid refcode)])
    (let ([the-file (get-file)])
      (make-directory* (dirname f))
      (with-output-to-file f
        (lambda ()
          (display the-file))
        #:exists 'replace)
      (update-aggregate! sanid #t))
    (confirm
     (lambda ()
       (make-confirmation-page
        "Upload Complete"
        `((p "You have uploaded a document of size "
             ,(number->string (file-size f))
             " bytes.")
          (p "You have uploaded the following additional comments:")
          (pre ,(applicant-letter-writer-comments sanid refcode)))))))
  (let ([appl-name (format "~a ~a"
                           (applicant-fname sanid)
                           (applicant-lname sanid))])
    (send-email
     (list writers-email) null null
     (format "~a: Uploaded Letter for ~a"
             (dept/program-at-univ/short) appl-name)
     `(,(format "Thank you for uploading your letter for ~a's" appl-name)
       ,(format "application to the ~a." (dept/program-at-univ))
       ""
       "If you did not recently upload a letter and are surprised "
       "to receive this message, it may mean your Reference Code "
       "was compromised -- please reply to this message,"
       "quoting it in its entirety."
       ""
       "Thank you for your letter of reference.")))
  (output thanks-for-the-ref-letter-page))

(define (letter-refcode-page init-refcode)
  (lambda (k-url)
    (make-resume-page
     "Provide Reference Code"
     `((p "The email message you received will contain a "
          "Reference Code, a short combination of "
          "letters and numbers such as \""
          (code (span ([id "alert"]) ,sample-refcode))
          "\". Please enter it in the box below.")
       (form ([action ,(url->string k-url)] [method "post"]
                              [id "narrow-query"])
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Reference Code:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "refcode"] [onLoad "this.focus();"]
                                         [value ,init-refcode])))))
             
             (p ([id "submit-button"])
                (input ([type "submit"] [name "enter"] [value "Submit"]))))
       (p ([id "imp-note"])
          "Please submit reference letters in PDF.")))))

(define (get-refcode init-refcode)
  (let ([b
         (request-bindings
          (send/suspend/url
           (letter-refcode-page init-refcode)))])
    (redirect/get)
    (values
     (strip-leading/trailing-space&quote
      (lowercase-string
       (extract-binding/single 'refcode b))))))

(define (no-such-refcode-page refcode)
  (make-confirmation-page
   "No Such Account"
   `((p
      "We were unable to associate a letter request with Reference Code \""
      (code (span ([id "alert"]) ,refcode))
      "\".  Please check and try again."))))

(define (dont-try-sample-refcode-page)
  (make-confirmation-page
   "Please!"
   `((p "You didn't actually think we'd fall for that, did you?"))))

(define (refcode-attempts->-threshold-page)
  (make-output-page
   "Reference Code Entry Threshold Exceeded"
   `((p "You have exceeded the threshold. "
        "If you are a genuine letter writer, and are having trouble, "
        (a ([href "/static/contact.html"])
           "contact us")
        ".")
     (p ([id "imp-note"])
        "We log all failed attempts."))))

(define (refcode-loop)
  (define (the-loop counter bad-tries init-refcode)
    (if (> counter refcode-loop-threshold)
        (begin
          (record-failed-login 'refcode-entry bad-tries)
          (output refcode-attempts->-threshold-page))
        (let-values ([(refcode)
                      (get-refcode init-refcode)])
          (if (or (string=? "" refcode))
              (begin
                (confirm enter-all-fields-page)
                (the-loop counter	; don't penalize a login count
                          bad-tries
                          refcode))
              (if (string=? refcode sample-refcode)
                  (begin
                    (confirm dont-try-sample-refcode-page)
                    (the-loop counter ; don't penalize a login count
                              bad-tries
                              ""))
                  (if (refcode-exists? refcode)
                      refcode
                      (begin
                        (confirm no-such-refcode-page refcode)
                        (the-loop (add1 counter)
                                  (cons (list refcode) bad-tries)
                                  refcode))))))))
  (the-loop 1 '() ""))

(define (refcode-login)
  (make-identity:reference
   (refcode-loop)))
(define refcode-continue upload-reference-letter)