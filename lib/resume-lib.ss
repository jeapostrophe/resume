#lang web-server
(require (lib "date.ss")
         (lib "smtp.ss" "net")
         (lib "mzssl.ss" "openssl")
         (only-in (lib "file.ss") make-directory*)
         (lib "head.ss" "net")
         (lib "list.ss"))
(require web-server/servlet/bindings)
(require "resume-data.ss"
         "resume-data-util.ss"
         "util.ss")
(provide (all-defined-out))

(define (login-loop logging-context 
                    person-exists-check
                    person-email
                    person-password
                    set-password!)
  (define counter (box 1))
  (let the-loop ([bad-tries empty]
                 [init-id ""]
                 [init-password ""])
    (if (> (unbox counter) login-loop-threshold)
        (begin
          (record-failed-login logging-context bad-tries)
          (output login->-threshold-page))
        (let-values ([(id password forgot-password? set?)
                      (get-id/password init-id init-password)])
          (if (or (string=? "" id)
                  (and (not (or set? forgot-password?))
                       (string=? "" password)))
              (begin
                (confirm enter-all-fields-page)
                (the-loop bad-tries
                          id
                          password))
              (let ([sanid (sanitize-string id)])
                (if (person-exists-check sanid)
                    (let ([the-password (person-password sanid)])
                      (if (and the-password (string=? the-password password))
                          (values id sanid password)
                          (begin
                            (if (or (not the-password)
                                    set?
                                    forgot-password?)
                                (begin (forgot-password-page set? person-email id)
                                       (reset-password-page set? set-password! id))
                                (confirm incorrect-password-page id))
                            (set-box! counter (add1 (unbox counter)))
                            (the-loop (cons (list id password) bad-tries)
                                      id
                                      password))))
                    (begin
                      (confirm no-such-account-page id)
                      (set-box! counter (add1 (unbox counter)))
                      (the-loop (cons (list id password) bad-tries)
                                id password)))))))))

(define (name-id-password/2-page set? init-password init-password2)
  (lambda (k-url)
    (make-resume-page
     (string-append
      (if set?
          "Set"
          "Reset")
      " Password")
     `((form ([action ,(url->string k-url)] [method "post"] [id "narrow-query"])
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your desired password:")
               (td ([id "q-and-a-a"])
                   (input ([type "password"] [name "password"]
                                             [onLoad "this.focus();"]
                                             [value ,init-password]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your password again:")
               (td ([id "q-and-a-a"])
                   (input ([type "password"] [name "password2"]
                                             [value ,init-password2])))))
             (p ([id "submit-button"])
                (input ([type "submit"] [name "enter"] [value "Submit"])))))))) 

(define (get-name/id/password/2
         set?
         init-password init-password2)
  (let ([b (request-bindings
            (send/suspend/url
             (name-id-password/2-page set? init-password init-password2)))])
    (redirect/get)
    (values (extract-binding/single 'password b)
            (extract-binding/single 'password2 b))))

(define (get-reset-password-loop set?)
  (let the-loop ([bad-tries empty]
                 [init-password ""]
                 [init-password2 ""])
    (let-values ([(password password2)
                  (get-name/id/password/2
                   set?
                   init-password init-password2)])
      (if (or (string=? "" password) (string=? "" password2))
          (begin
            (confirm enter-all-fields-page)
            (the-loop bad-tries
                      password password2))
          (if (string=? password password2)
              (values password)
              (begin
                (confirm passwords-not-equal-page)
                (the-loop (cons (list password password2)
                                bad-tries)
                          password password2)))))))

(define (reset-password-page set? set-password! id)
  (let-values ([(password) (get-reset-password-loop set?)])
    (set-password! id password)
    password))

(define (login-page init-id init-password)
  (lambda (k-url)
    (make-resume-page
     "Login"
     `((form ([action ,(url->string k-url)] [method "post"] [id "narrow-query"])
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Site Username:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "id"] [onLoad "this.focus();"] [value ,init-id]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Password:")
               (td ([id "q-and-a-a"])
                   (input ([type "password"] [name "password"] 
                                             [value ,init-password]))))
              (tr
               (td ([id "q-and-a-q"])
                   (input ([type "submit"] [name "enter"] [value "Submit"])))
               (td ([id "q-and-a-a"])
                   (input ([type "submit"] [name "reset"] [value "Reset password"]))
                   (br)
                   (input ([type "submit"] [name "set"] [value "Set initial password"]))))))))))

(define (get-id/password init-id init-password)
  (let ([b
         (request-bindings
          (send/suspend/url
           (login-page init-id init-password)))])
    (redirect/get)
    (values
     (extract-binding/single 'id b)
     (extract-binding/single 'password b)
     (exists-binding? 'reset b)
     (exists-binding? 'set b))))

(define (forgot-password-page set? person-email id)
  (send/suspend/url
   (lambda (k-url)
     (send-email 
      (list (person-email id)) '() '()
      (string-append (dept/program-at-univ/short)
                     " Job Application: "
                     (if set?
                         "Set"
                         "Reset")
                     " Password")
      `("A request to change the password of the account associated "
        "with your email address has been requested at "
        ,(format "at the ~a." (dept/program-at-univ))
        ""
        "If you intended this, please visit the URL"
        ""
        ,(format "~a~a" (host/portname) (url->string k-url))
        ""
        "to reset the password."
        ""
        "If you did not initiate this application, please notify us immediately "
        "and we will investigate."
        ""
        "Thank you for your application."))
     (output/non-terminal
      (lambda ()
        (lambda (ignore-k-url)
          (make-output-page
           (string-append
            (if set?
                "Set"
                "Reset")
            " Password")
           `((p "We have sent a link to the address in our system. "
                "This link allows you to set your password. "
                "If this address is incorrect, please email the "
                "site administrators. If you do not receive email "
                "in the next few minutes, "
                "there may be a transient network problem" 151
                "please wait and try again later.")
             (p  "The link sent to you will expire after a few "
                 "hours, so please check your email promptly.")))))))))

(define (email-refcode sanid refcode)
  (define name (applicant-letter-writer-name sanid refcode))
  (define institution (applicant-letter-writer-institution sanid refcode))
  (define email (applicant-letter-writer-email sanid refcode))
  (let ([app-name (format "~a ~a"
                          (applicant-fname sanid)
                          (applicant-lname sanid))]
        [app-email (applicant-email sanid)])
    (send-email 
     (list email) null null
     (format "~a: Requesting Letter for ~a"
             (dept/program-at-univ/short) app-name)
     `("Dear Referee:"
       ""
       "We have received a job application from"
       ,(format "  name: ~a~n  email: ~a" app-name app-email)
       "who has listed you as a reference with the following data:"
       ,(format "      name: ~a~n  email: ~a~n  institution(s): ~a" 
                name email institution)
       ""
       "If you have been listed in error or have reason to suspect mischief, please reply to this message, quoting it in its entirety."
       ""
       "To upload your letter, you will need the following"
       "Reference Code:"
       ,(format "      \"~a\"  [sans quotes]" refcode)
       ,(format "      (~a)" (military-alphabet refcode))
       "which should not be revealed to the candidate."
       ""
       "To upload your letter, please visit the URL"
       ,(url/letter-submission)
       "And click the link for `Letter Writers'. In the box, please enter your Reference Code."
       ""
       "N.B.: We request that you submit letters in PDF.  This enables us to create internal electronic dossiers on each candidate."
       ""
       "Thank you for your time and assistance."))))

(define (incorrect-password-page id)
  (make-confirmation-page
   "Incorrect Password"
   `((p
      "The password you supplied for \""
      (span ([id "alert"]) ,id)
      "\" was incorrect (or perhaps you have the "
      "wrong username).  Please try again."))))

(define (no-such-account-page id)
  (make-confirmation-page
   "No Such Account"
   `((p
      "We were unable to associate an account with \""
      (span ([id "alert"]) ,id)
      "\".  Please try again."))))

(define (login->-threshold-page)
  (make-output-page
   "Login Threshold Exceeded"
   `((p "You have exceeded the login threshold. "
        "If you are genuinely having trouble, "
        (a ([href "/static/contact.html"])
           "contact us")
        ".")
     (p ([id "imp-note"])
        "We log all failed attempts."))))

(define (passwords-not-equal-page)
  (make-confirmation-page
   "Passwords Not Equal"
   `((p "You failed to enter the same password twice. "
        "Please make sure you enter the same password in "
        "both boxes."))))

(define (make-resume-page title content-list)
  `(html
    (head
     (title "Job Application: " ,title)
     (style ([type "text/css"])
            "@import \"/static/resume.css\";"))
    (body
     (h1 ([id "banner"]) ,title)
     ,@content-list
     (hr ([noshade "noshade"]
          [size "1"]))
     (p ([id "footer"])
        "Powered by "
        (a ([href "http://www.plt-scheme.org/"])
           "PLT Scheme")
        ". Comments?  Problems? "
        (a ([href "/static/contact.html"])
           "Contact us")
        "."))))

(define (make-confirmation-page title material)
  (lambda (k-url)
    (make-resume-page
     title
     `((form
        ([action ,(url->string k-url)] [method "post"])
        ,@material
        (input ([id "proceed-button"]
                [type "submit"] [name "enter"] [value "Proceed"])))))))

(define (confirm page . args)
  (send/suspend/url (apply page args)))

(define (make-output-page title material)
  (make-resume-page
   title
   material))

(define (output/non-terminal page . args)
  (send/suspend/url (apply page args)))

(define (output page . args)
  (send/suspend/url (apply page args)))

(define login-loop-threshold 3)
(define refcode-loop-threshold 5)

(define (any-files? sanid)
  (ormap file-exists?
         (map (lambda (field)
                ((field->filename-constructor
                  field)
                 sanid))
              `(cover-letter cv research-stmt teaching-stmt))))
(define (applicant/material-link sanid field label)
  (let ([f ((field->filename-constructor
             field)
            sanid)])
    (if (file-exists? f)
        `(a ([href
              ,(internal-path->url
                (app-doc-/url-constructor
                 sanid field))])
            ,label)
        "-")))
(define (applicant/aggregate-letters-link sanid)
  (if (any-files? sanid)
      `(a ([href ,(internal-path->url (applicant/aggregate-letters sanid))])
          "Aggregate")
      'nbsp))
(define (applicant/aggregate-link sanid)
  (if (any-files? sanid)
      `(a ([href ,(internal-path->url (applicant/aggregate sanid))])
          "Aggregate")
      'nbsp))

(define (contact-info-table sanid)
  `(table ([class "contact-info-list"])
          (tr
           (td ([class "category"])
               "Email:")
           (td ([class "response"])
               (a ([href ,(format "mailto:~a" (applicant-email sanid))])
                  ,(applicant-email sanid))))
          (tr
           (td ([class "category"])
               "Work Phone:")
           (td ([class "response"])
               ,(applicant-work-phone sanid)))
          (tr
           (td ([class "category"])
               "Home Phone:")
           (td ([class "response"])
               ,(applicant-home-phone sanid)))
          (tr
           (td ([class "category"])
               "Mobile Phone:")
           (td ([class "response"])
               ,(applicant-mobile-phone sanid)))
          (tr
           (td ([class "category"])
               "Postal Address:")
           (td ([class "response"])
               (pre ,(applicant-postal sanid))))))

(define (get-contact-info-page sanid)
  (lambda (k-url)
    (make-resume-page
     "Contact Information"
     `((form ([action ,(url->string k-url)] [method "post"]
                              [id "wide-query"])
             (p ([id "note"])
                "Why do we have two different Web page boxes? "
                "The first is for your professional home page, "
                "while the second" 151 "which may reside on a "
                "different server" 151 "is for supplementary "
                "documents such as papers.  If your supplements "
                "are accessible from the first page, "
                "or you have no supplements, "
                "you can leave the second field blank.")
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Email Address:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "email"] [size "80"] [onLoad "this.focus();"]
                                         [value ,(applicant-email sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Home Web Page:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "web-home"] [size "80"]
                                         [value ,(applicant-web-home sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Application Web Page:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "web-app"] [size "80"]
                                         [value ,(applicant-web-app sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Work Phone:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "work-phone"] [size "20"]
                                         [value ,(applicant-work-phone sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Home Phone:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "home-phone"] [size "20"]
                                         [value ,(applicant-home-phone sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Mobile Phone:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "mobile-phone"] [size "20"]
                                         [value ,(applicant-mobile-phone sanid)]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Postal Address:")
               (td ([id "q-and-a-a"])
                   (textarea ([rows "6"]
                              [cols "80"]
                              [wrap "off"]
                              [name "postal"])
                             ,(applicant-postal sanid)))))
             (p ([id "note"])
                "Please fill in as many fields as possible, "
                "except of course those you prefer to keep private "
                "(such as your home and mobile "
                "phone numbers).")
             (p ([id "submit-button"])
                (input ([type "submit"]
                        [name "enter"]
                        [value "Submit"]))))))))

(define (lookup-contact-field contact-info field)
  (cadr (assoc field contact-info)))

(define (applicant-rating/average sanid)
  (let ([reviewers (filter (lambda (csid)
                             (review-score-exists? sanid csid))
                           (read-reviewers sanid))])
    (if (zero? (length reviewers))
        #f
        (round-to 
         (/ (apply + (map (lambda (r) (string->number (applicant-review-score sanid r)))
                          reviewers))
            (length reviewers))
         1))))

(define (email-letter-writers sanid bindings)
  'do-nothing)

(define (record-failed-login locus new-entry)
  (make-directory* (dirname (system/log/failed-login)))
  (with-output-to-file (system/log/failed-login)
    (lambda ()
      (write (cons (date->string (seconds->date (current-seconds)) #t)
                   (cons locus
                         new-entry)))
      (newline))
    #:exists 'append))

(define (enter-all-fields-page)
  (make-confirmation-page
   "Enter All Fields"
   `((p "Please don't leave any of the fields blank."))))

(define (not-pdf-page)
  (make-confirmation-page
   "Please Use PDF"
   `((p "Your document does not appear to be in PDF. "
        "We accept only PDF documents."))))

(define (empty-upload-page)
  (make-confirmation-page
   "Empty Upload"
   `((p "Your uploaded document was empty. "
        "This problem is caused on some browsers when hitting "
        "Return instead of the Spacebar to select a file for "
        "upload."))))

(define (last-update sanid field show-size?)
  (define (update-for-file filename-constructor)
    (let ([f (filename-constructor sanid)])
      (if (file-exists? f)
          (string-append
           "Modified "
           (date->string
            (seconds->date
             (file-or-directory-modify-seconds f))
            #t)
           (if show-size?
               (string-append " ("
                              (number->string (file-size f))
                              " bytes)")
               ""))
          "not yet uploaded")))
  (update-for-file (field->filename-constructor field)))

(define (get-letter-writers sanid)
  (define (get-letter-writers-page init-name init-inst init-email)
    (lambda (k-url)
      (define current-letter-writers (applicant-letter-writers sanid))
      (make-resume-page
       "Letter Writers"
       `((h3 ([id "mini-banner"])
             "Already Requested")
         ,(if (null? current-letter-writers)
              `(p "None.")
              `(p
                (table ([id "letter-writers"])
                       (tr
                        (th "Name") (th "Institution(s)") (th "Email"))
                       ,@(map (lambda (refcode)
                                `(tr (td ,(applicant-letter-writer-name sanid refcode))
                                     (td ,(applicant-letter-writer-institution sanid refcode))
                                     (td ,(applicant-letter-writer-email sanid refcode))))
                              current-letter-writers))))
         (h3 ([id "mini-banner"])
             "Additions")
         (p "Please enter information about your letter writers "
            "one at a time.")
         (p ([id "imp-note"])
            "The information you enter below generates an "
            "automatic letter request by email.  Because we "
            "cannot undo this operation, you can only add "
            "information" 151 "you " (strong "cannot") " modify, or delete, data entered. "
            "Please test the email address before entering it.")
         (p "Please try to use the official work addresses "
            "of your letter writers, so we don't need to further "
            "authenticate their identity.")
         (form ([action ,(url->string k-url)] [method "post"] [id "narrow-query"])
               (table
                (tr
                 (td ([id "q-and-a-q"])
                     "Name:")
                 (td ([id "q-and-a-a"])
                     (input ([type "text"] [name "name"] [value ,init-name]))))
                (tr
                 (td ([id "q-and-a-q"])
                     "Institution(s):")
                 (td ([id "q-and-a-a"])
                     (input ([type "text"] [name "inst"]
                                           [value ,init-inst]))))
                (tr
                 (td ([id "q-and-a-q"])
                     "Email Address:")
                 (td ([id "q-and-a-a"])
                     (input ([type "text"] [name "email"]
                                           [value ,init-email])))))
               (p ([id "submit-button"])
                  (input ([type "submit"]
                          [name "enter"]
                          [value "Submit"]))))
         (p ([id "note"])
            "You should provide a "
            (span ([id "alert"]) "minimum of three")
            " references.  We strongly prefer that you have "
            "no more than "
            (span ([id "alert"]) "six")
            ".")))))
  (let loop ([init-name ""] 
             [init-inst ""]
             [init-email ""])
    (let* ([b (request-bindings
               (send/suspend/url
                (get-letter-writers-page init-name init-inst init-email)))]
           [n (extract-binding/single 'name b)]
           [i (extract-binding/single 'inst b)]
           [e (extract-binding/single 'email b)])
      (cond
        [(or (string=? n "") (string=? i "") (string=? e ""))
         (begin
           (confirm enter-all-fields-page)
           (loop n i e))]
        [(no-@-in-email? e)
         (begin
           (confirm no-@-in-email-page)
           (loop n i e))]
        [else
         (values n i e)]))))

(define (send-email to-list cc-list bcc-list subject body)
  (if (string=? "" (smtp/port))
      (smtp-send-message
       (smtp/server)
       (dept/contact-email)
       (append to-list cc-list bcc-list)
       (standard-message-header
        (dept/contact-email)
        to-list
        cc-list
        bcc-list
        subject)
       (append body '("") (dept/signature)))
      (smtp-send-message
       (smtp/server)
       (dept/contact-email)
       (append to-list cc-list bcc-list)
       (standard-message-header
        (dept/contact-email)
        to-list
        cc-list
        bcc-list
        subject)
       (append body '("") (dept/signature))
       #:port-no (string->number (smtp/port))
       #:tcp-connect ssl-connect
       #:auth-user (smtp/username)
       #:auth-passwd (smtp/password))))

(define (handle-all-errors invoking-application)
  (lambda (exn)
    ((error-display-handler) (exn-message exn) exn)
    (make-directory* (dirname (system/log/fatal-error)))
    (with-output-to-file (system/log/fatal-error)
      (lambda ()
        (write `(,(date->string (seconds->date (current-seconds)) #t)
                 ,invoking-application
                 ,(exn-message exn)))
        (newline))
      #:exists 'append)
    (output/non-terminal
     (lambda ()
       (lambda (k-url)
         (make-output-page
          "Error"
          '((p "The system has encountered an error. "
               "We apologize for the inconvenience. ")
            (p "If you have a moment, please "
               (a ([href "/static/contact.html"])
                  "tell us")
               " what you were trying to do. We have recorded "
               "some information in an error log, and with your "
               "help we can identify the cause of the problem.")
            (p "Once again, sorry for the inconvenience."))))))))

(define (no-@-in-email-page)
  (make-confirmation-page
   "Please Provide a Legal Email Address"
   `((p "The address you provided does not contain an `@'."))))

(define (html:span:applicant-name sanid name-link)
  `(span ([class "applicant-name"])
         (a ([href ,name-link])              
            ,(applicant-fname sanid)
            " "
            ,(applicant-lname sanid))
         (br)
         (span ([class "applicant-url"])
               (a ([href ,(format "mailto:~a"
                                  (applicant-email sanid))])
                  (img ([src "/static/images/16x16/mail-message-new.png"]
                        [width "12px"]
                        [height "12px"]))))
         " "
         (span ([class "applicant-url"])
               (a ([href ,(format "http://images.google.com/images?q=\"~a ~a\""
                                  (applicant-fname sanid)
                                  (applicant-lname sanid))]
                   [target "_new"])
                  (img ([src "/static/images/16x16/image-x-generic.png"]
                        [width "12px"]
                        [height "12px"]))))
         " "
         ,(if (string=? (applicant-web-home sanid) "")
              ""
              `(span ([class "applicant-url"])
                     "["
                     (a ([href ,(sanitize-url (applicant-web-home sanid))])
                        "home")
                     nbsp
                     (a ([href ,(sanitize-url (applicant-web-home sanid))]
                         [target "_new"])
                        (img ([src "/static/images/16x16/window-new.png"]
                              [width "12px"]
                              [height "12px"])))
                     "]"))
         " "
         ,(if (string=? (applicant-web-app sanid) "")
              ""
              `(span ([class "applicant-url"])
                     "["
                     (a ([href ,(sanitize-url (applicant-web-app sanid))])
                        "app")
                     nbsp
                     (a ([href ,(sanitize-url (applicant-web-app sanid))]
                         [target "_new"])
                        (img ([src "/static/images/16x16/window-new.png"]
                              [width "12px"]
                              [height "12px"])))
                     "]"))))

(define (field->filename-constructor field)
  (case field
    [(cover-letter) applicant/cover-letter]
    [(cv) applicant/cv]
    [(research-stmt) applicant/research-stmt]
    [(teaching-stmt) applicant/teaching-stmt]
    [(letter-writers) applicant/letter-writers/directory]
    [else (error 'field->file-constructor "don't know field ~s" field)]))

(define (field->field-name field)
  (case field
    [(cover-letter) "Cover Letter"]
    [(cv) "Curriculum Vitae"]
    [(research-stmt) "Research Statement"]
    [(teaching-stmt) "Teaching Statement"]
    [(letter-writers) "Letter Writers"]
    [else (error 'field->file-constructor "don't know field ~s" field)]))