#lang web-server
(require (lib "list.ss"))
(require web-server/servlet/bindings)
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/resume-data.ss"
         "../lib/resume-data-util.ss"
         "../lib/identity.ss")
(provide (all-defined-out))

(define (app-requirements-page)
  (make-confirmation-page
   "Application Process"
   `((p "Welcome! Thank you for your interest in the "
        ,(dept/program-at-univ) ".")
     (p "The application process first collects your "
        "contact information and creates an account for you. "
        "You must then provide your application packet materials.")
     (p "You need not upload all documents at once. "
        "You can use your account to both upload and update information.")
     (p "To complete your application, you will need to provide "
        "the following:"
        (ul
         (li "cover letter")
         (li "curriculum vitae")
         (li "research statement")
         (li "teaching statement")
         (li "name, contact and email information for letter writers")))
     (p ([id "imp-note"])
        "All documents must be in PDF. "
        "Please try to upload screen-vieweable rather than "
        "bit-mapped PDF documents.")
     (p "In the application process, we will allow you to provide "
        "a link to a Web site for supplemental material.  Use this "
        "to refer to papers, videos, etc.  These may, of course, be "
        "in any format, not only in PDF."))))

(define (init-name-id-password/2-page init-fname init-lname init-id
                                      init-password init-password2)
  (lambda (k-url)
    (make-resume-page
     "Register"
     `((form ([action ,(url->string k-url)] [method "post"]
                              [id "narrow-query"])
             (div ([id "imp-note"])
                  (p "For security reasons, you will "
                     (strong "not")
                     " be able to modify your name after clicking the \"Submit\" button on this page.")
                  (p "Please provide your name in a format that your letter writers will recognize: e.g., \"Prof. Eli Upfal\" instead of \"Eliezer Upfal\"; \"John \'Spike\' Hughes\" if everyone knows you by the nickname \"Spike\".")
                  (p "We'll use your name in multiple ways in our software; please help us to get it right. Your \"last name\" should be the one we'll use for alphabetizing, and the one people use in referring to you professionally. Our colleague Andries van Dam would enter \"van Dam\" as his last name. Your \"First name\" is the one used to address you in the U.S. (in our example, \"Andries\" or \"Andy\"). If you are Sarah Jane Smith, and are always called \"Sarah Jane,\" even though it is not hyphenated, then you would use \"Sarah Jane\" as your first name. Please enter your first and last names below.")
                  (p "We also ask for a username; this will be the name you use to access the system again in the future (to enter new recommendation letter writers, for instance)."))
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your first name:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "fname"] [onLoad "this.focus();"]
                                         [value ,init-fname]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your last name:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "lname"] 
                                         [value ,init-lname]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Pick a username for this site:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "id"]
                                         [value ,init-id]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your desired password:")
               (td ([id "q-and-a-a"])
                   (input ([type "password"] [name "password"] 
                                             [value ,init-password]))))
              (tr
               (td ([id "q-and-a-q"])
                   "Enter your password again:")
               (td ([id "q-and-a-a"])
                   (input ([type "password"] [name "password2"]
                                             [value ,init-password2])))))
             (div ([id "note"])
                  (p "For convenience, you may use something like your email address as your username. (However, we do not allow many characters found in email addresses.) " (strong "If you wish to protect your identity") ", however, please pick an obscure username. Someone else could try to use your email address as a username; although they would not gain access to your data without knowing your password, from the resulting error page they might infer that you are applying.")
                  (p "Please make a note of your username and password so that you can access the system in the future."))
             (p ([id "submit-button"])
                (input ([type "submit"] [name "enter"] [value "Submit"]))))))))

(define (now-registered-page id)
  (make-confirmation-page
   "Registration Acknowledgment"
   `((p
      "We have stored your information against the username \""
      (span ([id "alert"]) ,id)
      "\".  Please use this for subsequent logins."))))

(define (init-get-name/id/password/2
         init-fname init-lname init-id init-password init-password2)
  (let ([b (request-bindings
            (send/suspend/url
             (init-name-id-password/2-page init-fname init-lname init-id init-password
                                           init-password2)))])
    (redirect/get)
    (values (extract-binding/single 'fname b)
            (extract-binding/single 'lname b)
            (extract-binding/single 'id b)
            (extract-binding/single 'password b)
            (extract-binding/single 'password2 b))))

(define (already-existing-id-page id)
  (make-confirmation-page
   "Pick a New Username"
   `((p
      "The username you provided, \""
      (span ([id "alert"]) ,id)
      "\", is too close to one already in the system. "
      "Please try another one."))))

(define (initiate->-threshold-page)
  (make-output-page
   "Initiation Threshold Exceeded"
   `((p "You have exceeded the initiation threshold. "
        "If you are a genuine applicant, and are having trouble, "
        (a ([href "/static/contact.html"])
           "contact us")
        ".")
     (p ([id "imp-note"])
        "We log all failed attempts."))))    

(define (initiate-loop)
  (define counter (box 1))
  (let the-loop ([bad-tries empty]
                 [init-fname ""]
                 [init-lname ""]
                 [init-id ""]
                 [init-password ""]
                 [init-password2 ""])
    (if (> (unbox counter) login-loop-threshold)
        (begin
          (record-failed-login 'account-creation bad-tries)
          (output initiate->-threshold-page))
        (let-values ([(fname lname id password password2)
                      (init-get-name/id/password/2
                       init-fname init-lname init-id init-password init-password2)])
          (if (or (string=? "" fname) (string=? "" lname) (string=? "" id)
                  (string=? "" password) (string=? "" password2))
              (begin
                (confirm enter-all-fields-page)
                (the-loop bad-tries
                          fname lname id password password2))
              (let ([sanid (sanitize-string id)])
                (if (applicant-exists? sanid)
                    (begin
                      (confirm already-existing-id-page id)
                      (set-box! counter (add1 (unbox counter)))
                      (the-loop (cons (list fname lname id password password2)
                                      bad-tries)
                                fname lname id password password2))
                    (if (string=? password password2)
                        (values fname lname id password)
                        (begin
                          (confirm passwords-not-equal-page)
                          (set-box! counter (add1 (unbox counter)))
                          (the-loop (cons (list fname lname id password password2)
                                          bad-tries)
                                    fname lname id password password2))))))))))

(define (store-contact-info sanid)
  (let ([b
         (request-bindings (send/suspend (get-contact-info-page sanid)))])
    (redirect/get)
    (write-applicant-contact-info! sanid b)))

(define (got-contact-info-page)
  (make-confirmation-page
   "Contact Information Stored"
   `((p "We have stored your contact information. "
        "At any time you can upload your material at the "
        (a ([href "/"])
           "application console")
        ", visiting it as often as you want. "
        "(If you wish to create a bookmark, please use that "
        "link, not this page.) ")
     (p
      "If you already have material you want to upload, "
      "you can proceed to the console now."))))

(define (email-address-page init-email)
  (lambda (k-url)
    (make-resume-page
     "Provide Email Address"
     `((form ([action ,(url->string k-url)] [method "post"]
                              [id "narrow-query"])
             (p "To begin, please provide "
                "a single email address that you will be able to access "
                "throughout the application period.")
             (table
              (tr
               (td ([id "q-and-a-q"])
                   "Email Address:")
               (td ([id "q-and-a-a"])
                   (input ([type "text"] [name "email"]
                                         [value ,init-email])))))
             (p ([id "imp-note"])
                "After you click the \"Submit\" button, we will send an email message "
                "to your address.  This message will contain a link. "
                "You must click on this link to resume the application "
                "process.")
             (p ([id "submit-button"])
                (input ([type "submit"] [name "enter"] [value "Submit"]))))))))

(define (no-blank-email-init-app-page)
  (make-confirmation-page
   "Please Provide Email Address"
   `((p "Please provide an email address to initiate the "
        "application process."))))

(define (get-email-address init-email)
  (let ([b (request-bindings
            (send/suspend/url (email-address-page init-email)))])
    (redirect/get)
    (let ([email (extract-binding/single 'email b)])
      (cond
        [(string=? email "")
         (begin
           (confirm no-blank-email-init-app-page)
           (get-email-address email))]
        [(no-@-in-email? email)
         (begin
           (confirm no-@-in-email-page)
           (get-email-address email))]
        [else
         (send/suspend/url
          (lambda (k-url)
            (send-email 
             (list email) '() '()
             (string-append (dept/program-at-univ/short)
                            " Job Application: Continue Registration")
             `("An application with your email address has been initiated "
               ,(format "at the ~a." (dept/program-at-univ))
               ""
               "If you intended this, please visit the URL"
               ""
               ,(format "~a~a" (host/portname) (url->string k-url))
               ""
               "to continue the application process."
               ""
               "If you did not initiate this application, please notify us immediately "
               "and we will investigate."
               ""
               "Thank you for your application."))
            (output/non-terminal
             (lambda ()
               (lambda (ignore-k-url)
                 (make-output-page
                  "Please Enter Link"
                  `((p "We have sent a link to the address \""
                       (span ([id "alert"]) ,email)
                       "\".  If this address is incorrect, please "
                       "go back and enter the correct address.  If "
                       "you do not receive email in the next few minutes, "
                       "there may be a transient network problem" 151
                       "please wait and try again later.")
                    (p "The link sent to you will expire after a few "
                       "hours, so please check your email promptly."))))))))])
      email)))

(define (initiate-login)
  (confirm app-requirements-page)
  (let ([email (get-email-address "")])
    (let-values ([(fname lname id password) (initiate-loop)])
      (let ([sanid (sanitize-string id)])
        (make-applicant! sanid fname lname password email)
        (confirm now-registered-page sanid)
        (store-contact-info sanid)
        (confirm got-contact-info-page)
        (make-identity:applicant
         sanid)))))