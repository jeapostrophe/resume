#lang web-server
(require (lib "list.ss")
         (lib "date.ss")
         (lib "etc.ss"))
(require web-server/servlet/bindings)
(require "../wtk/wtk-list-table.ss"
         (planet "list.ss" ("jaymccarthy" "mmss.plt" 1)))
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/resume-data.ss"
         "../lib/identity.ss"
         "../lib/web-util.ss"
         "../part/apply.ss"
         "../part/letter.ss")
(provide (all-defined-out))

(define up:mode (make-web-cell "paper-list"))
(define up:sanid (make-web-cell ""))

(define-values
  (candidate-areas-form candidate-areas-page)
  (generate-list-form candidate-areas set-candidate-areas!
                      #:deleted!
                      (lambda (ov)
                        (for-each (lambda (sanid)
                                    (set-applicant-areas!
                                     sanid
                                     (filter (lambda (ov^)
                                               (not (equal? ov ov^)))
                                             (applicant-areas sanid))))
                                  (applicants)))
                      #:editted!
                      (lambda (ov nv)
                        (for-each (lambda (sanid)
                                    (set-applicant-areas!
                                     sanid
                                     (map (lambda (ov^)
                                            (if (equal? ov ov^)
                                                nv
                                                ov^))
                                          (applicant-areas sanid))))
                                  (applicants)))))

(define (generate-list-form:std
         getter setter!
         sanid-getter sanid-setter!)
  (generate-list-form getter setter!
                      #:deleted!
                      (lambda (ov)
                        (for-each (lambda (sanid)
                                    (if (equal? (sanid-getter sanid) ov)
                                        (sanid-setter! sanid (first (getter)))
                                        (void)))
                                  (applicants)))
                      #:editted!
                      (lambda (ov nv)
                        (for-each (lambda (sanid)
                                    (if (equal? (sanid-getter sanid) ov)
                                        (sanid-setter! sanid nv)
                                        (void)))
                                  (applicants)))))

(define-values
  (candidate-genders-form candidate-genders-page)
  (generate-list-form:std candidate-genders set-candidate-genders!
                          applicant-gender set-applicant-gender!))
(define-values
  (candidate-ethnicities-form candidate-ethnicities-page)
  (generate-list-form:std candidate-ethnicities set-candidate-ethnicities!
                          applicant-ethnicity set-applicant-ethnicity!))
(define-values
  (candidate-decisions-form candidate-decisions-page)
  (generate-list-form:std candidate-decisions set-candidate-decisions!
                          applicant-decision set-applicant-decision!))

(define-values
  (smtp-settings-form smtp-settings-page)
  (generate-form:strings
   (list (vector "Server" smtp/server set-smtp/server!)
         (vector "Port" smtp/port set-smtp/port!)
         (vector "Username" smtp/username set-smtp/username!)
         (vector "Password" smtp/password set-smtp/password!))))

(define-values
  (contact-settings-form contact-settings-page)
  (generate-form:strings
   (list (vector "Program" dept/program set-dept/program!)
         (vector "University" dept/univ set-dept/univ!)
         (vector "Short" dept/program-at-univ/short set-dept/program-at-univ/short!)
         (vector "Contact Name" dept/contact-name set-dept/contact-name!)
         (vector "Contact Title" dept/contact-title set-dept/contact-title!)
         (vector "Contact Phone" dept/contact-phone set-dept/contact-phone!)
         (vector "Contact Email" dept/contact-email set-dept/contact-email!))))

(define-values
  (server-settings-form server-settings-page)
  (generate-form:strings
   (list (vector "Hostname & Port" host/portname set-host/portname!)
         (vector "PDFtk Path" pdftk-path set-pdftk-path!)
         (vector "PDFlatex Path" pdflatex-path set-pdflatex-path!))))

(define (settings-form r)
  (define bs (request-bindings r))
  (candidate-areas-form r)
  (candidate-genders-form r)
  (candidate-ethnicities-form r)
  (candidate-decisions-form r)
  (smtp-settings-form r)
  (contact-settings-form r)
  (server-settings-form r)
  (redirect/get)
  (settings-page))

(define (settings-page)
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      "Settings"
      `((p ([id "change-password"])
           (a ([href ,(url->string (embed/url (lambda _ 'return-to-outside)))])
              "Return to list"))
        (form ([action ,(url->string (embed/url settings-form))]
               [method "POST"])
              (h2 "Server Settings")
              ,(server-settings-page embed/url)
              (h2 "SMTP Settings")
              ,(smtp-settings-page embed/url)
              (h2 "Contact Settings")
              ,(contact-settings-page embed/url)
              (h2 "Candidate Options")
              (table
               (tr (th "Areas")
                   (td ,(candidate-areas-page embed/url)))
               (tr (td ([colspan "2"]) nbsp))
               (tr (th "Genders")
                   (td ,(candidate-genders-page embed/url)))
               (tr (td ([colspan "2"]) nbsp))
               (tr (th "Ethnicities")
                   (td ,(candidate-ethnicities-page embed/url)))
               (tr (td ([colspan "2"]) nbsp))
               (tr (th "Decisions")
                   (td ,(candidate-decisions-page embed/url))))
              (p ([id "submit-button"])
                 (input ([type "submit"] [value "Save"])))))))))

(define (admin-console csid sanid)
  (web-cell-shadow up:mode "details")
  (web-cell-shadow up:sanid sanid)
  (redirect/get)
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      (format "~a ~a" (applicant-fname sanid) (applicant-lname sanid))
      `((p ([id "change-password"])
           (a ([href ,(url->string (embed/url (lambda _ 'return-to-outside)))])
              "Return to list"))
        
        (h2 (a ([href ,(url->string (embed/url (lambda _ (present-application-console sanid))))]
                [target "_new"])
               "View Applicant Console"))
        
        (h2 "Contact Information")
        ,(contact-info-table sanid)
        
        (h2 "Resend Reference Letters")
        (table 
         (tr (th "Letter Writer")
             (th nbsp)
             (th nbsp))
         ,@(map (lambda (ref)
                  `(tr (td ,(applicant-letter-writer-name sanid ref))
                       (td ,(if (reference-letter-exists? sanid ref)
                                `(span "Completed.")
                                `(a ([href
                                      ,(url->string
                                        (embed/url
                                        (lambda (_)
                                          (resend-refcode sanid ref)
                                          (admin-console csid sanid))))])
                                    "Resend Request")))
                       (td (a ([href
                                ,(url->string
                                  (embed/url
                                  (lambda (_)
                                    (upload-reference-letter ref))))]
                               [target "_new"])
                              "Upload"))))
                (applicant-letter-writers sanid)))
        
        (h2 "Classification")
        (table
         ,@(map (lambda (label options lookup set!)
                  (let ([handle-change
                         (lambda (r)
                           (let ([new-value (extract-binding/single 'new-value (request-bindings r))])
                             (set! sanid new-value))
                           (admin-console csid sanid))])
                    `(tr (td (h3 ,label))
                         (td nbsp)
                         (td ,(if (reviewer-student? csid)
                                  (lookup sanid)
                                  `(form ([method "POST"]
                                          [id "narrow-query"]
                                          [action ,(url->string (embed/url handle-change))])
                                         (select ([name "new-value"])
                                                 ,@(map (lambda (option)
                                                          `(option ([value ,option]
                                                                    ,@(if (string=? option (lookup sanid))
                                                                          `([selected "true"])
                                                                          empty))
                                                                   ,option))
                                                        (options)))
                                         (br)
                                         (input ([type "submit"] [value "Modify"]))))))))
                (list "Gender" "Ethnicity" "Decisions")
                (list candidate-genders candidate-ethnicities candidate-decisions)
                (list applicant-gender applicant-ethnicity applicant-decision)
                (list set-applicant-gender! set-applicant-ethnicity! set-applicant-decision!))))))))

(define (resend-refcode sanid refcode)
  (define name (applicant-letter-writer-name sanid refcode))
  (define institution (applicant-letter-writer-institution sanid refcode))
  (define email (applicant-letter-writer-email sanid refcode))
  (let* ([request
          (send/suspend/url
           (lambda (k-url)
             (make-resume-page
              (format "Resend Reference Code for ~a ~a" (applicant-fname sanid) (applicant-lname sanid))
              `((form ([method "POST"]
                       [action ,(url->string k-url)])
                      (p "By clicking confirm, a reference code will be resent to")
                      (p ,name " at " ,institution)
                      (p " via the email address: ")
                      (p (input ([type "text"] [name "email"] [value ,email])))
                      (p "To change the email address, enter it in the box above.")
                      (p (input ([type "submit"] [value "Confirm"]))))))))]
         [bindings (request-bindings request)]
         [email (extract-binding/single 'email bindings)])
    (if (no-@-in-email? email)
        (begin (no-@-in-email-page)
               (resend-refcode sanid refcode))
        (begin (set-applicant-letter-writer-email! sanid refcode email)
               (redirect/get)
               (email-refcode sanid refcode)))))

(define admin-ui-state (with-table-ui:initial-state 'name))
(define reviewers-ui-state (with-table-ui:initial-state 'name))

(define ((add-reviewer-form csid) r)
  (define bs (request-bindings r))
  (define username (extract-binding/single 'username bs))
  (define email (extract-binding/single 'email bs))    
  (set-reviewer-email! username email)
  (send-email 
   (list email) '() '()
   (format "Account to review ~a faculty candidates" (dept/program-at-univ/short))
   (list
    "Dear Faculty Member,"
    ""
    "To review faculty candidates, visit this URL:"
    ""
    (format "  ~a/" (host/portname))
    ""
    "Please bookmark this URL for future use."
    ""
    ;"You can only access this Web site when you are within the department's"
    ;"network, either from inside the department or from the outside using"
    ;"a VPN."
    ""
    "To log in, please use the following username:"
    ""
    (format "  u: ~a" username)
    ""
    "You can set your password at the login site by clicking the `Set initial password' button."
    "Please do not use your regular password on this site."
    ""
    "If you encounter difficulties, please contact us."))              
  (redirect/get)    
  (send/suspend
   (make-confirmation-page
    "New Reviewer"
    `((p "The reviewer has been sent an email letting them know their username. They can use the `Set initial password' button on the login screen to set their password."))))
  (reviewers-page csid))

(define (reviewers-page csid)
  (web-cell-shadow up:mode "reviewers")
  (redirect/get)    
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      "Reviewers"
      `((p ([id "change-password"])
           (a ([href ,(url->string
                       (embed/url
                       (lambda _
                         'exit)))])
              "Back to list"))
        (p
         (form ([method "POST"] [action ,(url->string (embed/url (add-reviewer-form csid)))])
               (table ([align "center"])
                      (tr (td "Username:")
                          (td (input ([type "text"] [name "username"] [onLoad "this.focus();"] [size "24"]))))
                      (tr (td "Email:")
                          (td (input ([type "text"] [name "email"] [size "24"]))))
                      (tr (td ([colspan "2"])
                              (input ([type "submit"] [value "Add Reviewer"])))))))
        (p
         ,(if (empty? (reviewers))
              'nbsp
              (with-table-ui/embed/url
               embed/url reviewers-ui-state "applicant-list"
               (list (make-table-column 'name "Username" "Username"
                                        identity
                                        string-ci<=?)
                     (make-table-column 'email "Email" "Email"
                                        reviewer-email
                                        (<=/proj string-ci<=? reviewer-email))
                     (make-table-column 'student "Student?" "Student?"
                                        (lambda (rid)
                                          `(p ,(if (reviewer-student? rid)
                                                   `(strong "Yes")
                                                   `(strong "No"))
                                              (br)
                                              (a ([href ,(url->string
                                                          (embed/url
                                                          (lambda _
                                                            (set-reviewer-student?! rid (not (reviewer-student? rid)))
                                                            (reviewers-page csid))))])
                                                 "change")))
                                        (lambda (a b) (and a b))))
               empty
               (list)
               reviewers
               (lambda (the-list-ui)
                 empty)))))))))

(define (generate-csv)
  (define obytes (open-output-bytes))
  (parameterize ([current-output-port obytes]
                 [date-display-format 'rfc2822])
    (for-each (lambda (l)
                (for-each display
                          (between "\t" l))
                (newline))
              (list* (append 
                      (list
                       "RFC2822" "Epoch Time"
                       "First Name" "Last Name" "Email")
                      (map (lambda (a) (format "Gender: ~a" a))
                           (candidate-genders))
                      (map (lambda (a) (format "Area: ~a" a))
                           (candidate-areas))
                      (map (lambda (a) (format "Ethnicity: ~a" a))
                           (candidate-ethnicities))
                      (map (lambda (a) (format "Decision: ~a" a))
                           (candidate-decisions)))
                     (map (lambda (sanid)
                            (append 
                             (list (date->string (seconds->date (applicant-date-received sanid)) #t)
                                   (applicant-date-received sanid)
                                   (applicant-fname sanid)
                                   (applicant-lname sanid)
                                   (applicant-email sanid))
                             (map (lambda (a) 
                                    (if (member a (list (applicant-gender sanid)))
                                        1 0))
                                  (candidate-genders))
                             (map (lambda (a) 
                                    (if (member a (applicant-areas sanid))
                                        1 0))
                                  (candidate-areas))
                             (map (lambda (a) 
                                    (if (member a (list (applicant-ethnicity sanid)))
                                        1 0))
                                  (candidate-ethnicities))
                             (map (lambda (a) 
                                    (if (member a (list (applicant-decision sanid)))
                                        1 0))
                                  (candidate-decisions))))
                          (applicants)))))
  (send/suspend/url
   (lambda _
     (make-response/full
      200 "Okay"
      (current-seconds) #"text/plain" empty
      (list (get-output-bytes obytes))))))

(define (present-admin-console csid)
  (web-cell-shadow up:mode "paper-list")
  (redirect/get)    
  (send/suspend/dispatch
   (lambda (embed/url)
     (make-resume-page
      "Applicants (Admin view)"
      `((p ([id "change-password"])
           (a ([href ,(url->string 
                       (embed/url
                       (lambda _
                         (generate-csv))))])
              "Generate CSV")
           (br)
           (a ([href ,(url->string
                       (embed/url
                       (lambda _
                         (reset-password-page #f set-admin-password! csid)
                         (present-admin-console csid))))])
              "Change your password")
           (br)
           (a ([href ,(url->string
                       (embed/url
                       (lambda _
                         (settings-page)
                         (present-admin-console csid))))])
              "Settings")
           (br)
           (a ([href ,(url->string
                       (embed/url
                       (lambda _
                         (reviewers-page csid)
                         (present-admin-console csid))))])
              "Add reviewers"))
        (p
         ,(with-table-ui/embed/url
           embed/url admin-ui-state "applicant-list"
           (list (make-table-column 'gender "Gender" "Gender"
                                    applicant-gender
                                    (<=/proj string-ci<=? applicant-gender))
                 (make-table-column 'ethnicity "Ethnicity" "Ethnicity"
                                    applicant-ethnicity
                                    (<=/proj string-ci>=? applicant-ethnicity))
                 (make-table-column 'decision "Decision" "Decision"
                                    applicant-decision
                                    (<=/proj string-ci<=? applicant-decision))
                 (make-table-column 'name "Name" "Name"
                                    (lambda (sanid)
                                      (html:span:applicant-name
                                       sanid 
                                       (url->string
                                        (embed/url (lambda _
                                                    (admin-console csid sanid)
                                                    (present-admin-console csid))))))
                                    (cascade-<= string-ci=?
                                                (<=/proj string-ci<=? applicant-lname)                                                           
                                                (<=/proj string-ci<=? applicant-fname))))
           empty
           (list (make-interleave
                  'every
                  (lambda (sanid)
                    `(tr ([class "letters-line"])
                         (td ([class "filler"]) nbsp)
                         (td ([class "filler"]) nbsp)
                         (td ([class "filler"]) nbsp)
                         (td ([class "letters-list"]
                              [colspan "1"])
                             "Unsent Letters: "
                             ,@(let ([refs 
                                      (map (lambda (ref)
                                             (applicant-letter-writer-name sanid ref))
                                           (filter (lambda (ref)
                                                     (not (reference-letter-exists? sanid ref)))
                                                   (applicant-letter-writers sanid)))])
                                 (list-with-separator
                                  refs
                                  "No names submitted")))))))
           applicants
           (lambda (the-list-ui)
             empty))))))))

(define (admin-login)
  (let-values ([(id csid password)
                (login-loop 'admin-person-login
                            admin-exists?
                            (lambda _ "jay@cs.brown.edu")
                            admin-password
                            set-admin-password!)])
    (make-identity:internal
     csid)))