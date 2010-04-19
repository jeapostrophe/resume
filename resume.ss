#lang web-server
(require (lib "plt-match.ss"))
(require "lib/resume-lib.ss"
         "lib/resume-data.ss"
         "lib/identity.ss"
         "part/admin.ss"
         "part/apply.ss"
         "part/initiate.ss"
         "part/letter.ss"
         "part/review.ss")
(provide ; contract
 start
 set-system/directory!
 set-htdocs/directory!
 set-htdocs-url-part!)

;; MAIN CODE
(define (with-identity)
  (match (web-cell-ref current-identity)
    [#f
     (define an-identity
       (send/suspend/dispatch
        (lambda (embed/url)
          (make-resume-page
           "Gateway"
           `((table ([id "homepage-table"])
                    (tr 
                     (td ([id "homepage-box"])
                         (center (strong "Applicants"))
                         (br)
                         (a ([href ,(url->string
                                     (embed/url
                                     (lambda _
                                       (apply-login))))])
                            "Already Registered")
                         (br) (br)
                         (a ([href ,(url->string
                                     (embed/url
                                     (lambda _
                                       (initiate-login))))])
                            "New Applicant")
                         (br))
                     (td ([id "homepage-box"])
                         (center (strong "Letter Writers"))
                         (br)
                         (a ([href ,(url->string
                                     (embed/url
                                     (lambda _
                                       (refcode-login))))])
                            "Upload Reference Letter")
                         (br)))
                    (tr
                     (td ([colspan "2"])
                         nbsp))
                    (td
                     (td ([id "homepage-box"])
                         (center (strong "Department Members"))
                         (br)
                         (a ([href ,(url->string
                                     (embed/url
                                     (lambda _
                                       (review-login))))])
                            "Review Applicants")
                         (br) (br)
                         (a ([href ,(url->string
                                     (embed/url
                                     (lambda _
                                       (admin-login))))])
                            "Administer Site")
                         (br)))))))))
     (web-cell-shadow current-identity an-identity)
     an-identity]
    [an-identity
     an-identity]))

; real-start : -> void
(define (start _)
  ; XXX with-handlers
  (match (with-identity)
    [(struct identity:internal (csid))
     (if (admin-exists? csid)
         (present-admin-console csid)
         (present-browsing-console csid))]
    [(struct identity:applicant (id))
     (present-application-console id)]
    [(struct identity:reference (refcode))
     (refcode-continue refcode)]))