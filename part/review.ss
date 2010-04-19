#lang web-server
(require (lib "list.ss")
         (lib "etc.ss"))
(require "../wtk/wtk-list-table.ss"
         (planet "list.ss" ("jaymccarthy" "mmss.plt" 1)))
(require web-server/servlet/bindings)
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/pdftk.ss"
         "../lib/resume-data.ss"
         "../lib/resume-data-util.ss"
         "../lib/identity.ss")
(provide review-login
         present-browsing-console)

(define up:mode (make-web-cell "paper-list"))
(define up:sanid (make-web-cell ""))

(define (review-console csid sanid)
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
        
        (table ([width "100%"])
               (tr (td ([width "40%"]) (h2 "Contact Information"))
                   (td nbsp)
                   (td ([width "40%"]) (h2 "Classification")))
               (tr (td ([width "40%"]) ,(contact-info-table sanid))
                   (td nbsp)
                   (td ([width "40%"]) 
                       (table
                        ,@(map (lambda (label options lookup set!)
                                 (define ids (map (lambda _
                                                    (symbol->string (gensym)))
                                                  (options)))
                                 (define (handle-change r)
                                   (define bs (request-bindings r))
                                   (define new-value
                                     (map cdr
                                          (filter (lambda (i*o)
                                                    (exists-binding? (string->symbol (car i*o)) bs))
                                                  (map cons 
                                                       ids
                                                       (options)))))
                                   (set! sanid new-value)
                                   (review-console csid sanid))
                                 (define display-xexpr
                                   `(table ,@(map (lambda (id option)
                                                    `(tr 
                                                      (td
                                                       (input ([type "checkbox"]
                                                               [name ,id]
                                                               ,@(if (member? option (lookup sanid))
                                                                     `([checked "true"])
                                                                     empty))))
                                                      (td
                                                       ,option)))
                                                  ids
                                                  (options))))
                                 `(tr (td (h3 ,label))
                                      (td nbsp)
                                      (td ,(if (reviewer-student? csid)
                                               display-xexpr
                                               `(form ([method "POST"]
                                                       [id "narrow-query"]
                                                       [action ,(url->string (embed/url handle-change))])
                                                      ,display-xexpr
                                                      (br)
                                                      (input ([type "submit"] [value "Modify"])))))))
                               (list "Areas")
                               (list candidate-areas)
                               (list applicant-areas)
                               (list set-applicant-areas!))
                        ,@(map (lambda (label options lookup set!)
                                 (define (handle-change r)
                                   (define new-value (extract-binding/single 'new-value (request-bindings r)))
                                   (set! sanid new-value)
                                   (review-console csid sanid))
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
                                                      (input ([type "submit"] [value "Modify"])))))))
                               (list "Gender" "Ethnicity")
                               (list candidate-genders candidate-ethnicities)
                               (list applicant-gender applicant-ethnicity)
                               (list set-applicant-gender! set-applicant-ethnicity!)))))
               (tr (td ([colspan "3"]) nbsp))
               (tr (td ([width "40%"]) (h2 "Materials"))
                   (td nbsp)
                   (td ([width "40%"]) ,(if (not (reviewer-student? csid))
                                            `(h2 "Letters")
                                            `nbsp)))
               (tr (td ([width "40%"])
                       (ul
                        ,@(map (lambda (field label)
                                 `(li ,(applicant/material-link sanid field label)))
                               '(cover-letter cv research-stmt teaching-stmt)
                               '("Cover" "CV" "Research" "Teaching"))
                        (li ,(if (not (reviewer-student? csid))
                                 (applicant/aggregate-letters-link sanid)
                                 (applicant/aggregate-link sanid)))))
                   (td nbsp)
                   (td ([width "40%"])
                       ,(if (not (or (reviewer-student? csid)
                                     (empty? (applicant-letter-links sanid))))
                            `(ul
                              ,@(map (lambda (l)
                                       `(li ,l))
                                     (applicant-letter-links sanid)))
                            `nbsp))))
        
        (h2 "Reviews")
        ,(let ([reviewers (read-reviewers/check csid sanid)])
           (if (empty? reviewers)
               `(p "No reviews submitted.")
               `(table ([id "review-table"])
                       ,@(if (applicant-rating/average sanid)
                             `((tr (td ([colspan "2"])
                                       (h3 "Average Rating: "
                                           ,(number->string
                                             (applicant-rating/average sanid))))))
                             empty)
                       ,@(apply append
                                (map (lambda (r)                                         
                                       `((a ([name ,(format "review-~a" r)]))
                                         (tr (td ,r)                                                  
                                             ,(if (review-score-exists? sanid r)
                                                  (let ([s (applicant-review-score sanid r)])
                                                    `(td ,s ": " ,(cadr (assoc (string->number s) review-scoring-table))))
                                                  `(td "*: This reviewer has not scored this candidate.")))
                                         (tr (td ([colspan "2"])
                                                 (pre ,(applicant-review-comments sanid r))))
                                         (tr (td ([colspan "2"]) nbsp))))
                                     reviewers)))))
        
        (h3 "Your Review")
        (form ([method "POST"]
               [action ,(url->string
                         (embed/url
                         (lambda (r)
                           (let* ([score* (extract-binding/single 'score (request-bindings r))]
                                  [score (if (string=? score* "#f") #f score*)]
                                  [comments (extract-binding/single 'comments (request-bindings r))])
                             (set-applicant-review-score! sanid csid score)
                             (set-applicant-review-comments! sanid csid comments))
                           (review-console csid sanid))))])
              (select ([name "score"])
                      (option ([value "#f"])
                              "*: This is just a comment.  I am not (yet) rating this candidate.")
                      ,@(let ([selected (if (review-score-exists? sanid csid)
                                            (applicant-review-score sanid csid)
                                            "do-not-match!")])
                          (map (lambda (scoring)
                                 (let ([num-str (number->string (first scoring))])
                                   `(option ([value ,num-str]
                                             ,@(if (string=? num-str selected)
                                                   `([selected "true"])
                                                   empty))
                                            ,num-str ": " ,(second scoring))))
                               review-scoring-table)))
              (br)
              (textarea ([rows "6"]
                         [cols "80"]
                         [wrap "physical"]
                         [name "comments"])
                        ,(if (review-comments-exists? sanid csid)
                             (applicant-review-comments sanid csid)
                             ""))
              (p
               (input ([type "submit"]
                       [name "submit"]
                       [value "Save Review"])))))))))

(define (applicant-letter-links sanid)
  (map
   (lambda (refcode)
     (define name (applicant-letter-writer-name sanid refcode))
     (if (reference-letter-exists? sanid refcode)
         `(a ([href
               ,(internal-path->url (applicant/reference-letter sanid refcode))])
             ,name)
         name))
   (applicant-letter-writers sanid)))

(define browsing-ui-state (with-table-ui:initial-state 'name))
(define current-search/name (make-web-cell ""))

(define ((pdf-generation-form csid) req)
  (define bs (request-bindings req))
  (define sanids
    (mergesort (map second
                    (filter (lambda (x) x)
                            (map (compose (lambda (x)
                                            (regexp-match #rx"^select-(.*)$" x))
                                          symbol->string
                                          car)
                                 bs)))
               (cascade-<= string-ci=?
                           (<=/proj string-ci<=? applicant-lname)                                                           
                           (<=/proj string-ci<=? applicant-fname))))
  (define url
    (internal-path->url 
     (create-multi-applicant-aggregate! sanids (not (reviewer-student? csid)))))
  (redirect/get)
  (send/suspend/url
   (lambda _
     (make-resume-page
      "Multi-Applicant PDF Generation"
      `((p "Your PDF for the following applicants is currently being generated:")
        (ul
         ,@(map (lambda (sanid)
                  `(li ,(applicant-fname sanid)
                       " "
                       ,(applicant-lname sanid)))
                sanids))
        (p "The PDF will be available at the following URL in anywhere from 2 to 10 minutes, depending on the number of applicants selected:")
        (p ([align "center"])
           (tt (a ([href ,url]) 
                  ,(host/portname)
                  ,url))))))))

(define (present-browsing-console csid)
  (web-cell-shadow up:mode "paper-list")
  (redirect/get)
  (let* ([sanids (applicants)]
         [sanids (filter (lambda (sanid)
                           (regexp-match (string-downcase (web-cell-ref current-search/name))
                                         (string-downcase (format "~a ~a" (applicant-fname sanid) (applicant-lname sanid)))))
                         sanids)])
    (send/suspend/dispatch
     (lambda (embed/url)
       (make-resume-page
        "Applicants"
        `((p ([id "change-password"])
             (a ([href ,(url->string
                         (embed/url
                         (lambda _
                           (reset-password-page #f set-reviewer-password! csid)
                           (present-browsing-console csid))))])
                "Change your password"))
          (form ([method "POST"]
                 [action ,(url->string
                           (embed/url 
                           (lambda (request)
                             (define bs (request-bindings request))
                             (cond
                               [(exists-binding? 'search bs)
                                (with-handlers ([exn:fail? (lambda (e) (void))])
                                  (web-cell-shadow 
                                   current-search/name
                                   (extract-binding/single 'name bs)))
                                (present-browsing-console csid)]
                               [(exists-binding? 'pdf bs)
                                ((pdf-generation-form csid) request)]
                               [else
                                (present-browsing-console csid)]))))])
                ,(with-table-ui/embed/url
                  embed/url browsing-ui-state "applicant-list"
                  (append (list (make-table-column 'checkbox "" ""
                                                   (lambda (sanid)
                                                     `(input ([name ,(format "select-~a" sanid)]
                                                              [type "checkbox"])))
                                                   string-ci>=?)
                                (make-table-column 'rating "Rating" "Rating"
                                                   (lambda (sanid)
                                                     (define avg (applicant-rating/average sanid))
                                                     (if avg
                                                         (number->string avg)
                                                         "none"))
                                                   (<=/proj >= (lambda (o)
                                                                 (or (applicant-rating/average o)
                                                                     -inf.0))))
                                (make-table-column 'area "Area" "Area" 
                                                   applicant-areas/string
                                                   (<=/proj string-ci>=? applicant-areas/string))
                                (make-table-column 'gender "Gender" "Gender"
                                                   applicant-gender
                                                   (<=/proj string-ci<=? applicant-gender))
                                (make-table-column 'ethnicity "Ethnicity" "Ethnicity"
                                                   applicant-ethnicity
                                                   (<=/proj string-ci>=? applicant-ethnicity))
                                (make-table-column 'name "Name" "Name"
                                                   (lambda (sanid)
                                                     (html:span:applicant-name
                                                      sanid 
                                                      (url->string
                                                       (embed/url (lambda _
                                                                   (review-console csid sanid)
                                                                   (present-browsing-console csid))))))
                                                   (cascade-<= string-ci=?
                                                               (<=/proj string-ci<=? applicant-lname)                                                           
                                                               (<=/proj string-ci<=? applicant-fname))))
                          (map (lambda (field label)
                                 (make-table-column field label label
                                                    (lambda (sanid)
                                                      (applicant/material-link sanid field label))
                                                    #f))
                               
                               '(cover-letter cv research-stmt teaching-stmt)
                               '("Cover" "CV" "Research" "Teaching"))
                          (list
                           (make-table-column 'aggregate "Aggregate" "Aggregate"
                                              (if (reviewer-student? csid)
                                                  applicant/aggregate-link
                                                  applicant/aggregate-letters-link)
                                              #f)))
                  (append (list
                           (make-list-filter 'unrated
                                             (lambda (sanid)
                                               (not (review-score-exists? sanid csid)))
                                             "Unrated by Me")
                           (make-list-filter 'rated
                                             (lambda (sanid)
                                               (review-score-exists? sanid csid))
                                             "Rated by Me"))
                          (map (lambda (area)
                                 (make-list-filter (string->symbol area)
                                                   (lambda (sanid)
                                                     (member? area (applicant-areas sanid)))
                                                   area))
                               (candidate-areas))
                          (list 
                           (make-list-filter 'uncategorized
                                             (lambda (sanid)
                                               (empty? (applicant-areas sanid)))
                                             "Uncategorized")))
                  (append (if (not (reviewer-student? csid))
                              (list (make-interleave 
                                     'every
                                     (lambda (sanid)
                                       `(tr ([class "letters-line"])
                                            (td ([class "filler"]) nbsp)
                                            (td ([class "filler"]) nbsp)
                                            (td ([class "filler"]) nbsp)
                                            (td ([class "filler"]) nbsp)
                                            (td ([class "letters-list"]
                                                 [colspan "5"])
                                                "Reference Letters: "
                                                ,@(list-with-separator
                                                   (applicant-letter-links sanid)
                                                   "No names submitted"))
                                            (td ([class "filler"]) nbsp)))))
                              empty)
                          (list
                           (make-interleave
                            'every 
                            (lambda (sanid)
                              `(tr ([class "reviewers-line"])
                                   (td ([class "filler"]) nbsp)
                                   (td ([class "filler"]) nbsp)
                                   (td ([class "filler"]) nbsp)
                                   (td ([class "filler"]) nbsp)
                                   (td ([class "reviewers-list"]
                                        [colspan "6"])
                                       ,@(let* ([reviewers (read-reviewers/check csid sanid)]
                                                [revs
                                                 (map (lambda (r)
                                                        `(span ,r
                                                               nbsp
                                                               "("
                                                               ,(if (review-score-exists? sanid r)
                                                                    (applicant-review-score sanid r)
                                                                    "*")
                                                               ")"))
                                                      reviewers)])
                                           (append (if (empty? revs)
                                                       empty
                                                       (list "Reviewers (w/ Scores): "))
                                                   (list-with-separator
                                                    revs
                                                    "No reviews yet")))))))))
                  (lambda ()
                    sanids)
                  (lambda (the-list-ui)
                    `(div
                      (fieldset ([style ,(string-append "margin: 0px auto; border: 2px dotted; "
                                                        "padding: 5px; font-size: small; width: 27em; "
                                                        "clear: both; line-height: 140%")])
                                (label "Filter")
                                (table
                                 (tr (td (strong "Name:"))
                                     (td (input ([type "text"]
                                                 [name "name"] [onLoad "this.focus();"]
                                                 [value ,(web-cell-ref current-search/name)]
                                                 [size "20"]))
                                         (input ([name "search"] [type "submit"] [value "Filter"]))))
                                 (tr (td (strong "Classification:"))
                                     (td (a ([class "k-url"]
                                             [href ,(url->string (embed/url (list-ui-clear-filters the-list-ui)))])
                                            "Clear All")))
                                 ,@((list-ui-map-filters the-list-ui)
                                    (lambda (tag filtered? filter)
                                      (let ([link `(a ([class "k-url"]
                                                       [href ,(url->string (embed/url filter))])
                                                      ,tag)])
                                        `(tr (td ,(if filtered? 'radic 'nbsp))
                                             (td ,(if filtered?
                                                      `(strong ,link)
                                                      link))))))
                                 (tr (td ([align "center"] 
                                          [colspan "2"]
                                          [style "font-size: 90%;"])
                                         "To see all graphics candidates you've not yet rated, click \"Graphics\" and \"Unrated by Me\"."))))
                      nbsp (br))))
                (p ([align "center"])
                   (input ([name "pdf"]
                           [type "submit"]
                           [value "Generate Multi-Applicant PDF"]))))))))))

(define (review-login)
  (let-values ([(id csid password)
                (login-loop 'reviewer-login
                            reviewer-exists?
                            reviewer-email
                            reviewer-password
                            set-reviewer-password!)])
    (make-identity:internal
     csid)))