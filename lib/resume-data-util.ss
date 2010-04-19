#lang web-server
(require  (planet "list.ss" ("jaymccarthy" "mmss.plt" 1)))
(require "../lib/resume-data.ss")
(provide (all-defined-out))

(define (url/letter-submission)
  (host/portname))

(define (dept/signature)
  (list (dept/contact-name)
        (dept/contact-title)
        (dept/program)
        (dept/univ)
        (format "phone: ~a" (dept/contact-phone))
        (format "email: ~a" (dept/contact-email))))

(define (dept/program-at-univ)
  (format "~a at ~a"
          (dept/program)
          (dept/univ)))

(define (applicant-areas/string sanid)
  (apply string-append (between " , " (applicant-areas sanid))))