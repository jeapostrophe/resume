#lang web-server
(require (lib "plt-match.ss"))
(provide (all-defined-out))

(define-struct an-identity () #:prefab)
(define-struct (identity:reference an-identity) (refcode) #:prefab)
(define-struct (identity:internal an-identity) (csid) #:prefab)
(define-struct (identity:applicant an-identity) (id) #:prefab)

(define current-identity (make-web-cell #f))
