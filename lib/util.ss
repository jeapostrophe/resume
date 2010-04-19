#lang web-server
(require (lib "list.ss")
         (lib "string.ss")
         (only-in (lib "file.ss") make-directory* explode-path))
(require (planet "list.ss" ("jaymccarthy" "mmss.plt" 1)))
(provide (all-defined-out))

(define (last-name person)
  (car (last-pair (filter
                   (lambda (s) (> (string-length s) 0))
                   (regexp-split
                    " "
                    (cadr (regexp-match
                           "([^,()]*)"
                           person)))))))

(define last-name-ci<=? (<=/proj string-ci<=? last-name))

(define (dirname p)
  (apply build-path
         (reverse (rest (reverse (explode-path (build-path p)))))))

(define (sanitize-string s)
  (define (sanitize-char c)
    (cond
      [(char-alphabetic? c) (char-downcase c)]
      [(char-numeric? c) c]
      [else #\-]))
  (list->string (map sanitize-char (string->list s))))

(define (bindings-replace b k v)
  (list* (list k v)
         (filter (lambda (k*v) (not (eq? (car k*v) k)))
                 b)))

(define (read*)
  (let loop ([r empty])
    (let ([v (read)])
      (if (eof-object? v)
          (reverse r)
          (loop (list* v r))))))

(define (lowercase-string s)
  (list->string (map char-downcase (string->list s))))

(define (strip-leading/trailing-space&quote s)
  (define (strip-leading-spaces&quotes lc)
    (if (null? lc)
        lc
        (if (or (char-whitespace? (car lc))
                (char=? (car lc) #\"))
            (strip-leading-spaces&quotes (cdr lc))
            lc)))
  (list->string
   (reverse
    (strip-leading-spaces&quotes
     (reverse
      (strip-leading-spaces&quotes (string->list s)))))))

;; assumes s contains only letters and numbers; ignores case
(define (military-alphabet s)
  (define (code-for c)
    (case c
      [(#\a #\A) "alpha"]  [(#\b #\B) "bravo"]    [(#\c #\C) "charlie"]
      [(#\d #\D) "delta"]  [(#\e #\E) "echo"]     [(#\f #\F) "foxtrot"]
      [(#\g #\G) "golf"]   [(#\h #\H) "hotel"]    [(#\i #\I) "india"]
      [(#\j #\J) "juliet"] [(#\k #\K) "kilo"]     [(#\l #\L) "lima"]
      [(#\m #\M) "mike"]   [(#\n #\N) "november"] [(#\o #\O) "oscar"]
      [(#\p #\P) "papa"]   [(#\q #\Q) "quebec"]   [(#\r #\R) "romeo"]
      [(#\s #\S) "sierra"] [(#\t #\T) "tango"]    [(#\u #\U) "uniform"]
      [(#\v #\V) "victor"] [(#\w #\W) "whiskey"]  [(#\x #\X) "xray"]
      [(#\y #\Y) "yankee"] [(#\z #\Z) "zulu"]
      [(#\1) "one"]   [(#\2) "two"]   [(#\3) "three"] [(#\4) "four"]
      [(#\5) "five"]  [(#\6) "six"]   [(#\7) "seven"] [(#\8) "eight"]
      [(#\9) "nine"]  [(#\0) "zero"]))
  (apply string-append
         (list-with-separator (map code-for (string->list s))
                              "")))

(define (pdf-document? file-bytes)
  (and (> (bytes-length file-bytes) 4)
       (bytes=? (subbytes file-bytes 0 4) #"%PDF")))

(define (no-@-in-email? s)
  (not (ormap (lambda (c) (char=? c #\@)) (string->list s))))

(define sanitize-url
  (let* ([http:// "http://"]
         [l (string-length http://)])
    (lambda (url)
      (if (< (string-length url) l)
          (string-append http:// url)
          (if (string-ci=? (substring url 0 l) http://)
              url
              (string-append http:// url))))))

(define (round-to n places)
  (/ (truncate (* (expt 10 places) (exact->inexact n)))
     (expt 10 places)))

(define (list-with-separator l if-none-string)
  (if (null? l)
      (list if-none-string)
      (let loop ([l l])
        (if (null? (cdr l))
            l
            (cons (car l)
                  (cons " - "
                        (loop (cdr l))))))))