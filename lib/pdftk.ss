#lang web-server
(require (lib "process.ss")
         (lib "list.ss")
         (only-in (lib "file.ss") make-directory*)
         (planet "hash-store.ss" ("jaymccarthy" "hash-store.plt" 1)))
(require "../lib/resume-lib.ss"
         "../lib/util.ss"
         "../lib/resume-data.ss"
         "../lib/resume-data-util.ss")
(provide update-aggregate!
         create-multi-applicant-aggregate!)

(define (merge new . olds)
  (apply process*/ports 
         (current-output-port) (open-input-string (string)) (current-error-port)
         (pdftk-path)
         (append olds
                 (list "cat" "output" new))))

(define (create-multi-applicant-aggregate! sanids letters?)
  (define filename
    (string-append
     (bytes->string/utf-8
      (SHA1
       (string->bytes/utf-8
        (apply string-append (if letters? "yes" "no") sanids))))
     ".pdf"))
  (define path (string-append (aggregate/public-directory) filename))
  (make-directory* (aggregate/public-directory))
  (apply merge path
         (map (lambda (sanid)
                (unless (and (file-exists? (applicant/aggregate-tex sanid))
                             (file-exists? (applicant/aggregate-letters-tex sanid)))
                  (update-aggregate! sanid #t)
                  (update-aggregate! sanid #f))
                (if letters?
                    (applicant/aggregate-letters sanid)
                    (applicant/aggregate sanid)))
              sanids))
  path)

(define (aggregate-latex sanid letters?)
  (string-append
   (format #<<END
\documentclass{article}
\usepackage{type1cm} 
\usepackage{graphics}
\usepackage{pdfpages}
\usepackage[pdftex]{hyperref}
\pdfoptionpdfminorversion=7
\hypersetup{
pdfauthor = {Resume},
pdftitle = {~a ~a},
pdfsubject = {Faculty Applications},
pdfkeywords = {~a}}
\begin{document}
\pdfbookmark[1]{~a ~a}{~a}
\begin{center}
{\bf \fontsize{150}{50}\selectfont X} \\
~~ \\
~~ \\
~~ \\
~~ \\
~~ \\
~~ \\
{\bf \fontsize{60}{50}\selectfont ~a ~a} \\
~~ \\
~~ \\
~~ \\
~~ \\
~~ \\
~~ \\
{\bf \fontsize{150}{50}\selectfont X}
\end{center}
\newpage

END
           (applicant-fname sanid) (applicant-lname sanid)             
           (applicant-areas/string sanid)
           (applicant-fname sanid) (applicant-lname sanid)
           sanid
           (applicant-fname sanid) (applicant-lname sanid))
   (apply string-append
          (map (lambda (l f)
                 (format #<<END
\pdfbookmark[2]{~a}{~a_ts}
\includepdf[pages=-]{~a}

END
                         l sanid (f sanid)))
               (list "Cover Letter"
                     "CV"
                     "Research Statement"
                     "Teaching Statement")
               (list applicant/cover-letter 
                     applicant/cv
                     applicant/research-stmt
                     applicant/teaching-stmt)))
   (if (not letters?)
       ""
       (string-append
        (format #<<END
\pdfbookmark[2]{Letter Writers}{~a_lw}

END
                sanid)
        (apply string-append
               (map (lambda (refcode)
                      (if (file-exists? (applicant/reference-letter sanid refcode))
                          (format
                           #<<END
\pdfbookmark[3]{~a}{~a}
\includepdf[pages=-]{~a}

END
                           (applicant-letter-writer-name sanid refcode)
                           refcode (applicant/reference-letter sanid refcode))
                          ""))
                    (applicant-letter-writers sanid)))))
   #<<END
\end{document}

END
   ))

(define (update-aggregate! sanid letters?)
  (define filename
    (if letters?
        (applicant/aggregate-letters-tex sanid)
        (applicant/aggregate-tex sanid)))
  (with-output-to-file filename
    (lambda ()
      (display (aggregate-latex sanid letters?)))
    #:exists 'truncate/replace)
  (parameterize ([current-directory (applicant/public-directory sanid)])
    (system* (pdflatex-path) filename)
    (system* (pdflatex-path) filename)))