#lang racket

(require rackunit)
(require csc151)
(require racket/dict)
(require racket/match)
(require racket/undefined)



;;; (add-space (x y) -> string?
;;;      x : string?
;;;      y : string?
;;;
;;; Takes in two strings and appends them with a space between them.
;;; Returns a string. Intended to be used in conjunction with reduce.
(define add-space
  (lambda (x y)
    (string-append x " " y)))

;;; (add-line (x y) -> string?
;;;      x : string?
;;;      y : string?
;;;
;;; Takes in two strings and appends them with a line-break between them.
;;; Returns a string. Intended to be used in conjunction with reduce.
(define add-line
  (lambda (x y)
    (string-append x "\n" y)))

;;; (add-quotes (x y) -> string?
;;;      x : string?
;;;      y : string?
;;;
;;; Takes in a string and appends it with a quotation
;;; mark on either side. For use before reduce.
(define add-quotes
  (lambda (x)
    (string-append "\"" x "\"")))

;;; (not-from? str) -> boolean?
;;;         str : string?
;;;
;;; Takes in a string. Returns #t if the
;;; string does not begin with "from" or "From",
;;; otherwise returns #f.
(define not-from?
  (lambda (str)
    (not (string-prefix? (string-downcase str) "from"))))

;;;  (not-empty? str) -> boolean?
;;;       str : string?
;;; 
;;; Takes in a string.
;;; returns true if the string is not empty,
;;; otherwise returns false.
(define not-empty?
  (lambda (str)
    (not (equal? "" str))))


;;; unwanted things in general...
(define words?
  (lambda (str)
    (not (or
          (equal? "" str)
          (string-contains? str "https")
          (string-contains? str "Unit")
          (string-contains? str "Sources")
          (string-contains? str "Words")
          (string-contains? str "=")))))




;;; (strip-chars str lst) -> string?
;;;        strs : string?
;;;        lst : list?
;;;
;;; Takes in a string and a list of characters in the string 
(define strip-chars
  (lambda (strs lst to)
    (if (null? lst)
        strs
        (strip-chars (string-replace strs (car lst) to) (cdr lst) to))))

;;; method to identify lines with verbs
;;; I figure that only verbs will have "ть " in them,
;;; and "ться " is a lot less common, I think
(define verb?
  (lambda (str)
    (or (string-contains? str "ть ")
        (string-contains? str "ться "))))

;;; method to produce name of splitting section
(define name
  (lambda (num)
    (string-append "Unit " (number->string num) ":")))



;;; method to return the words between two units
;;; returns a string
(define units
  (lambda (words start end)
    (car (string-split
          (cadr (string-split words (name start)))
          (name (+ end 1))))))

;;; method to format and return the words between units as a string
(define format-words
  (lambda (str)
    (reduce add-line
            (filter words?
                    (string-split str "\r\n")))))

(define format-verbs
  (lambda (str)
    (reduce add-line
            (filter verb?
                    (filter words?
                            (string-split str "\r\n"))))))


;; https://docs.racket-lang.org/guide/keywords.html
;;; method to write a file for certain units
(define words->file
  (lambda (words start end)
    (display-to-file
     (format-words (units words start end))
     (string-append "/Users"
                    "/sarah"
                    "/OneDrive"
                    "/Documents"
                    "/2nd Year Grinnell"
                    "/Intermediate Russian"
                    "/Vocab Lists"
                    "/RussianVocab"
                    "Units"
                    (number->string start)
                    "-"
                    (number->string end)
                    ".txt")
     #:mode 'text
     #:exists 'replace)))


;; https://docs.racket-lang.org/guide/keywords.html
;;; method to write a file for certain units' verbs
(define verbs->file
  (lambda (words start end)
    (display-to-file
     (format-verbs (units words start end))
     (string-append "/Users"
                    "/sarah"
                    "/OneDrive"
                    "/Documents"
                    "/2nd Year Grinnell"
                    "/Intermediate Russian"
                    "/Vocab Lists"
                    "/RussianVocab"
                    "Units"
                    (number->string start)
                    "-"
                    (number->string end)
                    "VerbsOnly.txt")
     #:mode 'text
     #:exists 'replace)))


;;; method that takes in a word
(define iterate-verb-helper
  (lambda (words start end)
    (if (equal? start end)
        (cons (format-verbs (units words start end)) '())
        (cons (format-verbs (units words start end)) (iterate-verb-helper words (+ start 1) end)))))

(define iterate-word-helper
  (lambda (words start end)
    (if (equal? start end)
        (cons (format-words (units words start end)) '())
        (cons (format-words (units words start end)) (iterate-word-helper words (+ start 1) end)))))

;(define iterate-words
; (lambda (words start end)
;  (let ([iterate-word-helper (lambda (words start end))
;                            (if (equal? start end)
;                               (cons (format-words (units words start end)) '())
;                              (cons (format-words (units words start end)) (iterate-word-helper words (+ start 1) end)))])
; )))
      



(define iterate-all
  (lambda (words start end)
    (cond
      [(equal? start end) (words->file words start end) (verbs->file words start end)]
      [else (words->file words start end) (verbs->file words start end) (iterate-all words start (- end 1))])))


(define combine
  (lambda (words start end)
    (cond
      [(equal? start end) (iterate-all words start end)]
      [else (iterate-all words start end) (combine words (+ start 1) end)]))) 





(define raw-file (file->string "/Users/sarah/OneDrive/Documents/2nd Year Grinnell/Intermediate Russian/Vocab Lists/RussianVocabData.txt"))








