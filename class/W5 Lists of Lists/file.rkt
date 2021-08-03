;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname file) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
;; Imports several file I/O library functions (see Help Desk)

;; WARNING: The file IO functions in this teachpack are platform dependent.
;; That is, as long as your programs and your files live on the same platform,
;; you should not have any problems reading the files that programs wrote and vice versa.
;; If, however, one of your programs writes a file on a Windows operating system and
;; if you then copy this output file to a Mac/Linux systems, reading the copied text file
;; may produce extraneous “return” characters. Note that this describes only one example
;; of possible malfunction; there are other cases when trans-platform actions
;; may cause this library to fail.

;; An InputFilename is one of
;; -- 'standard-in
;; -- 'stdin
;; -- a String representing the name of an existing file in the current directory

;; a Line is a List-Of-String, that is, either
;; -- '()
;; -- (cons String Line)
#;
(define (los-fun los)
  (cond [(empty? los) ...]
        [(cons? los) ...
         ... (first los) ;a String
         ... (los-fun (rest los))    ]))

;; a List-of-Line [LOL] is either
;; -- '()
;; -- (cons Line LOL)
#;
(define (lol-fun lol)
  (cond [(empty? lol) ...]
        [(cons? lol)
         ... (line-fun (first lol)) ;a Line, not primitive
         ... (lol-fun (rest lol))   ]))

;; read-words/line : InputFilename --> List-of-Line
;; Consumes: InputFilename f
;; Purpose: reads the standard input device (until closed) if f is 'stdin or 'standard-in
;;          or the content of a file named f in the current directory
;;          and produces it as a list of lists, one per line;
;;          each line is represented as a list of strings.

;; an OutputFilename is one of
;; -- 'standard-out
;; -- 'stdout
;; -- a String, representing a file name

;; write-file : OutputFilename--> String
;; Consumes OutputFilename f
;;          String cntnt
;; Purpose: sends cntnt to the standard output device (f is symbol 'standard-out or 'stdout)
;;          or turns cntnt into the content of file f (if f is a String)
;;          located in the same folder (directory) as the program.
;;          If the write succeeds, the function produces the name of the file (f)
;;          otherwise it signals an error.

(define TTT (read-words/line "ttt.txt"))

(define NEWLINE "\n")

;; hyphenate : InputFilename OutputFilename --> String
;; Consumes: InputFilename infile
;;           OutputFilename outfile
;; Purpose: Read the infile and write it to the outfile, with all spaces between letters
;;          converted to hyphens "-", and preserving the current line breaks. Returns the name of the
;;          output file if sucessful, otherwise signals an error (from write-file).
(check-expect (hyphenate "ttt.txt" "out.txt") "out.txt")
(define (hyphenate infile outfile)
  (write-file outfile
              (hyphenate-all-lines
               (read-words/line infile))))

;; hyphenate-all-lines : LOL --> String
;; Consumes LOL lol, the list of lines
;; produces a single string with all spaces replaced by hyphens,
;; and the line breaks preserved.
(check-expect (hyphenate-all-lines '()) "")
(check-expect (hyphenate-all-lines (list (list "alpha" "beta")
                                         (list "gamma" "delta")))
              "alpha-beta\ngamma-delta\n")
(define (hyphenate-all-lines lol)
  (cond [(empty? lol) ""]
        [(cons? lol)
         (string-append (hyphenate-line (first lol))
                        NEWLINE
                        (hyphenate-all-lines (rest lol)))]))

;; !! hyphenate-line : Line --> String
;; Consumes Line los, a list of String
;; Produces a string with all the elements separated with hyphens
(check-expect (hyphenate-line '()) "")
(check-expect (hyphenate-line '("beta")) "beta")
(check-expect (hyphenate-line '("alpha" "beta")) "alpha-beta")
(define (hyphenate-line los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [(cons? los) 
         (string-append (first los) ;a String
                        "-"
                        (hyphenate-line (rest los)))]))
