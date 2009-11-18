#lang mzscheme

;;; Remove alarming 'lambda' keyword and replace with something more soothing
(define-syntax fluffy-niceness
  (syntax-rules ()
    ((_ x ...)
     (lambda x ...))
    )
  )

;;; We all know the list comes first
(define-syntax sane-for-each
  (syntax-rules ()
    ((_ x y)
     (for-each y x))
    )
  )

;;; Prevent cruel and unusual abuse of let keyword
(define-syntax open-curly-bracket
  (syntax-rules ()
    ((_ x ...)
     (let x ...))
    )
  )

;;; and restore Universal harmony
(define-syntax actually-let
  (syntax-rules ()
    ((_ x y z)
     (define x z)) ;;; Cunningly consume the '=' in the middle
    )
  )

;;; Remind self what self is really doing
(define-syntax function
  (syntax-rules ()
    ((_ x ...)
     (define x ...))
    )
  )

;;; OK, here we go

;;; Slurp a file into a list
(function slurp-file
          (fluffy-niceness (filename)
                           (open-curly-bracket loop ((content '()) (file 
                                                                    (open-input-file filename)))
                                               (if (eof-object? 
                                                    (peek-char file))
                                                   (begin
                                                     (close-input-port file)
                                                     content
                                                     )
                                                   (loop (append content 
                                                                 (list (read-line file))) file)
                                                   )
                                               )
                           )
          )

(function write-file
          (fluffy-niceness (filename content)
                           (begin
                             (if (file-exists? filename) (delete-file filename))
                             (open-curly-bracket ((file (open-output-file filename)))
                                                 (sane-for-each content                                                     
                                                                (fluffy-niceness (line)                                                                            
                                                                                 (fprintf file "~a~n" line)
                                                                                 
                                                                                 )
                                                                )
                                                 (close-output-port file)
                                                 )
                             )
                           )
          )

(actually-let input = (slurp-file "input.txt"))
(actually-let output = (map (fluffy-niceness (line)
                                             (regexp-replace "find" line 
                                                             "replace"))
                            input))

(write-file "output.txt" output)
