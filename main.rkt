#lang racket/base
(require racket/match)
(require racket/gui)
(require racket/file)

(define (mkCSS n c bg k hash lS paren fN fS)
  (string-append
   "    <style>\n"
   "      body > pre {\n"
   "        color : " n " ;\n"
   "        background-color : " bg " ;\n"
   "        font-size : " fS " ;\n"
   "        font-family : \"" fN "\" ; }\n"
   "      .comment { color : " c " ; }\n"
   "      .keyword { color : " k " ; }\n"
   "      .hashTag { color : " hash " ; }\n"
   "      .litStr { color : " lS " ; }\n"
   "      .paren { color : " paren " ; }\n"
   ; what the heck I'm doing...
   "    </style>\n"))
(define (mkHeader t)
  (string-append
   "<html>\n"
   "  <head>\n"
   t
   "  </head>\n"
   "  <body>\n"
   "  <!-- Created by rkt2html. -->\n"))
(define (mkFooter)
  (string-append
   "  <!-- END -->\n"
   "  </body>"
   "</html>"))

(define keywordList
  (map symbol->string
    '(define
      lambda λ
      let letrec let*
      car cdr cons
      set!
      cond if when unless case match
      for
      module
      : -> ->*
      require provide
      display
      read
      list
      and or not
   )))
(define (isKeyword t)
  (member t keywordList))

(define (isParen t)
  (member t (string->list "(){}[]")))

(define (isHashtag t)
  (string=? "#:" (substring t 0 2)))
    
(define (parse currentToken state str hold)
  (if (string=? str "") hold
  (let ([fst (string-ref str 0)]
        [rst (substring str 1)])
    (match state
      ['inString
       (match fst
         [#\" (parse "" 'normal rst
                     (string-append hold
                                    "<font class=\"litStr\">"
                                    currentToken "\"</font>"))]
         [#\\ (parse (string-append currentToken "\\")
                     'inStringEscaped rst hold)]
         [#\< (parse (string-append currentToken "&lt;") state rst hold)]
         [#\> (parse (string-append currentToken "&gt;") state rst hold)]
         [_ (parse (string-append currentToken (make-string 1 fst))
                   state rst hold)])]
      ['inStringEscaped
       (parse (string-append currentToken (make-string 1 fst))
              'inString rst hold)]
      ['inComment
       (match fst
         [#\newline (parse "" 'normal rst
                           (string-append hold
                                          currentToken "</font><br />"))]
         [#\< (parse (string-append currentToken "&lt;") state rst hold)]
         [#\> (parse (string-append currentToken "&gt;") state rst hold)]
         [_ (parse (string-append currentToken (make-string 1 fst))
                   'inComment rst hold)])]
      ['normal
       (match fst
         [#\newline (parse "" 'normal rst
                           (string-append hold
                                          currentToken
                                          "<br />"))]
         [#\; (parse "" 'inComment rst
                     (string-append hold
                                    currentToken
                                    "<font class=\"comment\">;"))]
         [#\" (if (string=? "#\\" currentToken)
                  (parse (string-append currentToken "\"") 'normal rst hold)
                  (parse "\"" 'inString rst (string-append hold currentToken)))]
         [#\space
          (cond
            ((isKeyword currentToken)
             (parse "" 'normal rst
                    (string-append hold
                                   "<font class=\"keyword\">"
                                   currentToken
                                   "</font> ")))
            ((and (>= (string-length currentToken) 2) (isHashtag currentToken))
             ; the phrase above needs fixing
             (parse "" 'normal rst
                    (string-append hold
                                   "<font class=\"hashTag\">"
                                   currentToken
                                   "</font> ")))
            (else
             (parse "" 'normal rst
                     (string-append hold currentToken " "))))]
         [#\< (parse (string-append currentToken "&lt;") state rst hold)]
         [#\> (parse (string-append currentToken "&gt;") state rst hold)]
         [_
          (if (isParen fst)
              (parse "" 'normal rst
                   (string-append hold currentToken
                                  "<font class=\"paren\">"
                                  (make-string 1 fst)
                                  "</font>"))
              (parse (string-append currentToken (make-string 1 fst))
                     'normal rst hold))])]))))
; todo: clean up this mess

(define frmConfig
  (new frame%
       [label "Configuration..."]))
(define lblColor
  (new message%
       [label "These fields accept any format that is accepted in CSS."]
       [parent frmConfig]))
(define tfNormal
  (new text-field%
       [label "Normal"] [parent frmConfig]
       [init-value "#000000"]))
(define tfComment
  (new text-field%
       [label "Comment"] [parent frmConfig]
       [init-value "#003300"]))
(define tfBackground
  (new text-field%
       [label "Background"] [parent frmConfig]
       [init-value "#FFFFFF"]))
(define tfKeyword
  (new text-field%
       [label "Keyword"] [parent frmConfig]
       [init-value "#0000FF"]))
(define tfHashtag
  (new text-field%
       [label "Hashtag"] [parent frmConfig]
       [init-value "#FF0000"]))
(define tfLitStr
  (new text-field%
       [label "String"] [parent frmConfig]
       [init-value "#00DD00"]))
(define tfParen
  (new text-field%
       [label "Parenthesis"] [parent frmConfig]
       [init-value "#FF0000"]))
(define tfFontName
  (new text-field%
       [label "FontName (e.g. courier)"] [parent frmConfig]
       [init-value "courier"]))
(define tfFontSize
  (new text-field%
       [label "FontSize (e.g. 12pt)"] [parent frmConfig]
       [init-value "12pt"]))
(define btn
  (new button%
       [label "Save configuration"] [parent frmConfig]
       [callback (lambda (btn e) (saveConfig))]))
(define (saveConfig)
  (let ([outf (open-output-file "rkt2html.cfg"
                                #:mode 'text #:exists 'replace)])
    (display
     (string-append "("
                    "\"" (send tfNormal get-value) "\" "
                    "\"" (send tfComment get-value) "\" "
                    "\"" (send tfBackground get-value) "\" "
                    "\"" (send tfKeyword get-value) "\" "
                    "\"" (send tfHashtag get-value) "\" "
                    "\"" (send tfLitStr get-value) "\" "
                    "\"" (send tfParen get-value) "\" "
                    "\"" (send tfFontName get-value) "\" "
                    "\"" (send tfFontSize get-value) "\" "
                    ")")
     outf)))

(define (outputFileNameOf t) (string-append t ".html"))

(define (output fileN)
  (let ([config (begin
                  (unless (file-exists? "rkt2html.cfg") (saveConfig))
                  (file->value "rkt2html.cfg"))]
        [str (port->string (open-input-file fileN #:mode 'text))])
    (display (string-append
              (mkHeader (apply mkCSS config))
              "<pre>\n" (parse "" 'normal (string-append str "\n") "")
              "\n</pre>\n"
              (mkFooter))
             (open-output-file (outputFileNameOf fileN)
                               #:mode 'text #:exists 'replace))))

(define (showConfigFrm)
  (begin
    (when (file-exists? "rkt2html.cfg")
      (let ([val (file->value "rkt2html.cfg")])
        (begin
          (send tfNormal set-value (first val))
          (send tfComment set-value (second val))
          (send tfBackground set-value (third val))
          (send tfKeyword set-value (fourth val))
          (send tfHashtag set-value (fifth val))
          (send tfLitStr set-value (sixth val))
          (send tfParen set-value (seventh val))
          (send tfFontName set-value (eighth val))
          (send tfFontSize set-value (ninth val)))))
    (send frmConfig show #T)))

(let ([args (current-command-line-arguments)])
  (begin
    (display "rkt2html by Brad Contenary\n")
    (display "rkt2html [filename] - export source to html file\n")
    (display "rkt2html --config   - configure the output style\n")
    (if (= (vector-length args) 1)
        (cond ((string=? (vector-ref args 0) "--config")
               (showConfigFrm))
              (else (output (vector-ref args 0))))
        (error "Wrong arguments.\n"))))
