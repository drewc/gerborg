(import :drewc/smug :std/srfi/13 :std/srfi/1
         :drewc/org/environment)
 (export (import: <global>) (import: <headline>)
         (import: <affiliated-keywords>) (import: <timestamp>)
         (import: <tables>) (import: <planning>)
         (import: <drawers>) (import: <property-drawers>) (import: <emphasis>))

 (module <global>
   (export #t)
   (def (ANY-STRING . strings)
     (apply .or (map (cut .string-ci=? <>) strings)))
   (def EOF (.begin (.not (item)) (return #!eof)))
   (def WS (sat (? (and char-whitespace? (not (cut char=? #\newline <>))))))
   (def BOL
     (.let* (p (point))
      (unless p (error "Point is #f for BOL")) 
       (if (zero? p) (return #t) (peek (.begin (goto-char (- p 1)) #\newline)))))
   (def EOL (.or #\newline EOF))
   (def SKIP-LINE
     (let (S (sat (? (not (cut char=? <> #\newline)))))
        (.or (.begin (skip S) (sat (cut char=? #\newline <>)))
             (.begin (many1 S) EOF (return #!eof)))))
   (def (OPT p) (.or p (return #f)))
   (def RTRIM (cut .begin0 <> (skip WS))))
 (import <global>)

 ;; * Objects

 (module <timestamp>
   (import <global>)
   (export TIMESTAMP)
   (def SEXP (some1 (sat (? (not (cut string-any <> ">\n"))))))
   
   (def TIMESTAMP-SEXP
     (.let* (sexp (.list->string (bracket "<%%(" SEXP ")>")))
       (string-append "(" sexp ")")))
   
   (def NUM (sat char-numeric?))
   (def DAYNAME
     (.let* (_ (skip WS))
      (.list->string (many1 (sat (? (not (or char-numeric? (cut string-any <> " +-]>\n")))))))))
   
   (def DATE
     (.let* ((y (.string->number (.make-string 4 NUM))) (_ #\-)
             (m (.string->number (.make-string 2 NUM))) (_ #\-)
             (d (.string->number (.make-string 2 NUM)))
             (n (OPT DAYNAME)))
     ['date [y: y m: m d: d n: n]]))
   
   (def TIME
     (.let* ((h (.string->number
                 (.list->string (.cons NUM (.or (.list NUM) (return []))))))
             (m (.begin #\: (.string->number (.make-string 2 NUM)))))
      ['time [h: h m: m]]))
   (def MARK (.or "++" "+" ".+" "--" "-"))
   (def VALUE (.string->number (.list->string (many1 NUM))))
   (def UNIT (sat (cut string-any <> "hdwmy")))
   (def REPEATER-OR-DELAY
     (.let* ((m MARK) (v VALUE) (u UNIT)) ['repeater-or-delay [m: m v: v u: u]]))
   (def INNER-TIMESTAMP
     (.let* ((d DATE)
             (start (OPT (.begin " " (skip WS) TIME)))
             (end (OPT (.begin "-" TIME)))
             (rep (OPT (.begin " " (skip WS) REPEATER-OR-DELAY)))
             (wa (OPT (.begin " " (skip WS) REPEATER-OR-DELAY))))
       ['inner-timestamp [date: d start: start end: end repeat: rep warn: wa]]))
   
   (def TIMESTAMP
     (.let* ((b (.or #\[ #\<))
             (ts INNER-TIMESTAMP)
             (e (if (char=? #\[ b) #\] #\>))
             (range (OPT (.begin "--" (peek b) TIMESTAMP))))
     ['timestamp [start: b inner: ts range: range]])))
 (import <timestamp>)

 (module <emphasis>
   (import <global>)
   (export TEXT-MARKUP)
   (def PRE (.or (sat (? (or (cut memv <> '(#\{ #\( #\' #\"))))) WS BOL))
   (def (M c s) (.begin (.char=? c) (return (values c s))))
   (def MARKER (.or (M #\* 'bold) (M #\= 'verbatim)
                    (M #\/ 'italic) (M #\+ 'strike-through)
                    (M #\_ 'underline) (M #\~ 'code)))
   (def BORDER (sat (? (not char-whitespace?))))
   (def BODY
     (sat (lambda (cs) (> 2 (count (cut char=? #\newline <>) cs)))
          (some1 (item))))
   
   (def CONTENTS
     (.let* ((bb BORDER) (b BODY) (be BORDER))
       (set! (cdr (last-pair b)) [be])
       (cons bb b)))
   (def POST (.or (sat (cut string-any <> " -,.:!?')}\""))
                  (peek #\newline)))
   
   (def TEXT-MARKUP
     (.let* ((b (point))
             (_ (save-excursion (goto-char (1- b)) (.or (.not (item)) PRE)))
             ((values marker type) MARKER) 
             (cb (point)) (contents CONTENTS) (ce (point))
   
             (pb (.begin (.char=? marker) (peek (.or POST (.not (item))))
                         (skip-chars-forward " \t")))
             (end (point)))
       (cons* type (append [begin: b end: end]
                           (if (member type '(code verbatim)) 
                             [value: (list->string contents)]
                             [contents-begin: cb contents-end: ce])
                           [post-blank: pb])
              (if (member type '(code verbatim)) []
                  [(list->string contents)])))))
 (import <emphasis>)

;; ** Planning

 (module <planning> 
   (import <timestamp>)
   (export PLANNING-KEYWORD PLANNING)
   (def PLANNING-KEYWORD
     (bracket (skip WS)
              (.or (ci=? "DEADLINE") (ci=? "SCHEDULED") (ci=? "CLOSED"))
              ": "))
   (def (PLANNING (ts TIMESTAMP))
     (def plan (.let* ((k PLANNING-KEYWORD)
                       (t (.begin (skip WS) ts)))
                 [(string->keyword (string-downcase k))  t]))
     (.let* ((plans (many1 plan))
             (_ (.begin (skip WS) EOL)))
       (apply append plans))))
 (import <planning>)

 ;; * Headlines and Sections
 (module <headline>
   (import <global>)
   (export HEADLINE HEADLINE-ELEMENT STARS STARS-ONLY)
   (def STARS-ONLY (.begin0 (many1 #\*) #\space))
   (def (STARS (min-depth 1))
    (RTRIM (sat (lambda (stars) (>= (length stars) min-depth)) STARS-ONLY)))
   (def TODO-KEYWORD (OPT (RTRIM (apply .or (org-env-ref 'org-todo-keywords-1)))))
   (def PRIORITY (OPT (RTRIM (bracket "[#" (sat char-alphabetic?) "]"))))
   (def TITLE (some (sat (? (not (cut char=? <> #\newline))))))
   (def TAG
     ((liftP list->string)
      (many1 (.or (sat char-alphabetic?) (sat char-numeric?) #\_ #\@ #\# #\%))))
   
   (def TAGS (RTRIM (OPT (bracket " :" (sepby1 TAG ":") ":"))))
   (def COMMENT 
     (.or (.begin "COMMENT" (.or " " EOL) (skip WS) (return #t))
                   (return #f)))
   (def (footnote-section? title)
     (string=? (org-env-ref 'org-footnote-section) (list->string title))) 
   (def (archived? tags)
     (if (and tags (member (org-env-ref 'org-archive-tag) tags)) #t #f))
   (def (HEADLINE-ELEMENT min-depth: (min-depth 1) section: (sect (.read-line)))
     (.let* ((headline (HEADLINE min-depth))
             (section (.begin0 (some sect) (.or (peek STARS-ONLY) (.not (item)))))
             (subs (many (HEADLINE-ELEMENT min-depth: (1+ min-depth)
                                            section: sect))))
      (cons 'headline (append headline [section: (append section subs)]))))
   (def (HEADLINE (min-depth 1))
     (.let* ((s (STARS min-depth)) (k TODO-KEYWORD) (p PRIORITY)
             (c COMMENT)
             (title TITLE) (tags (.or (.begin0 TAGS (skip WS) EOL)
                                      (.begin EOL #f))))
       ['headline (list stars: s todo-keyword: k priority: p
                        title: (list->string title) tags: tags
                        commented?: c footnote-section?: (footnote-section? title)
                        archived?: (archived? tags))])))
 (import <headline>)

 ;; * Affiliated Keywords

 (module <affiliated-keywords>
    (import <global>)
    (export AFFILIATED-KEYWORD
            (rename: KEY AFFILIATED-KEY))
    (def KEY (ANY-STRING "CAPTION" "HEADER" "NAME" "PLOT" "RESULTS"))
    (def BACKEND
      (some1 (.or (sat char-alphabetic?) (sat char-numeric?) #\- #\_)))
    
    (def ATTR_BACKEND (.let* ((a (.string-ci=? "ATTR_")) (b BACKEND))
                        (string-append a (list->string b))))
    (def NO-EOL (sat (? (not (cut char=? #\newline <>)))))
    (def OPTIONAL (OPT (bracket #\[ (some1 NO-EOL) #\])))
    (def VALUE (OPT (some1 NO-EOL)))
    (def (AFFILIATED-KEYWORD (keyword KEY))
      (.let* ((key (.begin "#+" (.or keyword ATTR_BACKEND)))
              (opt OPTIONAL) (_ ": ") (val VALUE) (_ EOL))
             ['affiliated-keyword
              (list key: key option: (if (or (not opt) (null? opt)) #f
                                         (list->string opt))
                    value: (list->string val))
              ])))
 (import <affiliated-keywords>)

 ;; * Greater Elements

 ;; ** Tables

 (module <tables>
   (import <global>)
   (export TABLE TBLFM)
   (def CONTENTS (some1 (sat (? (not (cut member <> '(#\| #\newline)))))))
   (def FINAL (.or #\| (peek #\newline) (.not (item))))
   (def SPACES (.or (.begin0 (many1 #\space) FINAL) (.begin FINAL (return []))))
   
   (def TABLE-CELL
     (.let* ((contents CONTENTS) (spaces SPACES))
       ['table-cell [contents: (list->string contents) spaces: spaces]]))
   (def RULE-CELL
     (.let* ((contents (many #\-)) (end (.or #\+ #\|)))
            ['table-cell [type: 'rule contents: (list->string contents)]]))
   
   (def RULE-ROW
     (.let* (c (.begin #\| (peek #\-) (many1 RULE-CELL)))
       ['table-row [type: 'rule contents: c]]))
   
   (def TABLE-ROW
    (.or RULE-ROW
         (.let* (cs (.begin "|" (many TABLE-CELL)))
           ['table-row [type: 'standard contents: cs]])))
   (def TBLFM (.begin (skip WS) "#+TBLFM: " (.read-line include-newline?: #f)))
   (def TABLE-ELEMENT-ROW
     (bracket (skip WS) TABLE-ROW (.begin (skip WS) EOL)))
   (def TABLE
     (.let* ((cb (point))(spaces (many WS)) (rows (many1 TABLE-ELEMENT-ROW))
             (ce (point)) (formulas (.or (many1 TBLFM) #f)))
       ['table [type: 'org spaces: spaces contents-begin: cb contents-end: ce contents: rows tblfm: formulas]])))
 (import <tables>)

 ;; ** Drawers and Property Drawers

 (module <drawers>
   (import <global>)
    (export DRAWER-DELIMITER DRAWER-START DRAWER-END)
     (def (DRAWER-DELIMITER p)
       (bracket (.begin (skip WS) #\:) p (.begin #\: (skip WS) EOL)))
     
     (def DRAWER-START
       ((liftP list->string)
        (DRAWER-DELIMITER
         (many1 (.or (sat (? (or char-alphabetic? char-numeric?))) #\_ #\-)))))
     
     (def DRAWER-END
       (DRAWER-DELIMITER "END")))
 (import <drawers>)

 (module <property-drawers> 
   (import <global> <drawers>)++
   (export PROPERTYDRAWER PROPERTYDRAWER-START PROPERTYDRAWER-CONTENTS PROPERTYDRAWER-END)
   (def NODE-PROPERTY
     (.let* ((k (bracket (.begin (skip WS) #\:) (some1 (sat (? (not char-whitespace?)))) #\:))
             (v (bracket (skip WS) (some (sat (? (not (cut char=? <> #\newline))))) EOL)))
      ['node-property [key: (list->string k) value: (list->string v)]]))
   (def PROPERTYDRAWER-CONTENTS (some NODE-PROPERTY))
   (def PROPERTYDRAWER-START (DRAWER-DELIMITER (ci=? "PROPERTIES" #t)))
   (def PROPERTYDRAWER-END DRAWER-END)
   
   (def PROPERTYDRAWER
     (bind (bracket PROPERTYDRAWER-START PROPERTYDRAWER-CONTENTS PROPERTYDRAWER-END)
           (lambda (c) (return ['property-drawer [] . c])))))
 (import <property-drawers>)


 ;; Greater Blocks

 (module <all-blocks>
   (export #t)
   (def NAME ((liftP list->string) (many1 (sat (? (not char-whitespace?)))))))

 ;; * Elements

 ;; ** Blocks 
 (module <block> 
 (import <global>)
   (export BLOCK)
   (def BLOCK 
     (.let* ((name BEGIN_NAME) (data (DATA name)) 
                 (value (CONTENTS name)) (_ SKIP-LINE))
       [(string->symbol (string-append (string-downcase name) "-block"))
        value: (string-trim-right value)]))
   (def NAME ((liftP list->string) (many1 (sat (? (not char-whitespace?))))))
   (def BLOCK-ELEMENT-NAME (sat (cut member <> '("COMMENT" "EXAMPLE" "EXPORT" "SRC" "VERSE")
                                     string-ci=?)))
   (def BEGIN_NAME
     (.begin (.string-ci=? "#+BEGIN_")
               ))
   
   (def (END_NAME name)
     (.let* ((name (.begin (.string-ci=? "#+END_") (.string-ci=? name)))
                 (_ (peek (sat char-whitespace?))))
        name))
   
   (def (CONTENTS name)
     (let lines ((ls []))
       (.let* (l (.or (.begin (END_NAME name) (return #t))
                              (.read-line)))
                  (if (eq? #t l)
                    (return (string-concatenate (reverse! ls)))
                    (lines (cons l ls))))))
   (def (DATA name)
     (case (string->symbol (string-downcase name))
       ((export) EXPORT-DATA)
       ((src) SRC-DATA)
       (else (.read-line))))
   (def EXPORT-DATA (.let* ((_ (skip WS))
                               (type ((liftP list->string)
                                      (many1 (sat (? (not char-whitespace?))))))
                               (_ SKIP-LINE))
                      type))
   (def SRC-DATA 
    (.let* ((_ (skip WS)) (l LANGUAGE)
                (_ (skip WS)) (s (OPT SWITCHES))
                (_ (skip WS)) (p (OPT PARAMETERS)) (_ SKIP-LINE))
      [language: l switches: s parameters: p]))
   (def LANGUAGE
     ((liftP list->string) (many (sat (? (not char-whitespace?))))))
   (def SWITCHES
     (.or (sepby1
               (.or SWITCH-r
                               SWITCH-l
                               SWITCH-n
                               SWITCH-i)
               (skip WS))
              (return #f)))
   
   (def SWITCH (.let* ((sign (.or #\+ #\-)) (letter (sat char-alphabetic?)))
                 (values sign letter)))
   (def SWITCH-n
     (.let* (((values sign letter) SWITCH)
                 (arg (if (not (char=? #\n letter)) (fail)
                          (OPT (.begin " " (skip WS)
                                         ((liftP (lambda (lst)
                                                   (string->number (list->string lst))))
                                          (at-least 1 (sat char-numeric?))))))))
                ['switch letter: letter sign: sign arg: arg]))
   (def (SWITCH-char char)
     (.let* (((values s l) SWITCH))
      (if (and (char=? l char) (char=? #\- s))
        ['switch letter: l sign: s])))
   
   (def SWITCH-r (SWITCH-char #\r))
   (def SWITCH-i (SWITCH-char #\i))
   (def FORMAT
     (.begin #\"
               (.begin0 ((liftP list->string)
                             (many (sat (? (not (cut member <> '(#\" #\newline)))))))
                            #\")))
   
   (def SWITCH-l
     (.let* (((values sign letter) SWITCH)
                 (format (if (not (char=? #\l letter)) (fail)
                          (OPT (.begin " " (skip WS)
                                         FORMAT)))))
                (return ['switch letter: letter sign: sign arg: format])))
   (def PARAMETERS
     (.or 
      ((liftP list->string) (many1 (sat (? (not (cut char=? #\newline))))))
      (return #f))))
 (import <block>)
