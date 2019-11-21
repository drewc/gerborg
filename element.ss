;; (c) drewc <me@drewc.ca> All Rights Reserved
(import :drewc/smug 
        :drewc/org/syntax :drewc/org/environment :drewc/org/location
        :std/srfi/13 :std/srfi/1 :gerbil/gambit/exact :std/misc/list :std/iter)
(export #t)

(def all-elements
  '(babel-call center-block clock comment
    comment-block diary-sexp drawer dynamic-block example-block export-block
    fixed-width footnote-definition headline horizontal-rule inlinetask item keyword
    latex-environment node-property paragraph plain-list planning property-drawer
    quote-block section special-block src-block table table-row verse-block))

(def (org-element-element? el) (and (pair? el) (memq (car el) all-elements) #t))

(def greater-elements
  '(center-block drawer dynamic-block footnote-definition headline inlinetask
    item plain-list property-drawer quote-block section special-block table))

(def (org-element-greater-element? el)
  (and (memq (if (symbol? el) el (car el)) greater-elements)  #t))


(def all-objects
    '(bold code entity export-snippet footnote-reference inline-babel-call
  inline-src-block italic line-break latex-fragment link macro radio-target
  statistics-cookie strike-through subscript superscript table-cell target
  timestamp underline verbatim))

(def (org-element-object? el) (and (pair? el) (memq (car el) all-objects) #t))

(def org-element? (? (or org-element-element? org-element-object?)))

(def object-restrictions

  ;;  Alist of objects restrictions.

  ;; key is an element or object type containing objects and value is
  ;; a list of types that can be contained within an element or object
  ;; of such type.

  ;; For example, in a `radio-target' object, one can only find
  ;; entities, latex-fragments, subscript, superscript and text
  ;; markup.

  ;; This alist also applies to secondary string.  For example, an
  ;; `headline' type element doesn't directly contain objects, but
  ;; still has an entry since one of its properties (`:title') does.

  (let* ((standard-set (remq 'table-cell all-objects))
         (standard-set-no-line-break (remq 'line-break standard-set)))
    `((bold ,@standard-set)
      (footnote-reference ,@standard-set)
      (headline ,@standard-set-no-line-break)
      (inlinetask ,@standard-set-no-line-break)
      (italic ,@standard-set)
      (item ,@standard-set-no-line-break)
      (keyword ,@(remq 'footnote-reference standard-set))
      ;; Ignore all links in a link description.  Also ignore
      ;; radio-targets and line breaks.
      (link bold code entity export-snippet inline-babel-call inline-src-block
            italic latex-fragment macro statistics-cookie strike-through
            subscript superscript underline verbatim)
      (paragraph ,@standard-set)
      ;; Remove any variable object from radio target as it would
      ;; prevent it from being properly recognized.
      (radio-target bold code entity italic latex-fragment strike-through
                    subscript superscript underline superscript)
      (strike-through ,@standard-set)
      (subscript ,@standard-set)
      (superscript ,@standard-set)
      ;; Ignore inline babel call and inline source block as formulas
      ;; are possible.  Also ignore line breaks and statistics
      ;; cookies.
      (table-cell bold code entity export-snippet footnote-reference italic
                  latex-fragment link macro radio-target strike-through
                  subscript superscript target timestamp underline verbatim)
      (table-row table-cell)
      (underline ,@standard-set)
      (verse-block ,@standard-set))))

(def (org-element-object-restrictions el)
  (cdr (assq (if (symbol? el) el (org-element-type el)) object-restrictions)))

 ;; "List of recursive object types."

(def recursive-objects
 '(bold footnote-reference italic link subscript radio-target strike-through
   superscript table-cell underline))

(def (org-element-recursive-object? el)
  (and (memq (if (symbol? el) el (car el)) recursive-objects)  #t))

(def secondary-value-alist
  '((headline title:)
    (inlinetask title:)
    (item tag:)))

(def (org-element-secondary-value el)
  (let (key (assgetq (org-element-type el) secondary-value-alist))
    (and key (org-element-property key el))))

(def parsed-properties-alist
  '(("CAPTION" . caption:)))



(def plain-text-properties-table (make-hash-table-eq weak-keys: #t))

(def (plain-text-property prop plain-text)
  (pgetq prop (cadr (hash-ref plain-text-properties-table plain-text ['plain-text []]))))

(def (plain-text-property-set! prop text value)
  (let (element (hash-ref plain-text-properties-table text #f))
    (begin0 text
    (if (not element)
      (hash-put! plain-text-properties-table text ['plain-text [prop value]])
      (set! (org-element-property prop element) value)))))


(def (org-element-type el)
  (cond ((string? el) 'plain-text) ((org-element? el) (car el)) (#t #f)))

(def (org-element-property prop el)
  (if (string? el) (plain-text-property prop el)) (pgetq prop (cadr el)))
(def (org-element-contents el) 
  (let (c (if (string? el) [] (cddr el))) (if (null? c) #f c)))

(def (org-element-contents-set! el contents)
  (for (c contents) (set! (org-element-property parent: c) el))
  (begin0 el (set-cdr! (cdr el) contents)))

(def (org-element-map data types fn
                      info: (info '())
                      first-match: (first? #f)
                      no-recursion: (no-recursions '())
                      with-affiliated: (with-affiliated? #t))
  ;; Should we map this element?
  (def (map-type? type) (or (eq? types #t) (memq type types)))

  (def (granularity)
    (let/cc found
      (let ((gran 'greater-elements)
            (all-objects (cons 'plain-text all-objects)))
        (if (eq? types #t) 'objects
            (for (type types) (cond ((memq type all-objects) (found 'objects))
                                    ((not (org-element-greater-element? type))
                                     (set! gran 'elements)))))
        (found gran))))



  (def results [])

  (let ((granulatity (granularity))
        (no-recursion (if (symbol? no-recursions) (list no-recursions) no-recursions)))
    (let/cc first-match

      (let walk-tree ((d data))
        (let ((type (org-element-type d)))
          (cond
           ((or (null? d) (not d)) #f)
           ;; A list (like o-e-contents returns)
           ((and (not type) (list? d)) (for (d d) (walk-tree d)))
           ;; If it's a parse-tree (aka (org-data [] contents ...)), walk the contents
           ((eq? type 'org-data) (walk-tree (org-element-contents d)))
           ((not type) (error "No element type for " d))
           (#t
            (let (el d)
              ;; If we map this type, call the fn
              (when (map-type? type)
                (let (r (fn el))
                  (and r (if first? (.begin (set! results r) (first-match))
                             (push! r results)))))

              ;; If this type has a secondary string, walk it.
              (cond ((and (eq? granularity 'objects) (org-element-secondary-value el))
                     => (cut walk-tree <>)))

              ;; If there's a keyword that has objects, and ~with-affiliated~ says
              ;; to walk them, walk it.

              (when (and with-affiliated? (eq? granularity 'objects)
                         (or (org-element-element? el)    
                             (org-element-greater-element? el))) 
                (for ([name . key] parsed-properties-alist)      
                  (let (val (org-element-property key el))       
                    (and val (not (void? val))                                 
                         (cond                                   
                          ;; Ok, if this is a dual-keyword, that means that its 
                          ;; value is (cons x y), were the first is last. 
                          ((memq key dual-keywords) 
                           ;; If it's a multiple, we parse it as a list where 
                           ;; last comes first. ;
                           (if (memq key multiple-keywords)       
                             (for ([y . x] (reverse val))         
                               (walk-tree x) (walk-tree y))       
                             (match val ([y . x] (walk-tree x) (walk-tree y))))) 
                          ;; If it's a multiple, we parse it as a list where 
                          ;; last comes first 
                          ((memq key multiple-keywords) (walk-tree (reverse val))) 
                          ;; Otherwise, just walk it ;
                          (#t (walk-tree val)))))))

              ;; Now, should we recurse?
              (unless (or 
                        ;; If there's no recursion specficically 
                        (memq type no-recursion)  
                        ;; or no contents 
                        (not (org-element-contents el)) 
                        ;; Or we're not going that far 
                        (and (eq? granularity 'greater-elements) 
                             (not (org-element-greater-element? el))) 
                        ;; Like, we want elements, but this is not one 
                        (and (eq? granularity 'elements) 
                             (not (org-element-element? el))))
                (walk-tree (org-element-contents el))))))))))

    ;; we've walked it, return the results
    (if (list? results) (reverse results) results))



(def (org-element-property-set! prop el value)
  (def (%set!)
    (def props (let lp ((ps (cadr el)))
               (cond ((null? ps) [])
                     ((eq? prop (car ps))
                      (lp (cddr ps)))
                     (#t 
                      (cons* (first ps) (second ps) (lp (cddr ps)))))))
    (set-car! (cdr el) (cons* prop value props)))

  (if (string? el) (plain-text-property-set! prop el value)
      (%set!)))



;; * Affiliated Keywords

;; Each element can optionally get some more from affiliated keywords, namely:
;; ~caption:~, ~header:~, ~name:~, ~plot:~, ~results:~ or ~attr_NAME:~ where
;; =NAME= stands for the name of an export back-end.

(defconst affiliated-keywords
  '("CAPTION" "DATA" "HEADER" "HEADERS" "LABEL" "NAME" "PLOT" "RESNAME" "RESULT"
    "RESULTS" "SOURCE" "SRCNAME" "TBLNAME"))

;; The key is the old name and the value the new one.")
(defconst keyword-translation-alist
  '(("DATA" . "NAME")  ("LABEL" . "NAME") ("RESNAME" . "NAME")
    ("SOURCE" . "NAME") ("SRCNAME" . "NAME") ("TBLNAME" . "NAME")
    ("RESULT" . "RESULTS") ("HEADERS" . "HEADER")))

;;  Affiliated keywords can occur more than once in an element. By default, all
;;  keywords setting attributes (e.g., "ATTR_LATEX") allow multiple occurrences.
(defconst multiple-keywords '("CAPTION" "HEADER"))

;; Affiliated keywords whose value can be parsed.
(defconst parsed-keywords '("CAPTION"))

;; Affiliated keywords can have a secondary[value].
(defconst dual-keywords '("CAPTION" "RESULTS"))

(def (collect-affiliated-keywords (limit +inf.0))
   ;; => /list/
  (def KEY (apply .any (map ci=? affiliated-keywords)))
  (def (afks (alist []))
    (.or 
     (.let*
      ( ;; make sure we're before the limit
       (_ (sat (cut < <> limit) (point)))
       ;; Find the afk
       (afk (AFFILIATED-KEYWORD KEY)))
      (let* ( ;; Take the keyword out of it
             (afkey (org-element-property key: afk))
             ;; Make sure we match the modern key
             (key (or (assget afkey keyword-translation-alist) afkey))
             ;; Now the value
             (val (org-element-property value: afk))
             ;; If we're parsed, parse!
             (restrict (org-element-object-restrictions 'keyword))
             (parse? (member key parsed-keywords))
             (val (if parse?
                      (run (parse-objects 0 +inf.0 #f restrict) val)
                    val))
             ;; If ~key~ is a dual keyword, find its secondary value.
             (dual? (member key dual-keywords))
             (dual-val (and dual? (org-element-property option: afk)))
             ;;Maybe parse it.
             (dual-val
              (and dual-val
                   (if (not parse?) dual-val
                       (run (parse-objects 0 +inf.0 #f restrict) dual-val))))
             ;; And add it to the value
             (val (if (and dual? (or val dual-val)) (cons val dual-val) val))
             ;; Now, if this is one that can have many values, and one exists,
             ;; we'll cons it up.
             (val (if (or (member key multiple-keywords)
                          (string= key "ATTR_" 0 5))
                    (let (ac (assoc key alist))
                      (if (not ac) val (cons val (let (r (cdr ac))
                                                   (if (list? r) r (list r))))))
                    val))
             ;; name a new alist with this new key/val
             (new-alist (cons (cons key val) (alist-delete key alist))))
        ;; now call us again
        (afks new-alist)))

     (if (null? alist)
       (return #f)
       (return (append-map (lambda (ac) (list (string->keyword (string-downcase (car ac)))
                                   (cdr ac))) alist)))))

  (.let* ((b (point)) (lst (afks))) (if lst (cons b lst) [])))

(def (table-parser (affiliated []) (granularity 'greater-element))
  ;; ~affiliated~ is a list of which ~car~ is the buffer position at the
  ;; beginning of the first affiliated keyword and ~cdr~ is a plist of
  ;; affiliated keywords along with their value.

  ;; If ~table-row~ is a parser, use that and return contents. This saves time
  ;; and effort for big tables when we actually want the lines.

  ;; A | followed by anything that is not WS marks a table line

  (def TABLE-ROW
     (if (eq? granularity 'greater-element)
       (.begin (skip WS) #\| (skip WS)
               (sat (? (not char-whitespace?))) SKIP-LINE)
       (table-row-parser granularity)))

  (.let* ((b (if (not (null? affiliated)) (return (car affiliated)) (point)))
          (table-begin (point))
          (contents (many1 TABLE-ROW))
          (table-end (point))
          (tblfm (.or (many1 TBLFM) #f))
          (pos-before-blank (point))
          (blanks (many (.begin (many WS) #\newline)))
          (end (point))
          (afks (return (if (pair? affiliated) (cdr affiliated) []))))
    (let (el ['table (cons* begin: b end: end type: 'org tblfm: tblfm
                           contents-begin: table-begin contents-end: table-end
                           ;;; emacs tables get a value
                           value: #f
                           post-blank: (length blanks)
                           post-affiliated: table-begin
                           afks)])

       (begin0 (return el)
         (unless (eq? granularity 'greater-element)
           (set! (org-element-contents el) contents))))))
(def (table-row-parser  (granularity 'element))
  (.let* ((beg (point))
          (cbeg (.begin (skip WS) #\| (point)))
          (type (.or (.begin (sat (? (cut char=? #\- <>)) (peek))
                             (return 'rule))
                     (return 'standard)))
          (lend (end-of-line))
          (cend (save-excursion
                 (skip-chars-backward " \t")
                 (point)))
          (end (.begin (.or (item) EOF) (point)))
          (row (return ['table-row
                        (list
                         type: type begin: beg end: end
                         contents-begin: cbeg contents-end: cend
                         post-blank: 0 post-affiliated: beg)]))
          (contents (if (not (memq granularity '(object #f))) #f
                        (parse-objects cbeg cend row [(if (eq? type 'rule)
                                                        'table-cell-rule
                                                        'table-cell)]))))
    (return row)))

(def (table-cell-parser (type 'standard))
  (def STANDARD-CONTENTS (some (sat (? (not (cut member <> '(#\| #\newline)))))))
  (def RULE-CONTENTS (many (sat (cut char=? #\- <>))))

  (.let* ((beg (point))
          (cbeg (.begin (skip WS) (point)))
          (contents (if (eq? type 'standard) STANDARD-CONTENTS RULE-CONTENTS))
          (cend (point))
          (end (.begin (skip WS)
                        (.or (if (eq? type 'standard) #\| (.or #\+ #\|))
                             (peek EOL))
                        (point))))
    (if (= beg end) (fail)
        ['table-cell (list begin: beg end: end
                           contents-begin: cbeg contents-end: cend)])))

(def (timestamp-parser)
  (def (stamp-type stamp)
    (let* ((start (org-element-property start: stamp))
           (name (if (char=? start #\<) "active" "inactive"))
           (range (org-element-property range: stamp))
           (ts (org-element-property inner: stamp))
           (name (if (or range (org-element-property end: ts))
                   (string-append name "-range") name)))
      (string->symbol name)))
  (def (repeater/warning-type r)
   ;; MARK is ~+~ (cumulate type), ~++~ (catch-up type) or ~.+~ (restart type)
   ;; for a repeater, and ~-~ (all type) or ~--~ (first type) for warning
   ;; delays.
    (case (string->symbol (org-element-property m: r))
      ((+) 'cumulate) ((++) 'catch-up) ((.+) 'restart)
      ((-) 'all) ((--) 'first)))

  (def (warning? r)
    (and r (member (repeater/warning-type r) '(all first))))
  (def (repeater? r)
    (and r (not (warning? r))))

  (def (rep/warn-props r)
    (def name (if (warning? r) "warning-" "repeater-"))
    (def (key n) (string->keyword (string-append name n)))

    (if (not r) []
        (list (key "type") (repeater/warning-type r)
              ;; UNIT is a character among ~h~ (hour), ~d~ (day), ~w~ (week),
              ;; ~m~ (month), ~y~ (year).
              (key "unit") (case (string->symbol
                                  (string (org-element-property u: r)))
                             ((h) 'hour) ((d) 'day) ((w) 'week)
                             ((m) 'month) ((y) 'year))
              (key "value") (org-element-property v: r))))


  (.let* ((b (point)) (stamp TIMESTAMP) (e (point))
          (raw-value (buffer-substring b e)))
   (let* ((ts (org-element-property inner: stamp))
          (type (stamp-type stamp))
          (sd (org-element-property date: ts))
          (ys (org-element-property y: sd))
          (mos (org-element-property m: sd)) 
          (ds (org-element-property d: sd)) 
          (st (org-element-property start: ts)) 
          (hs (and st (org-element-property h: st)))
          (mis (and st (org-element-property m: st)))
          (range (org-element-property range: stamp))
          (rts (and range (org-element-property inner: range)))
          (et (or (and rts (org-element-property start: rts))
                  (org-element-property end: ts)
                  st))         
          (range (if rts (org-element-property date: rts) sd))
          (ye (org-element-property y: range)) 
          (moe (org-element-property m: range))
          (de (org-element-property d: range))
          (he (and et (org-element-property h: et)))
          (mie (and et (org-element-property m: et)))
          (rep/warn (list  (org-element-property repeat: ts)
                            (org-element-property warn: ts)
                            (and rts (org-element-property repeat: rts))
                            (and rts (org-element-property warn: rts))))
          (repeater (find repeater? rep/warn))
          (warning (find warning? rep/warn))
          (r/w-props (append (rep/warn-props repeater)
                             (rep/warn-props warning))))
     ['timestamp  (cons* type: type raw-value: raw-value
                        year-start: ys month-start: mos day-start: ds
                        hour-start: hs minute-start: mis

                        year-end: ye month-end: moe day-end: de
                        hour-end: he minute-end: mie
                        begin: b end: e post-blank: 0

                        r/w-props)])))









(def (headline-parser (raw-secondary? #f))
  (def NODE-PROPERTIES
    (.let* (pd PROPERTYDRAWER)
       (append-map!
        (lambda (np) [(string->keyword (string-upcase (org-element-property key: np)))
                 (org-element-property value: np)])
        (org-element-contents pd))))
  (.let*
   ((beg (point)) (h (HEADLINE))
    (stars (return (org-element-property stars: h)))
    (todo (return (org-element-property todo-keyword: h)))
    (raw-value (return (org-element-property title: h)))
    (title-end  (point))
    (level (return (length stars)))
    (time-props (.or (PLANNING (timestamp-parser)) []))
    (standard-props (.or NODE-PROPERTIES []))
    (end (.begin (org-end-of-subtree level)))
    (contents-begin (.or (save-excursion
                          (goto-char title-end)
                           (skip-chars-forward " \n\r\t" end)
                           (.let* (pos (beginning-of-line))
                             (return (if (or (= pos end)  (= pos beg)) #f pos))))
                          #f))
     (pre-blank (if (not contents-begin) (return 0)
                    (count-lines title-end contents-begin)))
    (contents-end (.or (save-excursion
                        (goto-char end)
                        (skip-chars-backward " \n\r\t")
                        (beginning-of-line 2))
                       #f))
    (post-blank  (if (not contents-end) (return 0)
                     (count-lines contents-end end))))

      (let (headline
            ['headline
             (append!
              (list ;foo: title-end
                    raw-value: raw-value
                    begin: beg end: end
                    pre-blank: pre-blank
                    contents-begin: contents-begin
                    contents-end: (and contents-begin contents-end)
                    post-blank: post-blank
                    level: level
                    priority: (org-element-property priority: h)
                    tags: (org-element-property tags: h)
                    todo-keyword: todo
                    todo-type: (if todo
                                 (if (member todo (org-env-ref 'org-done-keywords))
                                   'done 'todo)
                                 #f)
                    footnote-section?: (org-element-property footnote-section?: h)
                    archived?: (org-element-property archived?: h)
                    commented?: (org-element-property commented?: h)
                    post-affiliated: beg)
              (append time-props standard-props))])
        (begin0 headline
          (set! (org-element-property title: headline)
            (if raw-secondary? raw-value
                (run (parse-objects
                      0 +inf.0 #f (org-element-object-restrictions 'headline)
                      headline) raw-value)))))))




(def (parse-buffer str (granularity 'object))
  (run (parse-elements 0 (string-length str) 'first-section #f granularity ['org-data []])
       str))

(defsyntax (nest stx)
  (syntax-case stx ()
    ((_ outer ... inner)
     (foldr (lambda (outer-form inner-form)
              (with-syntax (((o ...) outer-form)
                            (i inner-form))
                #'(o ... i)))
            #'inner
            #'(outer ...)))))

(def (parse-elements
      (beg 0) (end +inf.0) (mode #f) (structure #f)
      (granularity #f) (acc #f))

  (def (parse-greater-element-contents? el (type (org-element-type el)))
    ;;Make sure ~granularity~ allows the recursion, or
    ;; ~element~ is a headline, in which case going inside is
    ;; mandatory, in order to get sub-level headings.
    (and (org-element-greater-element? el)
         (or (memq granularity '(element object #f))
             (and (eq? granularity 'greater-element)
                  (eq? type 'section))
             (eq? type 'headline))))
  (.begin
    (goto-char beg)
    (narrow-to-region beg end)
    ;; When parsing only headlines, skip any text before first one.
    (if (eq? granularity 'headline)
      (.begin (some SKIP-LINE) ORG-AT-HEADING)
      #f)
    ;;  ;; Find current element's type and parse it accordingly to
    ;;  ;; its category.
    (.let*
        (els
         (nest
          (let ((elements [])
                (next-element (parse-current-element granularity mode structure))))
          (let parse-element ((p next-element))
            (nest (.let* (el (.or p #f)))
                  (if (not el) (return (reverse! elements)))
                  (let* ((next next-element)
                         ;; Paragraph return VALUES
                         (el (if (org-element? el) el
                                 (match el ((values nel n)
                                            (when n (set! next (return n)))
                                            nel))))
                         (type (org-element-type el))
                         (cbeg (org-element-property contents-begin: el))
                         (cend (org-element-property contents-end: el)))
                    (displayln el)
                    (push! el elements))
                  (.let* (contents 
                          (cond
                           ;; If element has no contents, don't modify it.
                           ((not cbeg) #f)
                           ;; ;; If we already have contents, We're almost done.
                           ((org-element-contents el) => (cut return <>))

                           ;; Fill ~element~ contents by side-effect. Greater
                           ;; element: parse between contents-begin: and
                           ;; contents-end:
                           ((parse-greater-element-contents? el)
                            (.begin (parse-elements
                                     cbeg cend ;; Possibly switch to a special mode.
                                     (next-mode type #t)
                                     (and (memq type '(item plain-list))
                                          (org-element-property structure: el))
                                     granularity el)
                                    (return (org-element-contents el))))

                           ;; It's an element or object that has contents, which
                           ;; are objects. So, parse them if allowed.
                           ((memq granularity '(object #f))
                            (displayln "Parsing objects " cbeg "-" cend " for " type)
                            (parse-objects cbeg cend el
                                           (org-element-object-restrictions el)))
                           (#t (return #f))))
                     ;; (when contents
                     ;;   (for (child contents)
                         ;(set! (org-element-property parent: child) el)))
                    (.begin (goto-char (org-element-property end: el))
                            (parse-element next)))))))
      (.begin (widen)
              (if (not acc) (return els)
                  (begin0 (return acc) (set! (org-element-contents acc) els)))))))

  ;; Return either values of the string that comes before the object and the next
  ;; object, or #f. ~restriction~ is a list of object types, as symbols, that
  ;; should be looked after.

(def (object-lex restrictions)
  (def (obj? name parser)
    (if (not (memq name restrictions)) (fail)
        parser))

  (def lex-objs
    (.or (obj? 'code (code-parser))
         (obj? 'bold (bold-parser))
         (obj? 'italic (italic-parser))
         (obj? 'verbatim (verbatim-parser))
         (obj? 'strike-through (strike-through-parser))
         (obj? 'timestamp (timestamp-parser))
         (obj? 'table-cell (table-cell-parser))
         (obj? 'table-cell-rule (table-cell-parser 'rule))))

    (.let* ((lst (some (item)))
            (obj (.or lex-objs
                      ;; if the list is not null, but we're at the end of the
                      ;; line, return #f for the object
                      (.begin (sat (lambda _ (not (null? lst))) (.not (item))) #f))))
           (values (list->string lst) obj)))

(def (parse-objects (beg 0) (end +inf.0) (acc #f) (restriction all-objects) (parent #f))
  (def (lexes->contents lexs)
    (def contents [])
    (let lp ((ls lexs))
      (if (null? ls) (return (reverse! contents))
        (let ((values str obj) (car ls))
          (unless (string-null? str) (push! str contents))
          (cond
           ((not obj) (lp (cdr ls)))
           (#t  
            (push! obj contents)
            (let ((obj-end (org-element-property end: obj))
                  (cont-beg (org-element-property contents-begin: obj)))
              ;; Fill contents of ~object~ if needed
                (.begin
                  (if (and (org-element-recursive-object? obj)
                           cont-beg)
                    (parse-objects cont-beg (org-element-property contents-end: obj)
                                   obj (org-element-object-restrictions obj))
                    #t)
                  (lp (cdr ls))))))))))

  (.begin
    (narrow-to-region beg end)
    (goto-char beg)
    (.let* ((lexes (many (object-lex restriction)))
            (cs (lexes->contents lexes))
            (_ (widen)))
      (when parent
        (for (el cs) (when (not (string? el))
                       (set! (org-element-property parent: el) parent))))
      ;;  If there's truly an element to give our contents to, giv'r!
      (if acc
        (begin0 (return acc)
          (org-element-contents-set! acc cs))
        (return cs)))))
;;; Parsing Element Starting At Point
;;
;; `parse-current-element' is the core function of this section. It returns the
;; Gerbil representation of the element starting at point.
;;
;; GRANULARITY determines the depth of the
;; recursion.  Allowed values are `headline', `greater-element',
;; `element', `object' or nil.  When it is broader than `object' (or
;; nil), secondary values will not be parsed, since they only
;; contain objects.
;;
;; `parse-current-element' makes use of special modes. They are activated for
;; fixed element chaining (e.g., `plain-list' > `item') or fixed conditional
;; element chaining (e.g., `headline' > `section'). Special modes are:
;; `first-section', `item', `node-property', `section' and `table-row'.



(def (parse-current-element (granularity #f) (mode #f) (structure #f))
  (def raw-secondary? (and granularity (not (eq? granularity 'object))))
  (.first
   (.or
     (.let* (p (point)) (displayln "parse current element " mode " at " p "\n") (fail))
     (if (not (eq? mode 'table-row)) (fail)
         (table-row-parser granularity))
    (headline-parser raw-secondary?)
    (.let* (afk (collect-affiliated-keywords))
     (.or (table-parser afk granularity)
          (if (eq? mode 'no-paragraph) (fail)
              (paragraph-parser)))))))
(def (next-mode type parent?)
  "Return next special mode according to TYPE, or #f.

Modes can be either `first-section', `item', `node-property', `planning',
`property-drawer', `section', `table-row' or #f."
  (if parent?
      (case type
        ((headline) 'section)
        ((inlinetask) 'planning)
        ((plain-list) 'item)
        ((property-drawer) 'node-property)
        ((section) 'planning)
        ((table) 'table-row)
        (else #f))
    (case type
      ((item) 'item)
      ((node-property) 'node-property)
      ((planning) 'property-drawer)
      ((table-row) 'table-row))))

(def (paragraph-parser (afk []) values: (return-next-element-as-well #f)
                       granularity: (granularity #f))
  (def EMPTY-LINE (.begin (skip WS) (.or #\newline EOF)))
  (def END-PARAGRAPH
    (.or (parse-current-element granularity 'no-paragraph #f) EMPTY-LINE))

  (def (para)
    (.let* ((pos (.begin SKIP-LINE (point)))
            (end? (.or END-PARAGRAPH (.not (item)) #f)))
      (if end?
        (return (values pos end?))
        (para))))

  (.let* ((beg (.begin (.not EOF) (point)))
          ((values lend end-el) (para))
          (end (if (org-element? end-el)
                 (return (org-element-property begin: end-el))
                 (.begin (skip-chars-forward " \n\r\t")
                         (point))))
          (post-blank (count-lines lend end))
          (_ (goto-char end)))
    (let (paragraph ['paragraph (cons* begin: (if (null? afk) beg (car afk))
                                       end: end
                                       contents-begin: beg
                                       contents-end:
                                       (if (eof-object? end-el)
                                         end lend)
                                       post-blank: post-blank
                                       post-affiliated: beg
                                       afk)])
      (if return-next-element-as-well
        (values paragraph (if (org-element? end-el) end-el #f))
        paragraph))))

(def (emphasis-parser)
  TEXT-MARKUP)

(def (bold-parser)
  (.begin (peek #\*) (emphasis-parser)))
(def (code-parser)
 (.begin (peek #\~) (emphasis-parser)))
(def (italic-parser)
 (.begin (peek #\/) (emphasis-parser)))
(def (strike-through-parser)
  (.begin (peek #\+) (emphasis-parser)))
(def (underline-parser)
  (.begin (peek #\_) (emphasis-parser)))
(def (verbatim-parser)
  (.begin (peek #\=) (emphasis-parser)))
