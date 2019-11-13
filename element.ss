;; (c) drewc <me@drewc.ca> All Rights Reserved
(import :drewc/smug 
        :drewc/org/syntax :drewc/org/environment :drewc/org/location
        :std/srfi/13 :std/srfi/1 :gerbil/gambit/exact :std/misc/list)
(export #t)

(def all-elements
  '(babel-call center-block clock comment
    comment-block diary-sexp drawer dynamic-block example-block export-block
    fixed-width footnote-definition headline horizontal-rule inlinetask item keyword
    latex-environment node-property paragraph plain-list planning property-drawer
    quote-block section special-block src-block table table-row verse-block))

(def (org-element? el) (and (pair? el) (memq (car el) all-elements) #t))

(def greater-elements
  '(center-block drawer dynamic-block footnote-definition headline inlinetask
    item plain-list property-drawer quote-block section special-block table))

(def (org-greater-element? el)
  (and (memq (if (symbol? el) el (car el)) greater-elements)  #t))

  (def all-objects
    '(bold code entity export-snippet footnote-reference inline-babel-call
  inline-src-block italic line-break latex-fragment link macro radio-target
  statistics-cookie strike-through subscript superscript table-cell target
  timestamp underline verbatim))

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

(def (org-element-restriction el)
  (cdr (assq (if (symbol? el) el (org-element-type el)) object-restrictions)))


(def plain-text-properties-table (make-hash-table-eq weak-keys: #t))

(def (plain-text-property prop plain-text)
  (pgetq prop (cadr (hash-ref plain-text-properties-table plain-text ['plain-text []]))))

(def (plain-text-property-set! prop text value)
  (let (element (hash-ref plain-text-properties-table text #f))
    (begin0 text
    (if (not element)
      (hash-put! plain-text-properties-table text ['plain-text [prop value]])
      (set! (org-element-property prop element) value)))))


(def (org-element-type el) (if (string? el) 'plain-text (car el)))

(def (org-element-property prop el)
  (if (string? el) (plain-text-property prop el)) (pgetq prop (cadr el)))
(def (org-element-contents el) 
  (let (c (cddr el)) (if (null? c) #f c)))

(def (org-element-contents-set! el contents)
  (set-cdr! (cdr el) contents))


(def (org-element-property-set! prop el value)
  (def props (let lp ((ps (cadr el)))
               (cond ((null? ps) [])
                     ((eq? prop (car ps))
                      (lp (cddr ps)))
                     (#t 
                      (cons* (first ps) (second ps) (lp (cddr ps)))))))
  (set-car! (cdr el) (cons* prop value props)))


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
             (restrict (org-element-restriction 'keyword))
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

(def (table-parser (affiliated []) (table-row #f))
  ;; ~affiliated~ is a list of which ~car~ is the buffer position at the
  ;; beginning of the first affiliated keyword and ~cdr~ is a plist of
  ;; affiliated keywords along with their value.

  ;; If ~table-row~ is a parser, use that and return contents. This saves time
  ;; and effort for big tables when we actually want the lines.

  ;; A | followed by anything that is not WS marks a table line

  (def TABLE-LINE
    (.begin (skip WS) #\| (skip WS) (sat (? (not char-whitespace?))) SKIP-LINE))

  (.let* ((b (if (not (null? affiliated)) (return (car affiliated)) (point)))
          (table-begin (point))
          (contents (many1 (or table-row TABLE-LINE)))
          (table-end (point))
          (tblfm (.or (many1 TBLFM) #f))
          (pos-before-blank (point))
          (blanks (many (.begin (many WS) #\newline)))
          (end (point))
          (afks (return (if (pair? affiliated) (cdr affiliated) [])))
          (contents (return (if table-row contents []))))
  ['table [begin: b end: end type: 'org tblfm: tblfm
           contents-begin: table-begin contents-end: table-end
           value: #f post-blank: (length blanks)
           post-affiliated: table-begin
           . afks]
          . contents]))

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
    (title-end ((liftP 1-) (point)))
    (level (return (length stars)))
    (time-props (.or (PLANNING (timestamp-parser)) []))
    (standard-props []) ;(.or NODE-PROPERTIES []))
    (end (peek (.begin (org-end-of-subtree level))))
    (contents-begin (save-excursion
                     (goto-char title-end)
                     (skip-chars-forward " \n\r\t" end)
                     (beginning-of-line)))
    (pre-blank ((liftP 1-) (count-lines beg contents-begin)))
    (contents-end (save-excursion
                   (goto-char end)
                   (skip-chars-backward " \n\r\t")
                   (beginning-of-line 2)))
    (post-blank ((liftP 1-) (count-lines contents-end end))))

      (let (headline
            ['headline
             (append!
              (list
               raw-value: raw-value
               begin: beg end: end
               pre-blank: pre-blank
               contents-begin: contents-begin
               contents-end: contents-end
               level: level
               priority: (org-element-property priority: h)
               tags: (org-element-property tags: h)
               todo-keyword: todo
               todo-type: (if todo
                            (if (member todo (org-env-ref 'org-done-keywords))
                              'done 'todo)
                            #f)
               post-blank: post-blank
               footnote-section?: (org-element-property footnote-section?: h)
               archived?: (org-element-property archived?: h)
               commented?: (org-element-property commented?: h)
               post-affiliated: beg)
              (append time-props standard-props))])
        (begin0 headline
          (set! (org-element-property title: headline)
            (if raw-secondary? raw-value
                (run (parse-objects
                      0 +inf.0 #f (org-element-restriction 'headline)
                      headline) raw-value)))))))




(def (parse-buffer str (granularity 'object))
  (run (parse-elements 0 (string-length str) 'first-section #f granularity ['org-data []])
       str))


(def (parse-elements
      (beg 0) (end +inf.0) (mode #f) (structure #f)
      (granularity #f) (acc #f))
  (def elements [])

  (def (parse-greater-element-contents? el (type (org-element-type el)))
    ;;Make sure ~granularity~ allows the recursion, or
    ;; ~element~ is a headline, in which case going inside is
    ;; mandatory, in order to get sub-level headings.
    (and (org-greater-element? type)
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
    (let parse-element ((el (parse-current-element granularity mode structure)))
      (.let*
       (el el)
       (push! elements el)
        (let ((type (org-element-type el))
        (cbeg (org-element-property contents-begin: el)))

    (cond
     ;; If element has no contents, don't modify it.
     ((not cbeg) (goto-char (org-element-property end: el)))
     ;; If we already have contents, just set the parent.
     ((org-element-contents el) =>
      (cut map
        (lambda (child) (set! (org-element-property parent: child) el)) <>))

     ;; Fill ~element contents by side-effect.
     ;; If element already has contents
     ;; Greater element: parse between contents-begin: and
     ;; contents-end:
     ((parse-greater-element-contents? type) #t)))
        ))
    (widen)
    (return elements)

    ))


;;; Parsing Element Starting At Point
;;
;; `parse-current-element' is the core function of this section. It returns the Gerbil
;; representation of the element starting at point.
;;
;; `parse-current-element' makes use of special modes. They are activated for fixed
;; element chaining (e.g., `plain-list' > `item') or fixed conditional element
;; chaining (e.g., `headline' > `section'). Special modes are: `first-section',
;; `item', `node-property', `section' and `table-row'.


(def (parse-current-element (granularity #f) (mode #f) (structure #f))
  (def raw-secondary? (and granularity (not (eq? granularity 'object))))
  (.first
   (.or
    (headline-parser raw-secondary?)
    (.let*
     (afk (collect-affiliated-keywords))
     (.or (table-parser afk #f)
          (fail)))
       )))


;; Return either values of the string that comes before the object and the next
;; object, or #f. ~restriction~ is a list of object types, as symbols, that
;; should be looked after.

(def (object-lex restriction)
  (.let* ((lst (some (item)))
          (obj (.or (and (memq 'timestamp restriction)
                         (timestamp-parser))
                    (.begin (sat (lambda _ (not (null? lst))) (.not (item))) #f))))
         (values (list->string lst) obj)))

(def (parse-objects (beg 0) (end +inf.0) (acc #f) (restriction all-objects) (parent #f))
  (def (lexes->contents lexs)
    (def contents [])
      (let lp ((ls lexs))
        (unless (null? ls) 
          (let ((values str obj) (car ls))
            (unless (string-null? str) (push! str contents))
            (when obj
              (push! obj contents)
              (let ((obj-end (org-element-property end: obj))
                    (cont-beg (org-element-property contents-begin: obj)))
                  ;; Fill contents of ~object~ if needed
                  (when cont-beg
                    (parse-objects cont-beg (org-element-property contents-end: obj)
                                   obj (org-element-restriction obj)))))
              (lp (cdr ls)))))
   (reverse! contents))

  (.begin
   (narrow-to-region beg end)
   (goto-char beg)

   (.let*
    (cs ((liftP lexes->contents) (many (object-lex restriction))))


    (return cs))))

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
