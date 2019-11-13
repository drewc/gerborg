(import :drewc/org/syntax :drewc/smug)
(export #t)

(def ORG-AT-HEADING
 (save-excursion
  BOL (STARS)))

(def org-at-heading?
  (.or ORG-AT-HEADING
       #f))

(def (org-end-of-subtree (level 1) (to-heading #f))
  (def END (.or (peek (sat (lambda (s) (= (length s) level)) STARS-ONLY)) EOF))
    (.begin (some SKIP-LINE) (if to-heading (.begin END (HEADLINE)) END) (point)))
