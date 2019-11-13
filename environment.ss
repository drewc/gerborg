(export #t)

(def default-org-env
  `((org-todo-keywords-1 . ("TODO" "DONE"))
    (org-done-keywords . ("DONE"))
    (org-footnote-section . "Footnotes")
    (org-footnote-define-inline . #f)
    (org-archive-tag . "ARCHIVE")))

(def current-org-env (make-parameter default-org-env))

(def (org-env-ref key (env (current-org-env))) (assget key env))
