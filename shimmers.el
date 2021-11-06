;; inspired by https://github.com/nextjournal/clerk/blob/main/clerk.el

(require 'cider-client)
(require 'cider-eval)
(require 'cider-mode)

;; TODO: add function to force reload or reset atoms?
;; also consider adding support for reloading with same seed
;; and consider keeping track of last active ns so that binding works from library code?
(defun shimmers-visit-sketch ()
  "Direct browser to (re)load the associated sketch for the current namespace."
  (interactive)
  (let ((ns (cider-current-ns)))
    (if (and ns (string-match-p "shimmers\\.sketches\\..*" ns))
        (progn
          (message (format "Visiting sketch %s" ns))
          (cider-interactive-eval
           (format "(if-let [page (:parameters (shimmers.core/visit! '%s))]
                  [(keyword (get-in page [:path :name]))
                   (get-in page [:query :seed])]
                  \"Unknown sketch %s!\")" ns ns)))
      (error (format "Namespace %s is not a sketch." ns)))))

(define-key cider-mode-map (kbd "<f8>") #'shimmers-visit-sketch)

(provide 'shimmers)
