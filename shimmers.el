;; inspired by https://github.com/nextjournal/clerk/blob/main/clerk.el

(require 'cider-client)
(require 'cider-eval)
(require 'cider-mode)

(defun shimmers-visit-sketch ()
  (interactive)
  (when-let ((ns (cider-current-ns)))
    (when (string-match-p "shimmers.sketches.*" ns)
      (cider-interactive-eval
       (format "(shimmers.core/visit! '%s)" ns)))))

(define-key cider-mode-map (kbd "<f8>") #'shimmers-visit-sketch)

(provide 'shimmers)
