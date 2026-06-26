;; shimmers.el --- open tests and visit sketches on keybind -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Charles L.G. Comstock

;; Author: Charles L.G. Comstock <dgtized@gmail.com
;; Version: 1.0.0
;; URL: https://github.com/dgtized/shimmers

;;; Commentary:

;; Adds a keybinding to visit a sketch in the browser correspondig to the sketch
;; open in a buffer and adds a helper to visit the shimmers test set.
;;
;; inspired by https://github.com/nextjournal/clerk/blob/main/clerk.el

;;; Code:

(require 'cider-client)
(require 'cider-eval)
(require 'cider-mode)

;; Is there a way to switch-to or open tab?
(defun shimmers-visit-tests ()
  (interactive)
  (shell-command "xdg-open http://localhost:9500/figwheel-extra-main/tests"))

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
