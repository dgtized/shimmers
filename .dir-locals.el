((nil
  (cider-preferred-build-tool . clojure-cli)
  (cider-clojure-cli-aliases . "dev")
  (cider-default-cljs-repl . figwheel-main)
  (cider-figwheel-main-default-options . ":dev")
  (cider-repl-display-help-banner . nil)
  (eval . ((lambda () (when (not (featurep 'shimmers))
                   (let ((shimmers-file (expand-file-name "shimmers.el" default-directory)))
                     (when (file-exists-p shimmers-file)
                       (load shimmers-file)
                       (require 'shimmers)))))))))
