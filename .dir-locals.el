((nil
  (cider-clojure-cli-global-options . "-A:dev")
  (cider-default-cljs-repl . figwheel-main)
  (cider-figwheel-main-default-options . ":dev")
  (eval . ((lambda () (when (not (featurep 'shimmers))
                   (let ((shimmers-file (expand-file-name "shimmers.el" default-directory)))
                     (when (file-exists-p shimmers-file)
                       (load shimmers-file)
                       (require 'shimmers)))))))))
