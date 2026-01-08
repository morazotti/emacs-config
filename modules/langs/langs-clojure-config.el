(use-package clojure-ts-mode
  :init (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))

(use-package cider)
(provide 'langs-clojure-config)
