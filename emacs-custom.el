(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(org-hide-emphasis-markers t)
 '(org-latex-preview-appearance-options
   '(:foreground default :background default :scale 2.0 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")) nil nil "Customized with use-package org")
 '(org-latex-src-block-backend 'minted nil nil "Customized with use-package org")
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-use-sub-superscripts nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(jinx-misspelled ((t (:underline (:color "red" :style wave :position nil)))))
 '(org-document-title ((t (:foreground "#fe640b" :weight bold))))
 '(org-drawer ((t (:foreground "Blue1" :family "Ligamonacop"))))
 '(visual-shorthands-face ((t (:inherit font-lock-keyword-face :extend nil :background "#e0e0e0")))))
