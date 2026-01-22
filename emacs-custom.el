(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(ebib-reading-list-template "* %M %T\12\12\12:PROPERTIES:\12%K\12:END:\12")
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-hide-macro-markers t)
 '(org-image-actual-width '(350))
 '(org-image-align 'center)
 '(org-latex-preview-appearance-options
   '(:foreground default :background default :scale 2.0 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")) nil nil "Customized with use-package org")
 '(org-latex-src-block-backend 'minted nil nil "Customized with use-package org")
 '(org-modern-star 'fold)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-safe-remote-resources '("\\`\\[\\[file:early-init\\.el]]\\'"))
 '(org-startup-with-latex-preview t)
 '(org-use-sub-superscripts nil)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (shell-command "pandoc README.org -o README.md"))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(jinx-misspelled ((t (:underline (:color "red" :style wave :position nil)))))
 '(org-document-title ((t (:weight bold :height 1.0))))
 '(org-drawer ((t (:foreground "Blue1" :family "Ligamonacop"))))
 '(visual-shorthands-face ((t (:inherit font-lock-keyword-face :extend nil :background "#e0e0e0")))))
