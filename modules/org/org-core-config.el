;; autoload org package
(use-package org
  :straight `(org
              :fork (:host nil
			   :repo "https://git.tecosaur.net/tec/org-mode.git"
			   :branch "dev"
			   :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
		(require 'lisp-mnt)
		(let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
			(with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)
  :demand t
  :init
  (setq org-directory (file-name-concat home "Documents" "org"))

  :custom ((org-src-fontify-natively t)
	   (org-src-window-setup 'reorganize-frame)
	   (org-confirm-babel-evaluate nil)

	   ;; org-cite
	   (org-cite-global-bibliography (list my/bibliography-file))
	   (org-cite-insert-processor 'citar)
	   (org-cite-follow-processor 'citar)
	   (org-cite-activate-processor 'citar)

	   ;; LaTeX
	   (org-latex-listings 'minted)
           (org-latex-packages-alist '(("AUTO" "babel" t)
				       ("" "physics" t)
				       ("" "framed" t)
				       ("" "tikz" t)))
           (org-latex-pdf-process '("latexmk -synctex=1 -shell-escape -bibtex -interaction=nonstopmode -pdf -f -8bit %f"))
           (org-latex-prefer-user-labels t)
	   (org-format-latex-options
	    '(:foreground default
			  :background default
			  :scale 2.0
			  :html-foreground "Black"
			  :html-background "Transparent"
			  :html-scale 1.0
			  :matchers ("begin" "$1" "$" "$$" "\\(" "\\[" )))

	   (org-preview-latex-image-directory
	    (file-name-concat home ".cache" "ltximg"))
	   )

  :hook ((org-babel-after-execute . display-ansi-colors)
	 (org-mode . org-indent-mode)
	 (org-mode . org-latex-preview-mode)
	 (org-mode . visual-line-mode)
	 (org-mode . org-toggle-pretty-entities))
  :bind (("C-c l" . org-store-link)
	 (:map org-mode-map ("<f7>" . (lambda () (interactive) (org-export-dispatch 1))))))

;; some extra configs to org and org-export
(use-package org-contrib)
(use-package ox-extra
  :after (org org-contrib)
  :config (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(provide 'org-core-config)
