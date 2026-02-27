;; autoload org package
(defun my/org-generate-version-file ()
  "Generate org-version.el for straight.el build process."
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
    (with-temp-file "org-version.el"
      (insert
       (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
       (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
       "(provide 'org-version)\n"))))

(use-package org
  :straight (:host nil
             :repo "https://git.tecosaur.net/tec/org-mode.git"
             :branch "dev"
             :remote "tecosaur"
             :files (:defaults "etc")
             :build t
             :pre-build (my/org-generate-version-file))
  :demand t
  :init
  (defvar org-level-max 8)
  (setq org-directory (file-name-concat home "Documents" "org"))

  :config
  (defun my/org-headline-re-fix (true-level &optional no-bol)
    "Fix org-headline-re for true-level=0."
    (when (< true-level 1) (setq true-level 1))
    (org-headline-re true-level no-bol))

  (advice-add 'org-headline-re :around
              (lambda (orig true-level &optional no-bol)
		(funcall orig (max 1 true-level) no-bol)))
  ;; (remove-hook 'kill-emacs-hook 'org-persist-write-all)
  ;; (remove-hook 'kill-emacs-hook 'org-persist-gc)
  ;; (run-with-idle-timer 600 t #'org-persist-write-all)

  :custom
  (org-id-track-globally t)
  (org-id-locations-file "~/.config/emacs/.org-id-locations")
  (org-src-fontify-natively t)
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
                              ("style=american" "csquotes" t)
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

  ;; ox
  (org-export-async-init-file (expand-file-name "async-init.el" user-emacs-directory))

  :hook
  (org-babel-after-execute . display-ansi-colors)
  (org-mode . org-indent-mode)
  (org-mode . org-latex-preview-mode)
  (org-mode . visual-line-mode)
  (org-mode . org-toggle-pretty-entities)
  (org-mode . (lambda ()
                (add-hook
                 'after-save-hook
                 'my/org-export-run-on-save
                 nil 'make-it-local)))

  :bind
  ("C-c l" . org-store-link)
  (:map org-mode-map
	("<f7>" . my/org-export-and-set-async-hook)
	("M-g R" . my/previous-reference-or-label)
	("M-g r" . my/next-reference-or-label)))

;; some extra configs to org and org-export
(use-package org-contrib)
(use-package ox-extra
  :after (org org-contrib)
  :config (ox-extras-activate '(latex-header-blocks ignore-headlines)))
(use-package org-ql)

(provide 'org-core-config)
