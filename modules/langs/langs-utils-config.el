;; better parenthesis
(use-package smartparens
  :hook ((org-mode latex-mode C-mode julia-mode python-mode) . smartparens-mode))

(use-package paredit
  :hook ((snippet-mode lisp-mode lisp-interaction-mode clojure-mode emacs-lisp-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode LaTeX-mode latex-mode org-mode) . rainbow-delimiters-mode))

;; mise
;; helps with binaries installed with mise
(use-package mise
  :config (mise-mode))

;; snippets
;; actually, I'm checking tempel, and it looks good.
;; (add-to-list 'load-path
;;              (file-name-concat user-emacs-directory "plugins" "yasnippet"))
(use-package yasnippet
  :init (yas-global-mode))

(use-package yasnippet-snippets)

;; yasnippet auto load
;; add symbol 'auto on condition

(defun my/yas-try-expanding-auto-snippets ()
  (when (bound-and-true-p yas/minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))

(add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

(defun my/yas-math-modify-accent (text)
  (let ((argument t))
    (condition-case nil
	(progn
          (backward-sexp)
          (kill-sexp)
          (delete-char 1))
      (error (setq argument 'nil)))
    (insert "\\" text "{" (if argument (current-kill 0) "") "}")))

;; remap -mode to -ts-mode
(add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


(provide 'langs-utils-config)
