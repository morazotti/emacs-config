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

(use-package resnippets
  :straight (:host github :repo "morazotti/resnippets" :branch "master")
  ;; :custom (resnippets-expand-env
  ;; 	   '((smartparens-mode . nil)
  ;; 	     (cdlatex-mode . nil)))
  :hook (org-mode . resnippets-mode)
  :config
  (resnippets-define
   "math-mode"
   '(:mode (LaTeX-mode org-mode) :condition (or (texmathp) (org-inside-LaTeX-fragment-p)))
   ("\\([a-zA-Z\\]+\\)hat" '("\\hat{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)bar" '("\\bar{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)til" '("\\tilde{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)vec" '("\\vec{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)<-" '("\\overleftarrow{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)cnc" '("\\cancel{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)dag" '(1 "^{\\dag}"))
   ("\\([a-zA-Z{\\]+\\)hh" '(1 "\\hbar"))
   ("\\([a-zA-Z{\\]+\\)ll" '(1 "\\ell"))
   ("\\([a-zA-Z\\]+\\)conj" '(1 "^{*}"))
   ("\\([a-zA-Z\\]+\\)trans" '(1 "^{T}"))
   ("\\([a-zA-Z\\]+\\)inv" '(1 "^{-1}"))

   ("<\\([a-zA-Z0-9_^{}\\\\]+\\)|" '("\\bra{" 1 "}"))
   ("<\\([a-zA-Z0-9_^{}\\\\]+\\) ?|\\([a-zA-Z0-9_^{}\\\\]+\\) ?>"
    '("\\braket{" 1 "}{" 2 "}"))
   ("\\([\(]?\\)\\([ ]?\\)//" '(1 "\\frac{" (resnippets-cursor) "{}"))
   ("\\([a-zA-Z0-9{}\\\\]+\\)/" '("\\frac{" 1 "}{" (resnippets-cursor)))
   ("\\([a-zA-Z}]\\)\\([0-9]\\)" '(1 "_" 2))
   ("_\\([0-9][0-9]\\)" '("_{" 1 (resnippets-cursor) "}"))


   ;; fun call
   ;; any elisp function ~fun~ with a numeric argument
   ;; can be called by ;fun=argument;

   (";\\([^=]+\\)=\\([\-0-9.]+\\);"
    '((number-to-string
       (funcall (intern (resnippets-group 1))
		(string-to-number (resnippets-group 2))))))))

;; remap -mode to -ts-mode
(add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


(provide 'langs-utils-config)
