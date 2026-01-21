;; box instead of dedicated buffer
(use-package eldoc-box)

;; expand-region increases region by semantic expressions
(use-package expand-region
    :bind ("C-=" . er/expand-region))

;; hide-show-mode folds code
;; in evil:
;; - zr (show-all)
;; - zm (fold-all)
;; - zo (show-this)
;; - zc (fold-this)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; elfeed
(use-package elfeed
  :custom (elfeed-db-directory "~/.local/share/elfeed")
  :bind ("C-c e" . elfeed))

(use-package elfeed-org
  :straight (:host github :repo "remyhonig/elfeed-org" :branch "master")
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (list (file-name-concat org-directory "elfeed.org"))))

;;jinx
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :init (setq jinx-languages "en_US pt_BR")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
(add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
;;ledger
(use-package ledger-mode
  :custom
  ((ledger-reports
    (quote
     (("bal cf" "%(binary) -f %(ledger-file) bal ^Income ^Expenses --real")
      ("bal nw" "%(binary) -f %(ledger-file) bal ^Assets ^Liabilities --real")
      ("bal precos" "%(binary) -f %(ledger-file) --price-db btc_price.db --price-db comm.db -V bal")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("reg - price" "%(binary) -f %(ledger-file) reg -V --price-db btc_price.db --price-db comm.db")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
   (ledger-post-amount-alignment-column 60))
  :hook
  (ledger-mode . (lambda ()
                   (setq-local tab-always-indent 'complete)
                   (setq-local completion-ignore-case t)
                   (setq-local ledger-complete-in-steps t))))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge)

;; email
(use-package notmuch
  :custom ((notmuch-search-oldest-first nil)
	   (send-mail-function 'smtpmail-send-it)
	   (message-send-mail-function 'smtpmail-send-it)
	   (user-mail-address "nicolas.morazotti@gmail.com")
	   (smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil)))
	   (smtpmail-default-smtp-server "smtp.gmail.com")
	   (smtpmail-smtp-server "smtp.gmail.com")
	   (smtpmail-smtp-service 587)
	   (smtpmail-debug-info t)
	   (starttls-extra-arguments nil)
	   (starttls-gnutls-program "/usr/bin/gnutls-cli")
	   (starttls-extra-arguments nil)
	   (starttls-use-gnutls t)
	   (message-signature-file
	    (file-name-concat user-emacs-directory "private" "signature")))
  :bind ("C-c m" . notmuch))

;; pdf-tools
(use-package pdf-tools
  :hook (pdf-view-mode . blink-cursor-mode)
  
  :config
  (define-key pdf-view-mode-map (kbd ":") 'pdf-view-goto-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (define-key pdf-view-mode-map (kbd "/") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "?") 'isearch-backward)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "J") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "K") 'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "TAB") 'pdf-outline)
  (define-key pdf-view-mode-map (kbd "C-S-r") 'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "T") 'pdf-view-themed-minor-mode)
  (define-key pdf-view-mode-map (kbd "s") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "a") 'pdf-view-fit-page-to-window)
  (pdf-loader-install)

  :mode ("\\.pdf" . pdf-view-mode)

  :custom (pdf-view-midnight-colors '("#FFBB33" . "#222222")))

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

;; terminal
;; I was using ~toggle-term~ to access popup terminal, but it was kinda clunky
;; apparently there's a 'popper' thingy to recover such behavior
(use-package popper
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("*vterm*"
	  "*eshell*"
	  "\\*Messages\\*"
	  "\\*Warning\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vterm)

;;vundo
(use-package vundo
  :demand t)

;; winner mode allows C-c <left> / C-c <right> to recover previous window configuration
(winner-mode)

;; tree-sitter
(use-package tree-sitter
  :custom (treesit-language-source-alist
	   '((python "https://github.com/tree-sitter/tree-sitter-python")
             (clojure  "https://github.com/tree-sitter/tree-sitter-clojure")
             (julia  "https://github.com/tree-sitter/tree-sitter-julia")
	     (bash   "https://github.com/tree-sitter/tree-sitter-bash")
             (c      "https://github.com/tree-sitter/tree-sitter-c")
             (cpp    "https://github.com/tree-sitter/tree-sitter-cpp"))))

;; split and follow window
(defun my/split-and-follow (direction)
  (interactive "c")
  (catch 'my-tag
    (cond ((= direction ?h) (split-window-below))
  	((= direction ?v) (split-window-right))
  	(t (throw 'my-tag "no correct character pressed")))
    (other-window 1)))

(defun my/split-and-follow-horizontally ()
  (interactive)
  (my/split-and-follow ?h))

(defun my/split-and-follow-vertically ()
  (interactive)
  (my/split-and-follow ?v))

;; zoom-window, just like <leader>-z on tmux
(use-package zoom-window
  :bind (("M-z" . zoom-window-zoom)
	 ("C-M-z" . zoom-window-next)))

;; (defvar zoom-register ?z
;;   "the register to store the window configuration for zooming/unzooming.")

;; (defvar zoomed-in-p nil
;;   "a flag to track if the window is currently zoomed in.")

;; (defun toggle-zoom-window ()
;;   "toggle zooming the current window: maximize or restore."
;;   (interactive)
;;   (if zoomed-in-p
;;       (restore-window-configuration-from-zoom-register)
;;     (save-window-configuration-to-zoom-register)
;;     (delete-other-windows))
;;   (setq zoomed-in-p (not zoomed-in-p)))

;; (defun save-window-configuration-to-zoom-register ()
;;   "save the current window configuration to a register for zooming/unzooming."
;;   (window-configuration-to-register zoom-register)
;;   (message "Window configuration saved for zooming."))

;; (defun restore-window-configuration-from-zoom-register ()
;;   "restore the window configuration from the zoom register."
;;   (jump-to-register zoom-register)
;;   (message "Window configuration restored."))

;; (global-set-key (kbd "M-z") 'toggle-zoom-window)

(provide 'tools-utils-config)
