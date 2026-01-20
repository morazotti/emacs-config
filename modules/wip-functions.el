;; I'll try to replace display-ansi-colors by this.
(defun my/org-babel-display-ansi-colors ()
  "Aplica cores ANSI apenas na região de resultados do último bloco Babel."
  (when-let ((result-begin (org-babel-where-is-src-block-result)))
    (save-excursion
      (goto-char result-begin)
      (let ((beg (point)))
        (org-babel-result-end)
        (ansi-color-apply-on-region beg (point))))))

(use-package lazytab
    :straight (:host github :repo  "karthink/lazytab" :branch "master")
    :hook ((LaTeX-mode . lazytab-mode) (LaTeX-math-mode . orgtbl-mode)))

;;;;
(org-link-set-parameters "notmuch"
			 :follow 'org-notmuch-open
			 :store 'org-notmuch-store-link)

(defun org-notmuch-open (id)
  "Visit the notmuch message or thread with id ID."
  (notmuch-show id))

(defun org-notmuch-store-link ()
  "Store a link to a notmuch mail message."
  (cl-case major-mode
    ('notmuch-show-mode
     ;; Store link to the current message
     (let* ((id (notmuch-show-get-message-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-show-get-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))
    ('notmuch-search-mode
     ;; Store link to the thread on the current line
     (let* ((id (notmuch-search-find-thread-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-search-find-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))))
    
(defvar my/latex-math-macros
  '(("hat" . "\\hat")
    ("bar" . "\\bar")
    ("dot" . "\\dot")
    ("vec" . "\\vec")
    ("til" . "\\tilde"))
  "Lista de sufixos e macros LaTeX para expansão automática.")

(defun my/latex-expand-sexp-macro ()
  "Expande a sexp anterior usando `my/latex-math-macros`."
  (interactive)
  (when (org-inside-LaTeX-fragment-p)
    (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (sexp   (and bounds
                      (buffer-substring-no-properties
                       (car bounds) (cdr bounds)))))
    (when sexp
      (let ((match (seq-find (lambda (pair)
                               (string-suffix-p (car pair) sexp))
                             my/latex-math-macros)))
        (when match
          (let* ((suffix (car match))
                 (macro  (cdr match))
                 (base   (substring sexp 0 (- (length sexp) (length suffix))))
                 (start  (car bounds))
                 (end    (cdr bounds)))
            (delete-region start end)
            (insert macro "{" base "}")
            (if (string-empty-p base)
                (backward-char 1)
              (goto-char (+ start (length macro) 2 (length base)))))))))))

(defun my/latex-trigger-expand ()
  (when (and (derived-mode-p 'org-mode)
             (org-inside-LaTeX-fragment-p))
    (let ((last (char-to-string last-command-event)))
      (when (seq-some (lambda (pair)
                        (string-suffix-p last (car pair)))
                      my/latex-math-macros)
        (my/latex-expand-sexp-macro)))))

(add-hook 'post-self-insert-hook #'my/latex-trigger-expand)

(use-package aas
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'org-mode
    :cond #'org-inside-LaTeX-fragment-p
    "ht" "hat"))

(provide 'org-notmuch)
(provide 'wip-functions)
