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
    
(provide 'org-notmuch)
