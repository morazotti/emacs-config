;; org-export hook: if I have compiled the file once this session
;; it must redo it async each save
(defun my/org-export-and-set-async-hook ()
  (interactive)
  (org-export-dispatch)
  (setq-local local-org-export-dispatch-last-action
	      (append (list (car org-export-dispatch-last-action)
			    'async)
		      (if (eq (cadr org-export-dispatch-last-action) 'async)
			  (cddr org-export-dispatch-last-action)
			(cdr org-export-dispatch-last-action)))))

(defun my/org-export-run-on-save ()
  (interactive)
  (when (bound-and-true-p local-org-export-dispatch-last-action)
    (setq org-export-dispatch-last-action local-org-export-dispatch-last-action)
    (org-export-dispatch 1)))

(use-package org-reveal)
(use-package ox-reveal
  :after (org-reveal))
(load-library "ox-reveal")

;; allows correct quotation on portuguese export
(add-to-list 'org-export-smart-quotes-alist
	     '("pt"
	       (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
	       (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
	       (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
	       (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
	       (apostrophe        :utf-8 "’" :html "&rsquo;")))


(provide 'org-ox-config)
