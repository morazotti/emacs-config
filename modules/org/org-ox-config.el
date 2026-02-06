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

;; fixes preview with #+cite_export malforming
(defun my/fix-org-cite-export-parsing (info)
  "Fix the :cite-export data type before Org attempts to process it.
Converts strings (\"biblatex opt\") or symbols ('bibtex) into lists."
  (let ((cite-export (plist-get info :cite-export)))
    (cond
     ;; Case 1: It is a string (Yu Huang's email bug)
     ;; Ex: "biblatex backend=biber" -> (biblatex "backend=biber")
     ((stringp cite-export)
      (let* ((parts (split-string cite-export))
             (processor (intern (car parts)))
             (args (cdr parts)))
        (plist-put info :cite-export (cons processor args))))

     ;; Case 2: It is a loose symbol (Your initial error)
     ;; Ex: 'bibtex -> (bibtex)
     ((symbolp cite-export)
      (plist-put info :cite-export (list cite-export)))))
  info)

;; Apply the advice to run BEFORE feature processing
(advice-add 'org-export-process-features :before #'my/fix-org-cite-export-parsing)

;; allows correct quotation on portuguese export
(add-to-list 'org-export-smart-quotes-alist
	     '("pt"
	       (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
	       (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
	       (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
	       (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
	       (apostrophe        :utf-8 "’" :html "&rsquo;")))


(provide 'org-ox-config)
