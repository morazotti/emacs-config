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

(defun my/org-latex-centering-filter (text backend info)
  "Adiciona \\centering automaticamente dentro de blocos figure no LaTeX."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\\begin{figure}" "\\\\begin{figure}\n\\\\centering" text)))

(if (bound-and-true-p org-export-filter-special-block-functions)
    (add-to-list 'org-export-filter-special-block-functions
		 'my/org-latex-centering-filter)
  (setq org-export-filter-special-block-functions (list 'my/org-latex-centering-filter)))

(use-package org-reveal)
(use-package ox-reveal
  :after (org-reveal))
(load-library "ox-reveal")

;; templates de apresentação: submódulos git em templates/, montados em
;; cada apresentação como submódulo (M-x my/presentation-new)
(defvar my/presentation-templates-dir
  (expand-file-name "templates" user-emacs-directory)
  "Diretório com os templates de apresentação (submódulos git).")

(defvar my/presentation-templates
  '(("reveal-quanta" :mount "quanta" :setupfile "quanta.setup"))
  "Templates: nome do diretório em `my/presentation-templates-dir',
pasta onde o submódulo é montado na apresentação e setupfile do org.")

(defun my/--git (dir &rest args)
  "Roda git ARGS em DIR; erro com a saída do git se falhar."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun my/presentation-new (dir template)
  "Cria (ou prepara) a apresentação em DIR usando TEMPLATE como submódulo."
  (interactive
   (list (read-directory-name "Diretório da apresentação: ")
         (completing-read "Template: " my/presentation-templates nil t
                          nil nil (caar my/presentation-templates))))
  (let* ((spec (cdr (assoc template my/presentation-templates)))
         (mount (plist-get spec :mount))
         (local (expand-file-name template my/presentation-templates-dir))
         (url (my/--git local "remote" "get-url" "origin"))
         (org (expand-file-name "present.org" dir)))
    (make-directory dir t)
    (unless (file-directory-p (expand-file-name ".git" dir))
      (my/--git dir "init"))
    (if (file-exists-p (expand-file-name mount dir))
        (message "%s/ já existe; submódulo não adicionado" mount)
      ;; Clona do template local (sem rede) e aponta o submódulo para o remoto.
      (my/--git dir "-c" "protocol.file.allow=always"
                "submodule" "add" local mount)
      (my/--git dir "submodule" "set-url" mount url)
      (my/--git dir "add" ".gitmodules"))
    (unless (file-exists-p org)
      (with-temp-file org
        (insert (format "#+title: \n#+author: %s\n#+email: %s\n#+language: pt-br\n#+SETUPFILE: %s/%s\n\n* \n"
                        user-full-name user-mail-address
                        mount (plist-get spec :setupfile)))))
    (find-file org)
    (goto-char (point-min))
    (end-of-line)))

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

;; adds default backend to biblatex
(with-eval-after-load 'oc
  (setf (alist-get 'latex org-cite-export-processors)
        '(biblatex "numeric" "backend=biber")))

(require 'org-ox-classes-config)
(provide 'org-ox-config)
