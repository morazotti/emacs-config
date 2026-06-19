;; use subfigs in LaTeX export

;;  #+name: fig_name
;;  #+attr_latex: :options \centering
;;  #+caption: Figure Caption
;;  #+begin_figure 
;;  [[subfig:img1.png][Subcaption 1 >(scale=0.6)]] 
;;  [[subfig:img2.png][Subcaption 2 >(scale=0.6)]]
;;  [[subfig:img3.png][Subcaption 3 >(scale=0.6)]]
;;  #+end_figure

(with-eval-after-load 'org
  (org-link-set-parameters
   "subfig"
   :follow (lambda (file) (find-file file))
   :face '(:foreground "chocolate" :weight bold :underline t)
   :display 'full
   :export
   (lambda (file desc backend)
     (when (eq backend 'latex)
       (if (string-match ">(\\(.+\\))" desc)
           (concat "\\subfigure[" (replace-regexp-in-string "\s+>(.+)" "" desc) "]"
                   "{\\includegraphics"
                   "[" (match-string 1 desc) "]"
                   "{" file "}}")
         (format "\\subfigure[%s]{\\includegraphics{%s}}" desc file))))))

(with-eval-after-load 'org
  (org-link-set-parameters "noter"
                           :follow (lambda (path) (org-noter))
			   :face '(:weight bold :foreground "orange" :underline t)
                           :help-echo "Abrir Org-noter"))

;; ----------------------------------------------------------------------
;; 1. As funções de busca (Completion) que sequestram o minibuffer
;; ----------------------------------------------------------------------

(defconst my/org-target-regexp "<<\\([^>]+\\)>>"
  "Regular expression to match Org targets (e.g., <<target>>).")

(defconst my/org-name-regexp "^[ \t]*#\\+name:[ \t]*\\([^ \t\n\r]+\\)"
  "Regular expression to match Org #+name: definitions.")

(defun my/org-complete-by-regexp (regexp prompt error-msg)
  "Search the buffer for matches of REGEXP, capturing group 1.
Present them via `completing-read` using PROMPT.
If no matches are found, raise a `user-error` with ERROR-MSG."
  (let ((matches '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string-no-properties 1) matches)))
    (if matches
        (completing-read prompt (delete-dups matches))
      (user-error error-msg))))

(defun my/org-complete-target (&optional arg)
  "Search the buffer for <<targets>>."
  (interactive "P")
  (my/org-complete-by-regexp my/org-target-regexp
                             "Choose target: "
                             "No <<target>> found in this buffer"))

(defun my/org-complete-name (&optional arg)
  "Search the buffer for #+name: entries."
  (interactive "P")
  (my/org-complete-by-regexp my/org-name-regexp
                             "Choose name: "
                             "No #+name: found in this buffer"))

;; ----------------------------------------------------------------------
;; Registering links for completion only
;; ----------------------------------------------------------------------
(with-eval-after-load 'org
  (org-link-set-parameters "target" :complete #'my/org-complete-target)
  (org-link-set-parameters "name" :complete #'my/org-complete-name))

;; ----------------------------------------------------------------------

;; link files and display everything as a single file
(use-package org-transclusion
  :after org
  :bind (("C-c t m" . org-transclusion-transient-menu)
	 ("C-c t t" . org-transclusion-mode)))

;; org-paste-image, simpler than org-download
(defun org-paste-image ()
  "Paste an image from the clipboard, let the user choose a directory to save it,
and insert a link to it in the buffer. Supports Org-mode and LaTeX."
  (interactive)
  (let* ((session (getenv "XDG_SESSION_TYPE"))
         (command-header (cond
                          ((string= session "x11")
                           "xclip -selection clipboard -t image/png -o")
                          ((string= session "wayland")
                           "wl-paste --type image/png")
                          (t (error "Unknown session type: %s" session))))
         ;; choose target dir
         (base-dir (read-directory-name
                    "Choose the directory to save image: "
                    default-directory nil nil "img/"))
         (_ (make-directory base-dir t))
         (filename (concat (make-temp-name
                            (expand-file-name "img-" base-dir)) ".png"))
         (command (concat command-header " | convert - " (shell-quote-argument filename))))

    (shell-command command)
    (if (file-exists-p filename)
        (cond
         ((derived-mode-p 'org-mode)
          (insert (concat "[[file:" (file-relative-name filename default-directory) "]]"))
          (message "Saved image in %s and added link (Org-mode)"
                   (file-relative-name filename default-directory)))
         ((derived-mode-p 'latex-mode)
          (insert (format "\\begin{figure}[h]\n\\centering\n\\includegraphics[width=\\linewidth]{%s}\n\\caption{}\n\\label{fig:}\n\\end{figure}"
                          (file-relative-name filename default-directory)))
          (message "Saved image in %s and added link (LaTeX)"
                   (file-relative-name filename default-directory)))
         (t
          (message "Not in org-mode or LaTeX-mode" filename)))
      (message "Failed to save image"))))

;; add ~framed~ to C-c C-x s // C-c C-,
(add-to-list 'org-structure-template-alist '("f" . "framed"))

(use-package org-excalidraw
  :after org
  :straight (:host github :repo "wdavew/org-excalidraw" :branch "main")
  :custom (org-excalidraw-directory "~/Pictures/excalidraw/")
  :init (org-excalidraw-initialize))

(provide 'org-link-config)
