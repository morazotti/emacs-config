(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defvar my/latex-colors-alist
  '(("red"     . "red")
    ("blue"    . "blue")
    ("green"   . "forest green")
    ("yellow"  . "yellow")
    ("magenta" . "magenta")
    ("cyan"    . "cyan")
    ("brown"   . "brown")
    ("blood"   . "#aa2233")
    ("orange"  . "orange"))
  "Alist mapping macro names to display colors in Emacs.")

(defun my/org-color-macro-sensor (_window oldpos dir)
  "Cursor sensor function to reveal or hide the color macro.
When the cursor enters the macro (dir = entered), we temporarily remove the 'invisible'
property. When the cursor leaves (dir = left), we trigger the refontify of the
revealed region via `font-lock-flush` so that the markers are hidden again."
  (let* ((pos (if (eq dir 'entered) (point) oldpos))
         (prop (get-text-property pos 'my/org-color-macro))
         (start (when prop (or (previous-single-property-change (1+ pos) 'my/org-color-macro) (point-min))))
         (end (when prop (or (next-single-property-change pos 'my/org-color-macro) (point-max)))))
    (when (and prop start end)
      (pcase dir
        ('entered
         (with-silent-modifications
           (remove-text-properties start end '(invisible nil))))
        ('left
         (font-lock-flush start end))))))

(defun my/org-fontify-color-macros ()
  "Apply colors to {{{color(text)}}} macros and configure cursor sensors for revealing."
  ;; Add managed properties to font-lock, including 'cursor-sensor-functions'
  (setq font-lock-extra-managed-props
        (append '(invisible display my/org-color-macro cursor-sensor-functions)
                font-lock-extra-managed-props))
  (let ((color-list-aux (list)))
    (dolist (color-entry my/latex-colors-alist)
      (let ((macro-name (car color-entry))
            (color-value (cdr color-entry)))
        (push `(,(format "{{{%s(\\(.*?\\))}}}" macro-name)
                (0 (progn
                     ;; Mark the entire region with the macro ID and attach the sensor function
                     (put-text-property (match-beginning 0) (match-end 0)
                                        'my/org-color-macro (match-beginning 0))
                     (put-text-property (match-beginning 0) (match-end 0)
                                        'cursor-sensor-functions (list #'my/org-color-macro-sensor))
                     ;; Apply invisibility and visual formatting
                     (put-text-property (match-beginning 0) (match-beginning 1)
                                        'invisible t)
                     (put-text-property (match-beginning 1) (match-end 1)
                                        'face '(:foreground ,color-value :weight bold))
                     (put-text-property (match-end 1) (match-end 0)
                                        'invisible t))))
              color-list-aux)))
    (font-lock-add-keywords nil color-list-aux 'append)))

;; Activate cursor-sensor-mode to monitor text properties in Org-mode
(add-hook 'org-mode-hook #'cursor-sensor-mode)
(add-hook 'org-mode-hook #'my/org-fontify-color-macros)

(defun my/org--color-macro-header (backend)
  (pcase backend
    ('latex "#+MACRO: color \\textcolor{$1}{$2}\n")
    ('html  "#+MACRO: color @@html:<font color=\"$1\">$2</font>@@\n")
    (_ (error "Invalid backend: %S" backend))))

(defun my/org--color-macro-render (backend color &optional mapping)
  (pcase backend
    ('latex
     (if (and (stringp color) (string-prefix-p "#" color))
         (format "\\textcolor[HTML]{%s}{$1}" (substring color 1))
       (format "\\textcolor{%s}{$1}" color)))
    ('html
     (let* ((raw-color (or (and mapping (cdr (assoc-string color mapping t)))
                           color))
            (html-color
             (if (stringp raw-color)
                 (replace-regexp-in-string
                  "\\b\\([[:alpha:]]\\)"
                  (lambda (m) (upcase m))
                  (downcase raw-color)
                  nil nil 1)
               raw-color)))
       (format "@@html:<font color=\"%s\">$1</font>@@" html-color)))
    (_ (error "Invalid backend: %S" backend))))

(defun my/update-colors-org-file (backend)
  (interactive
   (list (intern (completing-read "Backend: " '("latex" "html") nil t nil nil "latex"))))
  (let* ((relative-path (pcase backend
                          ('latex "macros/latex-colors.org")
                          ('html  "macros/colors.org")
                          (_ (error "Invalid backend: %S" backend))))
         (file-path (expand-file-name relative-path user-emacs-directory))
         (mapping (pcase backend
                    ('html '(("forest green" . "LightGreen")))
                    (_ nil))))
    (with-temp-file file-path
      (insert (my/org--color-macro-header backend))
      (dolist (entry my/latex-colors-alist)
        (let* ((name (car entry))
               (color (cdr entry))
               (body (my/org--color-macro-render backend color mapping)))
          (insert (format "#+MACRO: %s %s\n" name body)))))))

(defun my/update-latex-colors-org-file ()
  (interactive)
  (my/update-colors-org-file 'latex))

(defun my/update-html-colors-org-file ()
  (interactive)
  (my/update-colors-org-file 'html))

(when (bound-and-true-p my/latex-colors-alist)
  (progn (my/update-latex-colors-org-file)
	 (my/update-html-colors-org-file)))

(add-hook 'org-mode-hook #'my/org-fontify-color-macros)

;; Remaining configurations (org-modern, mixed-pitch, etc)
(use-package org-modern
  :after org
  :straight (:host github :repo "minad/org-modern" :branch "main")
  :custom ((org-modern-table nil)
	   (org-modern-label-border nil))
  :hook ((org-mode . org-modern-mode)
	 (org-mode . variable-pitch-mode)))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;; enhance latex stuff in org
(font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

;; Faces
(defun my/org--TeX-fold-setup ()
  (TeX-fold-mode)
  (unless (get 'my/org--TeX-fold-setup 'face-attribute-set)
    (set-face-attribute 'TeX-fold-folded-face nil :foreground nil :inherit 'shadow)
    (put 'my/org--TeX-fold-setup 'face-attribute-set t))
  (TeX-fold-buffer))

(add-hook 'org-mode-hook 'my/org--TeX-fold-setup)

;; Custom folded display for labels and refs
(defun my/TeX-fold-ref (text)
  (let* ((m (string-match "^\\([^:]+:\\)\\(.*\\)" text))
         (cat (or (match-string 1 text) ""))
         (ref (or (match-string 2 text) text)))
    (when (> (length ref) 13)
        (setq ref (concat (substring ref 0 6) "..." (substring ref -6))))
    (concat "[" (propertize cat 'face 'shadow) ref "]")))

(defun my/TeX-fold-label (&rest texts)
  (cl-loop for text in texts
           for m = (string-match "^\\([^:]+:\\)\\(.*\\)" text)
           for cat = (or (match-string 1 text) "")
           for ref = (or (match-string 2 text) text)
           collect (concat "[" (propertize cat 'face 'shadow) ref "]") into labels
           finally return (mapconcat #'identity labels ",")))

(setq-default TeX-fold-macro-spec-list
              '((my/TeX-fold-label ("cite"))
                (my/TeX-fold-label ("label"))
                (my/TeX-fold-ref ("ref" "pageref" "eqref" "footref"))))

(defun my/org-emphasis-autocomplete ()
  "Apply the chosen EMPHASIS-TYPE markup to the active region or word at point.
Uses `completing-read` to prompt the user for the emphasis style."
  (interactive)
  (let* ((options '(("Bold (*)" . ?*)
                   ("Italic (/)" . ?/)
                   ("Underline (_)" . ?_)
                   ("Strikethrough (+)" . ?+)
                   ("Code (~)" . ?~)
                   ("Verbatim (=)" . ?=)))
         (selection (minibuffer-with-setup-hook
                      (lambda ()
                        (let ((map (make-sparse-keymap)))
                          (set-keymap-parent map (current-local-map))
                          (dolist (key '(?* ?/ ?_ ?+ ?~ ?=))
                            (define-key map (char-to-string key)
                              (lambda ()
                                (interactive)
                                (delete-minibuffer-contents)
                                (insert (car (rassoc last-command-event options)))
                                (exit-minibuffer))))
                          (use-local-map map)))
                    (completing-read "Emphasis: " options nil t)))
         (char (cdr (assoc selection options))))
    (when char
      (org-emphasize char))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-f") #'my/org-emphasis-autocomplete))

(provide 'org-ui-config)
