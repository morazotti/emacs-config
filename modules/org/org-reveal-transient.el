;;; org-reveal-transient.el --- Transient com opções do org-re-reveal -*- lexical-binding: t -*-

;;; Commentary:
;; Transient com opções do org-re-reveal para o tema Quanta.

;;; Code:

(require 'transient)
(require 'org)
(require 'org-element)
(require 'org-re-reveal)

(defconst my/org-reveal-backgrounds
  '(("azul quanta" . "#242F62")
    ("laranja quanta" . "#E53B11")
    ("branco (remover)" . nil)))

(defconst my/org-reveal-transitions
  '("none" "fade" "slide" "convex" "concave" "zoom"))

(defconst my/org-reveal-frag-styles
  '("appear" "fade-in" "fade-up" "fade-out" "grow" "shrink"
    "highlight-current-blue"))

(defun my/org-reveal--require-heading ()
  "Sinaliza erro se o ponto estiver antes do primeiro heading."
  (when (org-before-first-heading-p)
    (user-error "Posicione o cursor em um heading (slide)")))

(defun my/org-reveal-set-background (color)
  "Define REVEAL_BACKGROUND do heading atual como COLOR."
  (interactive
   (let ((choice (completing-read
                  "Fundo: "
                  (append (mapcar #'car my/org-reveal-backgrounds)
                          '("outra cor..."))
                  nil t)))
     (list (if (equal choice "outra cor...")
               (read-string "Cor CSS: ")
             (cdr (assoc choice my/org-reveal-backgrounds))))))
  (my/org-reveal--require-heading)
  (if (or (null color) (string= color ""))
      (org-entry-delete nil "REVEAL_BACKGROUND")
    (org-entry-put nil "REVEAL_BACKGROUND" color)))

(defun my/org-reveal-set-transition (transition)
  "Define REVEAL_DATA_TRANSITION do heading atual como TRANSITION."
  (interactive
   (list (completing-read "Transição (vazio remove): "
                          my/org-reveal-transitions nil t)))
  (my/org-reveal--require-heading)
  (if (string= transition "")
      (org-entry-delete nil "REVEAL_DATA_TRANSITION")
    (org-entry-put nil "REVEAL_DATA_TRANSITION" transition)))

(defun my/org-reveal-toggle-auto-animate ()
  "Alterna data-auto-animate em REVEAL_EXTRA_ATTR do heading atual."
  (interactive)
  (my/org-reveal--require-heading)
  (let* ((tokens (split-string (or (org-entry-get nil "REVEAL_EXTRA_ATTR") "")))
         (had (member "data-auto-animate" tokens)))
    (setq tokens (if had
                     (delete "data-auto-animate" tokens)
                   (append tokens (list "data-auto-animate"))))
    (if (null tokens)
        (org-entry-delete nil "REVEAL_EXTRA_ATTR")
      (org-entry-put nil "REVEAL_EXTRA_ATTR" (string-join tokens " ")))
    (message "data-auto-animate %s" (if had "removido" "ativado"))))

(defun my/org-reveal-insert-notes ()
  "Insere bloco notes; com região ativa, envolve a região."
  (interactive)
  (org-insert-structure-template "notes"))

(defun my/org-reveal-insert-split ()
  "Insere quebra de slide acima da linha atual."
  (interactive)
  (beginning-of-line)
  (insert "#+REVEAL: split\n"))

(defun my/org-reveal-insert-fragment (style)
  "Insere #+ATTR_REVEAL com fragmento STYLE no elemento atual."
  (interactive
   (list (completing-read "Fragmento: " my/org-reveal-frag-styles
                          nil t nil nil "appear")))
  (save-excursion
    (if (org-in-item-p)
        (let ((top (save-excursion
                     (goto-char (org-in-item-p))
                     (org-list-get-top-point (org-list-struct)))))
          (goto-char top)
          (when (let ((case-fold-search t))
                  (save-excursion
                    (and (zerop (forward-line -1))
                         (looking-at-p "^[ \t]*#\\+ATTR_REVEAL:"))))
            (user-error "A lista já tem #+ATTR_REVEAL"))
          (beginning-of-line)
          (insert (make-string (current-indentation) ?\s)
                  "#+ATTR_REVEAL: :frag (" style ")\n"))
      (let ((el (org-element-at-point)))
        (when (or (memq (org-element-type el) '(headline nil))
                  (org-before-first-heading-p))
          (user-error "Posicione o cursor em um parágrafo, lista ou bloco"))
        (when (org-element-property :attr_reveal el)
          (user-error "O elemento já tem #+ATTR_REVEAL"))
        (goto-char (org-element-property :post-affiliated el))
        (beginning-of-line)
        (insert (make-string (current-indentation) ?\s)
                "#+ATTR_REVEAL: :frag " style "\n")))))

(defun my/org-reveal--wrap-macro (name)
  "Envolve a região (ou insere vazio) na macro NAME."
  (if (use-region-p)
      (let ((texto (replace-regexp-in-string
                    "," "\\\\,"
                    (buffer-substring (region-beginning) (region-end)))))
        (delete-region (region-beginning) (region-end))
        (insert (format "{{{%s(%s)}}}" name texto)))
    (insert (format "{{{%s()}}}" name))
    (backward-char 4)))

(defun my/org-reveal-azul ()
  "Aplica a macro azul."
  (interactive)
  (my/org-reveal--wrap-macro "azul"))

(defun my/org-reveal-laranja ()
  "Aplica a macro laranja."
  (interactive)
  (my/org-reveal--wrap-macro "laranja"))

(transient-define-prefix my/org-reveal-transient ()
  "Opções do org-re-reveal (tema Quanta)."
  [["Exportar"
    ("R" "HTML" org-re-reveal-export-to-html)
    ("B" "HTML e abrir" org-re-reveal-export-to-html-and-browse)
    ("S" "Subárvore atual" org-re-reveal-export-current-subtree)
    ("N" "Nova apresentação" my/presentation-new)]
   ["Slide (heading atual)"
    ("b" "Fundo" my/org-reveal-set-background)
    ("t" "Transição" my/org-reveal-set-transition)
    ("a" "Auto-animate (alternar)" my/org-reveal-toggle-auto-animate)]
   ["Inserir"
    ("n" "Notas do apresentador" my/org-reveal-insert-notes)
    ("s" "Quebra de slide (split)" my/org-reveal-insert-split)
    ("f" "Fragmento" my/org-reveal-insert-fragment)]
   ["Destaque"
    ("z" "Azul" my/org-reveal-azul)
    ("l" "Laranja" my/org-reveal-laranja)]])

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c v") #'my/org-reveal-transient))

(provide 'org-reveal-transient)
;;; org-reveal-transient.el ends here
