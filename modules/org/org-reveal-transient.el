;;; org-reveal-transient.el --- Transient com opções do org-re-reveal -*- lexical-binding: t -*-

;;; Commentary:
;; Transient com opções do org-re-reveal para o tema Quanta.

;;; Code:

(require 'transient)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'seq)
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

;;;; Auto-animate
;; Slide: atributos em :REVEAL_EXTRA_ATTR: do heading.
;; Elemento: atributos em #+ATTR_HTML do elemento no cursor.
;; Os menus abrem com os valores que já estão no buffer.

(defconst my/org-reveal-easings
  '("ease" "linear" "ease-in" "ease-out" "ease-in-out" "step-start" "step-end")
  "Curvas CSS sugeridas; `cubic-bezier(...)' também é aceita.")

(defun my/org-reveal--read-easing (prompt initial-input history)
  "Lê uma curva CSS com PROMPT, INITIAL-INPUT e HISTORY."
  (completing-read prompt my/org-reveal-easings nil nil initial-input history))

(defun my/org-reveal--read-seconds (prompt initial-input _history)
  "Lê um número de segundos (>= 0) com PROMPT e INITIAL-INPUT."
  (let ((n (read-number prompt (and initial-input
                                    (string-to-number initial-input)))))
    (when (< n 0)
      (user-error "O tempo não pode ser negativo"))
    (number-to-string n)))

(defconst my/org-reveal--attr-regexp
  "\\([^ \t\n=\"]+\\)\\(?:=\\(?:\"\\([^\"]*\\)\"\\|\\([^ \t\n]*\\)\\)\\)?"
  "Um atributo HTML: nome, nome=valor ou nome=\"valor\".")

(defun my/org-reveal--parse-attrs (string)
  "Lista de (NOME VALOR TEXTO) dos atributos em STRING.
VALOR é nil para atributos sem valor; TEXTO é o trecho original."
  (let ((pos 0) attrs)
    (while (string-match my/org-reveal--attr-regexp string pos)
      (push (list (match-string 1 string)
                  (or (match-string 2 string) (match-string 3 string))
                  (match-string 0 string))
            attrs)
      (setq pos (match-end 0)))
    (nreverse attrs)))

(defun my/org-reveal--slide-aa-args ()
  "Argumentos do menu de slide a partir de :REVEAL_EXTRA_ATTR:."
  (my/org-reveal--require-heading)
  (let (args)
    (pcase-dolist (`(,name ,value ,_) (my/org-reveal--parse-attrs
                                       (or (org-entry-get nil "REVEAL_EXTRA_ATTR") "")))
      (pcase name
        ((or "data-auto-animate" "data-auto-animate-restart") (push name args))
        ("data-auto-animate-unmatched"
         (when (equal value "false") (push "data-auto-animate-unmatched=false" args)))
        ((or "data-auto-animate-duration" "data-auto-animate-easing" "data-auto-animate-id")
         (when value (push (concat name "=" value) args)))))
    (nreverse args)))

(defun my/org-reveal--arg->attr (arg)
  "Converte ARG do transient (nome ou nome=valor) em atributo HTML."
  (if (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" arg)
      (format "%s=\"%s\"" (match-string 1 arg) (match-string 2 arg))
    arg))

(transient-define-suffix my/org-reveal-auto-animate-apply (args)
  "Grava ARGS em :REVEAL_EXTRA_ATTR:, preservando os outros atributos."
  :description "Aplicar ao slide"
  (interactive (list (transient-args 'my/org-reveal-auto-animate)))
  (my/org-reveal--require-heading)
  (let* ((others (seq-remove
                  (lambda (a) (string-prefix-p "data-auto-animate" (car a)))
                  (my/org-reveal--parse-attrs
                   (or (org-entry-get nil "REVEAL_EXTRA_ATTR") ""))))
         (attrs (append (mapcar #'caddr others)
                        (mapcar #'my/org-reveal--arg->attr args))))
    (if attrs
        (org-entry-put nil "REVEAL_EXTRA_ATTR" (string-join attrs " "))
      (org-entry-delete nil "REVEAL_EXTRA_ATTR"))
    (when (and args (not (member "data-auto-animate" args)))
      (message "Atenção: sem data-auto-animate, os outros atributos não têm efeito"))))

(transient-define-prefix my/org-reveal-auto-animate ()
  "Auto-animate do slide (heading atual).
O slide anima a partir do anterior quando os dois têm data-auto-animate."
  :init-value (lambda (obj) (oset obj value (my/org-reveal--slide-aa-args)))
  ["Auto-animate do slide (:REVEAL_EXTRA_ATTR:)"
   ("a" "Ativar (também no slide vizinho)" "data-auto-animate")
   ("d" "Duração em segundos (padrão 1)" "data-auto-animate-duration="
    :reader my/org-reveal--read-seconds)
   ("e" "Curva (padrão ease)" "data-auto-animate-easing="
    :reader my/org-reveal--read-easing)
   ("u" "Sem fade nos elementos sem par" "data-auto-animate-unmatched=false")
   ("i" "Grupo: só anima entre slides com o mesmo id" "data-auto-animate-id=")
   ("r" "Reiniciar: não anima a partir do slide anterior" "data-auto-animate-restart")]
  [("RET" my/org-reveal-auto-animate-apply)])

(defconst my/org-reveal--element-aa-keys
  '(:data-id :data-auto-animate-delay :data-auto-animate-duration
    :data-auto-animate-easing)
  "Chaves de #+ATTR_HTML controladas pelo menu de elemento.")

(defun my/org-reveal--aa-element ()
  "Elemento que recebe o #+ATTR_HTML: a lista inteira, se estiver numa."
  (let ((el (org-element-at-point)))
    (when (or (org-before-first-heading-p)
              (memq (org-element-type el) '(headline nil)))
      (user-error "Posicione o cursor em um parágrafo, lista, imagem ou bloco"))
    (or (org-element-lineage el '(plain-list) t) el)))

(defun my/org-reveal--element-aa-args ()
  "Argumentos do menu de elemento a partir do #+ATTR_HTML atual."
  (let ((attrs (org-export-read-attribute :attr_html (my/org-reveal--aa-element)))
        args)
    (dolist (key my/org-reveal--element-aa-keys)
      (when-let* ((value (plist-get attrs key)))
        (push (format "%s=%s" (substring (symbol-name key) 1) value) args)))
    (nreverse args)))

(transient-define-suffix my/org-reveal-auto-animate-element-apply (args)
  "Grava ARGS no #+ATTR_HTML do elemento, preservando as outras chaves.
As linhas #+ATTR_HTML existentes são unidas em uma só."
  :description "Aplicar ao elemento"
  (interactive (list (transient-args 'my/org-reveal-auto-animate-element)))
  (let* ((el (my/org-reveal--aa-element))
         (attrs (org-export-read-attribute :attr_html el))
         (begin (copy-marker (org-element-property :begin el)))
         (post (copy-marker (org-element-property :post-affiliated el))))
    (dolist (key my/org-reveal--element-aa-keys)
      (setq attrs (org-plist-delete attrs key)))
    (dolist (arg args)
      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" arg)
        (setq attrs (append attrs (list (intern (concat ":" (match-string 1 arg)))
                                        (match-string 2 arg))))))
    (save-excursion
      (goto-char begin)
      (let ((case-fold-search t))
        (while (< (point) post)
          (if (looking-at-p "[ \t]*#\\+attr_html:")
              (delete-region (line-beginning-position) (line-beginning-position 2))
            (forward-line 1))))
      (goto-char post)
      (when attrs
        (insert (make-string (current-indentation) ?\s) "#+ATTR_HTML:"
                (mapconcat (lambda (x) (format " %s" x)) attrs "")
                "\n")))
    (set-marker begin nil)
    (set-marker post nil)))

(transient-define-prefix my/org-reveal-auto-animate-element ()
  "Auto-animate do elemento no cursor (#+ATTR_HTML).
Elementos com o mesmo data-id em slides vizinhos são animados juntos."
  :init-value (lambda (obj) (oset obj value (my/org-reveal--element-aa-args)))
  ["Auto-animate do elemento (#+ATTR_HTML; numa lista, vale a lista inteira)"
   ("i" "Par (data-id): mesmo id no slide vizinho" "data-id=")
   ("t" "Atraso em segundos" "data-auto-animate-delay="
    :reader my/org-reveal--read-seconds)
   ("d" "Duração em segundos (sobrepõe a do slide)" "data-auto-animate-duration="
    :reader my/org-reveal--read-seconds)
   ("e" "Curva (sobrepõe a do slide)" "data-auto-animate-easing="
    :reader my/org-reveal--read-easing)]
  [("RET" my/org-reveal-auto-animate-element-apply)])

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
    ("a" "Auto-animate do slide…" my/org-reveal-auto-animate)]
   ["Inserir"
    ("n" "Notas do apresentador" my/org-reveal-insert-notes)
    ("s" "Quebra de slide (split)" my/org-reveal-insert-split)
    ("f" "Fragmento" my/org-reveal-insert-fragment)
    ("e" "Auto-animate do elemento…" my/org-reveal-auto-animate-element)]
   ["Destaque"
    ("z" "Azul" my/org-reveal-azul)
    ("l" "Laranja" my/org-reveal-laranja)]])

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c v") #'my/org-reveal-transient))

(provide 'org-reveal-transient)
;;; org-reveal-transient.el ends here
