;; Alinhamento visual (em pixels) de tabelas Org, considerando previews
;; de LaTeX (overlays com `display' de imagem).
(use-package valign
  :ensure t
  :hook (org-mode . valign-mode)
  :custom (valign-fancy-bar nil))

(defvar-local my/valign--pending-tables nil
  "Posições de início de tabelas Org a realinhar após mudança de preview LaTeX.")
(defvar-local my/valign--timer nil)

(defun my/valign--flush (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq my/valign--timer nil)
      (let ((tables (delete-dups (prog1 my/valign--pending-tables
                                   (setq my/valign--pending-tables nil))))
            (win (get-buffer-window buf)))
        ;; valign mede com a janela selecionada; sem janela viva, descarta.
        (when (and win valign-mode)
          (with-selected-window win
            (save-excursion
              (dolist (pos tables)
                (goto-char pos)
                (when (org-at-table-p)
                  (valign-table))))))))))

(defun my/valign-on-latex-overlay (ov)
  "Agenda realinhamento da tabela que contém o overlay de preview OV."
  (when-let* ((buf (overlay-buffer ov))
              ((buffer-local-value 'valign-mode buf)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (overlay-start ov))
        (when (org-at-table-p)
          (push (org-table-begin) my/valign--pending-tables)
          (unless my/valign--timer
            (setq my/valign--timer
                  (run-with-idle-timer 0.1 nil #'my/valign--flush buf))))))))

(dolist (hook '(org-latex-preview-overlay-update-functions
                org-latex-preview-overlay-open-functions
                org-latex-preview-overlay-close-functions))
  (add-hook hook #'my/valign-on-latex-overlay))

(provide 'org-table-config)
