;;; agent-shell-subagent.el --- Subagentes visíveis no agent-shell -*- lexical-binding: t; -*-

(require 'tab-bar)
(require 'seq)
(require 'map)
(require 'agent-shell)

(defvar agent-shell-subagent-tab "subagentes"
  "Nome da tab onde ficam todos os subagentes.")

(defun agent-shell-subagent--ensure-tab ()
  "Seleciona a tab de subagentes, criando-a se ela não existir."
  (if (member agent-shell-subagent-tab
              (mapcar (lambda (tb) (alist-get 'name tb)) (tab-bar-tabs)))
      (tab-bar-switch-to-tab agent-shell-subagent-tab)
    (tab-bar-new-tab)
    (tab-bar-rename-tab agent-shell-subagent-tab)))

(defun agent-shell-subagent--windows ()
  "Windows da tab atual que estão mostrando um agent-shell."
  (seq-filter
   (lambda (w)
     (with-current-buffer (window-buffer w)
       (derived-mode-p 'agent-shell-mode)))
   (window-list nil 'no-minibuf)))

(defun agent-shell-subagent--send-when-ready (buf prompt)
  "Envia PROMPT a BUF quando o agente terminar de inicializar.
Ter `:session :id' não basta: um prompt enviado antes do evento
`init-finished' fica marcado como ocupado, mas nunca chega ao agente."
  (let ((send (lambda ()
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (agent-shell-insert :text prompt :submit t))))))
    (if (agent-shell-initialized-p :shell-buffer buf)
        (funcall send)
      (agent-shell-subscribe-to
       :shell-buffer buf
       :event 'init-finished
       ;; Fora do handler do evento, para não reentrar na inicialização.
       :on-event (lambda (_event) (run-at-time 0 nil send))))))

;;;###autoload
(defun agent-shell-subagent-spawn (task prompt)
  "Abre um novo agent-shell `subagente-TASK', envia PROMPT e submete."
  (agent-shell-subagent--ensure-tab)
  (let* ((wins (agent-shell-subagent--windows))
         (win (if wins
                  (split-window (car (last wins)) nil 'right)
                (selected-window)))
         (name (format "subagente-%s" task))
         ;; `agent-shell' (C-u) mostra o shell novo, mas não o torna o buffer
         ;; corrente em `emacsclient --eval'; `agent-shell--start' devolve
         ;; o buffer criado.  `new' evita o seletor de sessões da estratégia
         ;; `prompt', que trava quando dois subagentes iniciam juntos.
         (buf (agent-shell--start :config (agent-shell--auto-preferred-config)
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new)))
    (set-window-buffer win buf)
    (select-window win)
    ;; `rename-buffer' puro quebra o shell-maker, que acha o próprio buffer
    ;; pelo nome: o submit falha com (wrong-type-argument processp nil).
    (shell-maker-set-buffer-name buf name)
    (balance-windows)
    (agent-shell-subagent--send-when-ready buf prompt)
    (buffer-name buf)))

(provide 'agent-shell-subagent)
;;; agent-shell-subagent.el ends here
