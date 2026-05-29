(defun my/define-repeat-map (name bindings)
  "Cria um repeat-map dinamicamente sem quebrar o compilador."
  (let ((map-name (intern (format "my-%s-repeat-map" name)))
        (map (make-sparse-keymap)))
    ;; 1. Popula o keymap com os atalhos
    (dolist (binding bindings)
      (define-key map (car binding) (cdr binding)))
    ;; 2. Define a variável global do mapa se ela não existir
    (set map-name map)
    ;; 3. Vincula cada comando ao mapa criado
    (dolist (binding bindings)
      (put (cdr binding) 'repeat-map map-name))))

(provide 'core-custom-macros-config)
