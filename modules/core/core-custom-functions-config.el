(defun my/define-repeat-map (name bindings)
  "Cria um repeat-map dinamicamente sem quebrar o compilador."
  (let ((map-name (intern (format "my/%s-repeat-map" name)))
        (map (make-sparse-keymap)))
    (dolist (binding bindings)
      (define-key map (car binding) (cdr binding)))
    (set map-name map)
    (dolist (binding bindings)
      (put (cdr binding) 'repeat-map map-name))))

(provide 'core-custom-functions-config)
