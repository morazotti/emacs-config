(defmacro my/define-repeat-map (name bindings)
  "Cria um repeat-map chamado NAME-repeat-map com os BINDINGS fornecidos."
  (let* ((map-name (intern (format "my/%s-repeat-map" name)))
         (cmds (mapcar #'cdr bindings)))
    `(progn
       (defvar ,map-name
         (let ((map (make-sparse-keymap)))
           ,@(mapcar (lambda (b) `(define-key map ,(car b) #',(cdr b))) bindings)
           map)
         ,(format "Mapa de repetição para %s." name))
       ,@(mapcar (lambda (cmd) `(put #',cmd 'repeat-map ',map-name)) cmds))))

(provide 'core-custom-macros-config)
