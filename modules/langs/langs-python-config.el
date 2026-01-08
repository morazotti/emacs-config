(use-package python-mode
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))


(use-package python-black)
(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'langs-python-config)
