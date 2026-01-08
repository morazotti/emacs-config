(use-package python-mode
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))


(use-package python-black)
(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(provide 'langs-python-config)
