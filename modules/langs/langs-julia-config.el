(use-package julia-repl)
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :interpreter ("julia" . julia-mode)
  :bind
  (:map julia-mode-map
        ("C-c C-z" . julia-repl)
        ("C-c C-c" . julia-repl-send-buffer)
        ("C-<return>" . julia-repl-send-region-or-line)
        ("C-<enter>" . julia-repl-send-region-or-line)
        ("C-c C-d" . julia-repl-doc)))

(provide 'langs-julia-config)
