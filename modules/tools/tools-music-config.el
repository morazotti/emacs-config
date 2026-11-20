(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  
  ;; Carrega as funções padrão do EMMS
  (emms-all)
  (emms-default-players)
  (emms-player-mpd-connect)

  ;; Define o backend do MPD como prioritário
)

(provide 'tools-music-config)
