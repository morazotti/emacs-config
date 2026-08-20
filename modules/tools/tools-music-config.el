(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  
  ;; Carrega as funções padrão do EMMS
  (emms-all)
  (emms-default-players)

  ;; Define o backend do MPD como prioritário
  (setq emms-player-list '(emms-player-mpd))

  ;; Conexão com o daemon do MPD
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6601")

  ;; Caminho absoluto da sua pasta de músicas (igual à do mpd.conf)
  (setq emms-player-mpd-music-directory "/home/nicolas/Music")

  ;; Sincroniza a playlist do buffer com a fila real do daemon
  (setq emms-player-mpd-sync-playlist t)

  ;; Sincroniza a playlist/cache do EMMS diretamente com o banco do MPD
  (setq emms-info-asynchronously nil)
  (setq emms-info-functions '(emms-info-mpd))
  
  ;; Sincroniza o volume do EMMS com o mixer do MPD
  (setq emms-volume-change-function 'emms-volume-mpd-change))

(provide 'tools-music-config)
