(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      user-lisp-directory (file-name-concat user-emacs-directory "modules"))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
