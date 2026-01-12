(use-package jupyter
  :straight
  (:host github
         :repo "emacs-jupyter/jupyter"
         :branch "master"
         :commit "f97f4b5d8c83e0b901020f835183dde8a2bf649e")
  :bind ("C-M-s-h" . jupyter-org-hydra/body))


;; manage ssh servers
;; we can set :session as
;; [/ssh:[USER@]HOST:]~/.local/share/jupyter/runtime/kernel-<PID>.json
;; if we name a session, otherwise, it launches a local ipykernel3
;; useful if we want to launch local uv venvs

(defun my/choose-ssh-host (ssh-config-file)
  "Read SSH config file and allow the user to choose a host.
    If the chosen host is not 'localhost', prefix it with '/ssh:<chosen-host>:'."
  (let ((hosts '("localhost"))
    	(kern-dir "~/.local/share/jupyter/runtime/"))  ;; Add the "local" option to the hosts list
    ;; Read the SSH config file
    (with-temp-buffer
      (insert-file-contents ssh-config-file)
      ;; Use regex to find "Host" definitions
      (while (re-search-forward "^Host\\s-+\\(.*\\)" nil t)
        (push (match-string 1) hosts)))
    ;; Use completing-read to select a host
    (let ((chosen-host (completing-read "Choose SSH host: " (reverse hosts))))
      (if (string= chosen-host "localhost")
          (message kern-dir)
        (message (concat "/ssh:" chosen-host ":" kern-dir))))))

(defun my/jupyter-runtime-folder ()
  (interactive)
  (expand-file-name (my/choose-ssh-host "~/.ssh/config"))) 

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (interactive)
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes (my/jupyter-runtime-folder) t ".*kernel")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: " files)))
;; (seq-filter
;;  (lambda (file)
;;    (member (cdr file) ports))
;;  files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel"
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                 (file-name-nondirectory (my/select-jupyter-kernel))))

(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete (seq-filter
		     (lambda (file)
		       (not (member (cdr file) ports)))
		     files)))
    (when (and (length> to-delete 0)
	       (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

(defun my/jupyter-launch-local-kernel ()
  (interactive)
  (let* ((venv (concat (getenv "VIRTUAL_ENV") ".venv/" ))
	 (process-environment process-environment)
	 (venv-string (concat "VIRTUAL_ENV=" venv))
	 (jupyter-bin (format "%sbin/jupyter" venv))
	 (cmd-kernelspec-list (format "%s %s kernelspec list" venv-string jupyter-bin))
	 (kernel-and-location
	  (completing-read "Choose kernel: "
			   (seq-remove #'string-empty-p (mapcar 'string-trim-left (cdr (split-string (shell-command-to-string cmd-kernelspec-list) "\n"))))
			   nil t))
	 (kernel
	  (when (string-match "\\([a-zA-Z0-9\-.]+\\) *\\(.*\\)" kernel-and-location)
	    (match-string 1 kernel-and-location)))
	 (command `(,jupyter-bin "kernel" "--kernel" ,kernel)))
    (make-process :name "virtual-jupyter"
		  :buffer "*Jupyter Kernel*"
		  :command command)))

(provide 'org-ob-jupyter-config)
