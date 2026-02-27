(use-package org
  :after org
  :straight nil
  :config
  (defcustom my/local-project-target-file "project.org"
    "Default Org file where project information and tracking are stored"
    :type 'file)
  (defvar my/org-capture--source-buffer nil)
  (defvar my/org-capture--region-beg-marker nil)
  (defvar my/org-capture--region-end-marker nil)
  (defvar my/org-capture--key nil)

  (defun my/org-capture-set-id-from-heading ()
    "Add an ID property based on a hash algorithm."
    (interactive)
    (when (org-at-heading-p)
      (let* ((title (org-get-heading t t t t))
	     (hash (md5 title))
	     (ordered-id (my/org-get-max-ordered-id)))
	(org-set-property "ID" hash))))
  (defun my/org-capture--next-task-num (buffer)
  "Returns the next TASK_NUM for the project in BUFFER."
  (with-current-buffer buffer
    (let ((max-num 0))
      (org-map-entries
       (lambda ()
         (when-let ((num (org-entry-get nil "TASK_NUM")))
           (setq max-num (max max-num (string-to-number num)))))
       nil 'file)
      (1+ max-num))))
  (defun my/org-capture-find-project-file ()
    "Returns the path for `my/local-project-target-file'."
    (require 'project)
    (if-let ((project (project-current)))
	(expand-file-name my/local-project-target-file (project-root project))
      (user-error "Not inside a project!")))
  (defun my/org-capture--store-region-before (&rest _)
    (when (and (use-region-p) (null my/org-capture--source-buffer))
      (setq my/org-capture--source-buffer (current-buffer)
	    my/org-capture--region-beg-marker (copy-marker (region-beginning))
	    my/org-capture--region-end-marker (copy-marker (region-end) t))))
  (defun my/org-capture--store-key ()
    (setq my/org-capture--key (plist-get org-capture-current-plist :key)))
  (defun my/org-capture--insert-link ()
  (when (and my/org-capture--source-buffer
             my/org-capture--region-beg-marker
             my/org-capture--region-end-marker
             (markerp org-capture-last-stored-marker)
             (marker-buffer org-capture-last-stored-marker)
             (string-prefix-p "p" (or my/org-capture--key "")))
    (let (id task-num region-text)
      (with-current-buffer (marker-buffer org-capture-last-stored-marker)
        (goto-char org-capture-last-stored-marker)
        (org-back-to-heading t)
        ;; ID
        (unless (org-entry-get nil "ID")
          (my/org-id-from-heading))
        (setq id (org-entry-get nil "ID"))
        ;; TASK_NUM
        (setq task-num (my/org-capture--next-task-num (current-buffer)))
        (org-set-property "TASK_NUM" (number-to-string task-num))
        (save-buffer))
      (with-current-buffer my/org-capture--source-buffer
        (setq region-text
              (buffer-substring-no-properties
               my/org-capture--region-beg-marker
               my/org-capture--region-end-marker))
        (save-excursion
          (goto-char my/org-capture--region-beg-marker)
          (delete-region my/org-capture--region-beg-marker
                         my/org-capture--region-end-marker)
          (insert (format "(#%d) [[id:%s][%s]]"
                           task-num id region-text)))))))
  (defun my/org-capture--cleanup ()
    (setq my/org-capture--source-buffer nil
	  my/org-capture--region-beg-marker nil
	  my/org-capture--region-end-marker nil
	  my/org-capture--key nil))
  (defun my/org-capture--update-id-locations ()
    "Atualiza o índice de IDs para o project.org atual após capture de projeto."
    (when (and my/org-capture--key
               (string-prefix-p "p" my/org-capture--key))
      (org-id-update-id-locations
       (list (my/org-capture-find-project-file)))))
  (advice-add 'org-capture :before #'my/org-capture--store-region-before)

  :custom 
  (org-capture-templates-contexts
      '(("p"  ((lambda () (project-current))))
        ("pt" ((lambda () (project-current))))
        ("pb" ((lambda () (project-current))))))
  (org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
         "* UNSEEN %?\n")
        ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
         "* TODO %?\n")
        ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
         "* TODO <%<%Y-%m-%d %H:%M:%S>> \n %?\n")
        ("p" "Project")
        ("pt" "Tasks" entry (file+headline my/org-capture-find-project-file "Tasks")
         "* TODO %<%Y%m%d%H%M%S> %?%i \n"
         :after-finalize my/org-capture-set-id-from-heading)
        ("pb" "Bugs" entry (file+headline my/org-capture-find-project-file "Bugs")
         "* TODO %<%Y%m%d%H%M%S> %?%i \n"
         :after-finalize my/org-capture-set-id-from-heading)
        ("v" "Grupo de Virtudes" entry
         (file+olp "~/Documents/org/tasks.org" "Religião" "Grupo de Virtudes")
         "* TODO [N] Grupo de Virtudes\n SCHEDULED: %^T"
         :immediate-finish t)
        ("r" "Recolhimento" entry
         (file+olp "~/Documents/org/tasks.org" "Religião" "Recolhimento")
         "* TODO [N] Recolhimento\n SCHEDULED: %^T"
         :immediate-finish t)
        ("t" "Tasks" entry (file+headline "~/Documents/org/tasks.org" "Tarefas")
         "* TODO %?\n")))

  :bind
  ("C-c c" . org-capture)
  (:map prog-mode-map
	("C-c C-o" . org-open-at-point-global)))

(add-hook 'org-capture-mode-hook  #'my/org-capture--store-key)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture--insert-link t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture--update-id-locations t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture--cleanup t)

(provide 'org-capture-config)
