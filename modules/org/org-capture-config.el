(defcustom my/local-project-target-file "project.org"
  "Default Org file where project information and tracking are stored"
  :type 'file)

(defun my/org-capture-set-id-from-heading ()
  "Add an ID property based on a hash algorithm."
  (interactive)
  (when (org-at-heading-p)
    (let* ((title (org-get-heading t t t t))
           (hash (md5 title)))
      (org-set-property "ID" hash))))

(defun my/project-org-find-file ()
  "Returns the path for `my/local-project-target-file'."
  (require 'project)
  (if-let ((project (project-current)))
      (expand-file-name my/local-project-target-file (project-root project))
    (user-error "Not inside a project!")))

(defvar my/org-capture--source-buffer nil)
(defvar my/org-capture--region-beg-marker nil)
(defvar my/org-capture--region-end-marker nil)
(defvar my/org-capture--key nil)

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
    (let (id region-text)
      (with-current-buffer (marker-buffer org-capture-last-stored-marker)
        (goto-char org-capture-last-stored-marker)
        (org-back-to-heading t)
        (unless (org-entry-get nil "ID")
          (my/org-id-from-heading))
        (setq id (org-entry-get nil "ID")))
      (with-current-buffer my/org-capture--source-buffer
        (setq region-text
              (buffer-substring-no-properties
               my/org-capture--region-beg-marker
               my/org-capture--region-end-marker))
        (save-excursion
          (goto-char my/org-capture--region-beg-marker)
          (delete-region my/org-capture--region-beg-marker
                         my/org-capture--region-end-marker)
          (insert (format "[[id:%s][%s]]" id region-text)))))))

(defun my/org-capture--cleanup ()
  (setq my/org-capture--source-buffer nil
        my/org-capture--region-beg-marker nil
        my/org-capture--region-end-marker nil
        my/org-capture--key nil))

(advice-add 'org-capture :before #'my/org-capture--store-region-before)
(add-hook 'org-capture-mode-hook  #'my/org-capture--store-key)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture--insert-link t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture--cleanup t)
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (when (and my/org-capture--key
                       (string-prefix-p "p" my/org-capture--key))
              (org-id-update-id-locations
               (list (my/project-org-find-file)))))
          t)


(setq org-capture-templates-contexts
      '(("p"  ((lambda () (project-current))))
        ("pt" ((lambda () (project-current))))
        ("pb" ((lambda () (project-current))))))

(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
         "* UNSEEN %?\n")
        ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
         "* TODO %?\n")
        ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
         "* TODO <%<%Y-%m-%d %H:%M:%S>> \n %?\n")
        ("p" "Project")
        ("pt" "Tasks" entry (file+headline my/project-org-find-file "Tasks")
         "* TODO %<%Y%m%d%H%M%S> %?%i \n"
         :after-finalize my/org-capture-set-id-from-heading)
        ("pb" "Bugs" entry (file+headline my/project-org-find-file "Bugs")
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

(global-set-key (kbd "C-c c") 'org-capture)

(provide 'org-capture-config)
