(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/tasks.org")
                             (concat org-directory "/opusdei.org")))
(global-set-key (kbd "C-c a") 'org-agenda)

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current) subtree-end nil)))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""((org-agenda-skip-function
                       '(or (air-org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))))))
        ("p" "Planning" ((tags-todo "+@planning-@work")
                         (todo "BACKLOG" )))
        ("u" "Untagged Tasks" ((tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks")))
                               (todo ".*" ((org-agenda-files '("~/Documents/org/notes.org"))
                                           (org-agenda-overriding-header "Inbox"))))) 
        ("w" "Work" ((tags-todo "+@work" ((org-agenda-overriding-header "Work Tasks")))
                     (agenda "" ((org-agenda-overriding-header "Work Agenda")
                                 (org-agenda-files '("~/Documents/org/tasks.org"))
                                 (org-agenda-skip-entry-if 'notscheduled 'notdeadline)
                                 (org-agenda-span 'week)))))))

;; super agenda - not sure if I should use it again
;; (use-package org-super-agenda
;;   :init (org-super-agenda-mode 1)
;;   ;; :init (setq initial-buffer-choice (lambda () (get-buffer "*Org Agenda*")))
;;   :config (setq org-super-agenda-groups
;;                 '(
;;                   (:auto-property "ProjectId")

;;                   ;; (:name "blackgenn"
;;                   ;;        :scheduled today)
;;                   (:name "emacs" :tag "emacs")
;;                   (:name "books"
;;                          :and (:tag "book")))))

(provide 'org-agenda-config)
