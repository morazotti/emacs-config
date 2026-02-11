(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/tasks.org")
                             (concat org-directory "/casamento.org")
                             (concat org-directory "/teste.org")
                             (concat org-directory "/google.org")))

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
          (agenda ""
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'regexp "[N]")
                         (let ((f (buffer-file-name (org-base-buffer (current-buffer)))))
                           (when (and f (string-match-p "unknown" (file-name-nondirectory f)))
                             (point-max)))))))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))
        ("p" "Planning" ((tags-todo "+@planning-@work")
                         (todo "BACKLOG")))
        ("u" "Untagged Tasks" ((tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks")))
                               (todo ".*" ((org-agenda-files '("~/Documents/org/notes.org"))
                                           (org-agenda-overriding-header "Inbox")))))))

(use-package org-gcal
  :straight (:host github :repo "kidd/org-gcal.el" :branch "master")
  :init 
  (let ((credentials (auth-source-search :host "api.agenda.com")))
    (setq org-gcal-client-id (plist-get (car credentials) :user)
          org-gcal-client-secret (funcall (plist-get (car credentials) :secret))))
  (org-gcal-reload-client-id-secret)
  :custom
  (org-gcal-fetch-file-alist
   '(("nicolas.morazotti@gmail.com" . "~/Documents/org/google.org")
     ("7adcf625da32815dfccbb68647750bbf892bf81654988afdb482302e6167dce1@group.calendar.google.com" . "~/Documents/org/casamento.org")
     ("727300d39128859d25197c5da2893082433ddf8a2b5a5a0a93cf65ae5838f756@group.calendar.google.com" . "~/Documents/org/teste.org"))))

(auth-source-search :host "midgard")
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
