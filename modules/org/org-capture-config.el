(use-package org
  :after org
  :straight nil

  :custom 
  (org-capture-templates
   '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
      "* UNSEEN %?\n")
     ("f" "Nutrition" )
     ("w" "Work")
     ("wd" "Daily" entry (file+olp+datetree "~/Documents/org/work.org" "Diário")
      "* %?\n" :empty-lines 1)
     ("wc" "Clock" entry (clock)
      "%?\n" :empty-lines 1)
     ("wm" "Meeting" entry (file+olp+datetree "~/Documents/org/work.org" "Reuniões")
      "%?\n" :empty-lines 1)
     ("wp" "Pessoas")
     ("wpa" "Amanda" entry (file+headline "~/Documents/roam/20260824084837-acompanhamento_quanta_ai_de.org" "<<<Amanda>>>")
      "** %t \n %?")
     ("wpx" "Axel" entry (file+headline "~/Documents/roam/20260824084837-acompanhamento_quanta_ai_de.org" "<<<Axel>>>")
      "** %t \n %?")
     ("wpn" "Nico" entry (file+headline "~/Documents/roam/20260824084837-acompanhamento_quanta_ai_de.org" "<<<Nico>>>")
      "** %t \n %?")
      ("wpt" "Tiago" entry (file+headline "~/Documents/roam/20260824084837-acompanhamento_quanta_ai_de.org" "<<<Tiago>>>")
       "** %t \n %?")
     ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
      "* TODO %?\n")
     ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
      "* TODO %T \n %?\n")
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
  ("C-c c" . org-capture))

(use-package org-snitch
  :config
  (org-snitch-setup)
  (org-snitch-mode 1)
  (with-eval-after-load 'git-commit
    (define-key git-commit-mode-map (kbd "C-c C-t") #'org-snitch-magit-insert-task))

  :bind
  (("C-c s" . org-snitch-dispatch)
   :map org-snitch-link-mode-map
   ("C-c C-o" . org-open-at-point-global)
   ("C-c C-d" . org-snitch-mark-done))
  
  :hook
  (prog-mode . org-snitch-link-mode))



(provide 'org-capture-config)
