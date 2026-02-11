(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
         "* UNSEEN %?\n")
        ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
         "* TODO %?\n")
        ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
         "* TODO <%<%Y-%m-%d %H:%M:%S>> \n %?\n")
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
