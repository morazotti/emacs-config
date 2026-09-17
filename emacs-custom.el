(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-command-prefix "\3o")
 '(biblio-download-directory "~/Downloads/")
 '(bibtex-autokey-name-year-separator "_")
 '(bibtex-autokey-titleword-length 'infty)
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "_")
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(ebib-keywords
   '("Quantum Computation" "Computer Science - Cryptography and Security"
     "Lie groups" "Lie algebras" "Physics - Optics"
     "Technical writing" "Technical English" "Style" "Rhetoric"
     "English language" "Business writing" "Business English"
     "Computer Science - Information Theory"
     "Mathematics - Mathematical Physics" "Attention Network"
     "Molecular evolution" "Computational models"
     "Quantitative Biology - Quantitative Methods"
     "Quantitative Biology - Populations and Evolution"
     "Quantitative Biology - Biomolecules" "81Q93"
     "Quantum Physics (quant-ph)"
     "Nonlinear Sciences - Cellular Automata and Lattice Gases"
     "Structural Biology"
     "Computer Science - Computer Vision and Pattern Recognition"
     "Geodesics (Mathematics)" "Riemannian}"
     "Physics - Atomic Physics"
     "Computer Science - Emerging Technologies"
     "Physics - Chemical Physics"
     "Condensed Matter - Disordered Systems and Neural Networks"
     "Quantum States Geometry" "Quantum Circuits"
     "Physics - Applied Physics" "Rafael"
     "Exterior differential systems" "Control theory" "Congresses"
     "Unread" "Biotechnology" "Mathematics - Statistics Theory"
     "Physics - Computational Physics"
     "FOS: Computer and information sciences"
     "FOS: Computer and information\12                  sciences"
     "Computational Physics\12                  (physics.comp-ph)"
     "Applied Physics\12                  (physics.app-ph)"
     "Machine Learning (cs.LG)" "Physics - Physics and Society"
     "Computer Science - Social and Information Networks"
     "FOS: Physical sciences"
     "FOS: Physical\12                  sciences"
     "Classical Physics (physics.class-ph)"
     "Statistical Mechanics (cond-mat.stat-mech)"
     "Astrophysics - Earth and Planetary Astrophysics"
     "Python (Computer program language)"
     "Neural networks (Computer science)" "Mathematical Physics"
     "Computer Science - Neural and Evolutionary Computing"
     "Computer Science - Computation and Language"
     "Condensed Matter - Mesoscale and Nanoscale Physics"
     "Open systems (Physics)" "Computer Science - Robotics"
     "Quantum theory" "Nonlinear Sciences - Chaotic Dynamics"
     "Condensed Matter - Statistical Mechanics"
     "Computer Science - Artificial Intelligence"
     "Mathematics - Differential Geometry" "58A32}" "58A20" "58A05"
     "53C35" "53B05" "{22E65"
     "Condensed Matter - Other Condensed Matter"
     "Condensed Matter - Strongly Correlated Electrons"
     "Condensed Matter - Quantum Gases" "Quantum computers"
     "Statistics - Machine Learning" "Metabolomics"
     "Quantum Complexity" "High Energy Physics - Theory"
     "Finsler spaces" "Differential}" "{Geometry" "Quantum Tomography"
     "Quantitative Biology - Genomics" "Synthetic Biology"
     "Computer Science - Machine Learning" "Biology" "Quantum Physics"
     "General Relativity and Quantum Cosmology"
     "Computer Science - Computational Complexity"))
 '(ebib-reading-list-template "* %M %T\12\12\12:PROPERTIES:\12%K\12:END:\12")
 '(image-auto-resize 'fit-window)
 '(org-export-smart-quotes-alist
   '(("pt"
      (primary-opening :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"
		       :texinfo "``")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "}" :texinfo
		       "''")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex
			 "\\enquote*{" :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("pt-br"
      (primary-opening :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"
		       :texinfo "``")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "}" :texinfo
		       "''")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex
			 "\\enquote*{" :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("ar"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "‹" :html "&lsaquo;" :latex
			 "\\guilsinglleft{}" :texinfo
			 "@guilsinglleft{}")
      (secondary-closing :utf-8 "›" :html "&rsaquo;" :latex
			 "\\guilsinglright{}" :texinfo
			 "@guilsinglright{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("da"
      (primary-opening :utf-8 "»" :html "&raquo;" :latex ">>" :texinfo
		       "@guillemetright{}")
      (primary-closing :utf-8 "«" :html "&laquo;" :latex "<<" :texinfo
		       "@guillemetleft{}")
      (secondary-opening :utf-8 "›" :html "&rsaquo;" :latex "\\frq{}"
			 :texinfo "@guilsinglright{}")
      (secondary-closing :utf-8 "‹" :html "&lsaquo;" :latex "\\flq{}"
			 :texinfo "@guilsingleft{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("de"
      (primary-opening :utf-8 "„" :html "&bdquo;" :latex "\"`"
		       :texinfo "@quotedblbase{}")
      (primary-closing :utf-8 "“" :html "&ldquo;" :latex "\"'"
		       :texinfo "@quotedblleft{}")
      (secondary-opening :utf-8 "‚" :html "&sbquo;" :latex "\\glq{}"
			 :texinfo "@quotesinglbase{}")
      (secondary-closing :utf-8 "‘" :html "&lsquo;" :latex "\\grq{}"
			 :texinfo "@quoteleft{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("el"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "“" :html "&ldquo;" :latex "``"
			 :texinfo "``")
      (secondary-closing :utf-8 "”" :html "&rdquo;" :latex "''"
			 :texinfo "''")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("en"
      (primary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo
		       "``")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo
		       "''")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("es"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "“" :html "&ldquo;" :latex "``"
			 :texinfo "``")
      (secondary-closing :utf-8 "”" :html "&rdquo;" :latex "''"
			 :texinfo "''")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("fa"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "‹" :html "&lsaquo;" :latex
			 "\\guilsinglleft{}" :texinfo
			 "@guilsinglleft{}")
      (secondary-closing :utf-8 "›" :html "&rsaquo;" :latex
			 "\\guilsinglright{}" :texinfo
			 "@guilsinglright{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("fr"
      (primary-opening :utf-8 "« " :html "&laquo;&nbsp;" :latex
		       "\\og " :texinfo "@guillemetleft{}@tie{}")
      (primary-closing :utf-8 " »" :html "&nbsp;&raquo;" :latex
		       "\\fg{}" :texinfo "@tie{}@guillemetright{}")
      (secondary-opening :utf-8 "“" :html "&ldquo;" :latex "``"
			 :texinfo "``")
      (secondary-closing :utf-8 "”" :html "&rdquo;" :latex "''"
			 :texinfo "''")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("is"
      (primary-opening :utf-8 "„" :html "&bdquo;" :latex "\"`"
		       :texinfo "@quotedblbase{}")
      (primary-closing :utf-8 "“" :html "&ldquo;" :latex "\"'"
		       :texinfo "@quotedblleft{}")
      (secondary-opening :utf-8 "‚" :html "&sbquo;" :latex "\\glq{}"
			 :texinfo "@quotesinglbase{}")
      (secondary-closing :utf-8 "‘" :html "&lsquo;" :latex "\\grq{}"
			 :texinfo "@quoteleft{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("it"
      (primary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo
		       "``")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo
		       "''")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("no"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("nb"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("nn"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex
		       "\\guillemotleft{}" :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex
		       "\\guillemotright{}" :texinfo
		       "@guillemetright{}")
      (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("ro"
      (primary-opening :utf-8 "„" :html "&bdquo;" :latex "\"`"
		       :texinfo "@quotedblbase{}")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo
		       "''")
      (secondary-opening :utf-8 "«" :html "&laquo;" :latex
			 "\\guillemotleft{}" :texinfo
			 "@guillemetleft{}")
      (secondary-closing :utf-8 "»" :html "&raquo;" :latex
			 "\\guillemotright{}" :texinfo
			 "@guillemetright{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("ru"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex "{}<<"
		       :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex ">>{}"
		       :texinfo "@guillemetright{}")
      (secondary-opening :utf-8 "„" :html "&bdquo;" :latex "\\glqq{}"
			 :texinfo "@quotedblbase{}")
      (secondary-closing :utf-8 "“" :html "&ldquo;" :latex "\\grqq{}"
			 :texinfo "@quotedblleft{}")
      (apostrophe :utf-8 "’" :html "&#39;"))
     ("sl"
      (primary-opening :utf-8 "«" :html "&laquo;" :latex "{}<<"
		       :texinfo "@guillemetleft{}")
      (primary-closing :utf-8 "»" :html "&raquo;" :latex ">>{}"
		       :texinfo "@guillemetright{}")
      (secondary-opening :utf-8 "„" :html "&bdquo;" :latex "\\glqq{}"
			 :texinfo "@quotedblbase{}")
      (secondary-closing :utf-8 "“" :html "&ldquo;" :latex "\\grqq{}"
			 :texinfo "@quotedblleft{}")
      (apostrophe :utf-8 "’" :html "&rsquo;"))
     ("sv"
      (primary-opening :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo
		       "’’")
      (primary-closing :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo
		       "’’")
      (secondary-opening :utf-8 "’" :html "&rsquo;" :latex "’"
			 :texinfo "`")
      (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "’"
			 :texinfo "'")
      (apostrophe :utf-8 "’" :html "&rsquo;"))))
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-hide-macro-markers t)
 '(org-image-actual-width '(350))
 '(org-image-align 'center)
 '(org-latex-preview-appearance-options
   '(:foreground default :background default :scale 2.0 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")) nil nil "Customized with use-package org")
 '(org-latex-src-block-backend 'minted nil nil "Customized with use-package org")
 '(org-modern-hide-stars " ")
 '(org-modern-star 'replace)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-ql-views
   '(("Agenda Files: (and (scheduled :from today) (todo \"TODO\"))"
      :buffers-files
      ("/home/nicolas/Dropbox/my_library/reading-list.org"
       "/home/nicolas/Documents/org/tasks.org"
       "/home/nicolas/Documents/org/work.org"
       "/home/nicolas/Documents/roam/20260824084837-acompanhamento_quanta_ai_de.org"
       "/home/nicolas/Documents/org/opusdei.org")
      :query (and (scheduled :from today) (todo "TODO")) :sort
      (todo priority) :narrow nil :super-groups nil :title
      "Agenda Files: (and (scheduled :from today) (todo \"TODO\"))")
     ("Overview: Agenda-like" :buffers-files org-agenda-files :query
      (and (not (done))
	   (or (habit) (deadline auto) (scheduled :to today)
	       (ts-active :on today)))
      :sort (todo priority date) :super-groups org-super-agenda-groups
      :title "Agenda-like")
     ("Overview: NEXT tasks" :buffers-files org-agenda-files :query
      (todo "NEXT") :sort (date priority) :super-groups
      org-super-agenda-groups :title "Overview: NEXT tasks")
     ("Calendar: Today" :buffers-files org-agenda-files :query
      (ts-active :on today) :title "Today" :super-groups
      org-super-agenda-groups :sort (priority))
     ("Calendar: This week"
      . #[0
	  "\301 \302\303\304\305\304\306\304\307\310\301 \311\1!\10>\204\34\0\312\313\314\3D\"\210\211\315H\204\232\0\211\315\316\317\320\311\6\6!\10>\2048\0\312\313\314\6\10D\"\210\5\321H\204\223\0\5\321\311\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\203\215\0\4\203\215\0\3\203\215\0\2\203\215\0\1\203\215\0\211\203\215\0\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\202\221\0\330 \266\206I\210\5\321H\"!I\210\211\315H\262\1[\6\12#&\7\302\303\332\305\333\306\333\307\310\327\301 \311\1!\10>\204\300\0\312\313\314\3D\"\210\211\315H\204>\1\211\315\316\317\320\311\6\6!\10>\204\334\0\312\313\314\6\10D\"\210\5\321H\2047\1\5\321\311\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\2031\1\4\2031\1\3\2031\1\2\2031\1\1\2031\1\211\2031\1\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\2025\1\330 \266\206I\210\5\321H\"!I\210\211\315H\262\1Z\6\13#&\7\334\335 \336\337\5\340\6\6\257\5\341\342\343\344\345\346&\10\207"
	  [cl-struct-ts-tags ts-now ts-apply :hour 0 :minute :second
			     ts-adjust day type-of signal
			     wrong-type-argument ts 7 string-to-number
			     format-time-string "%w" 17 3 2 1 4 5 6
			     float-time encode-time 23 59
			     org-ql-search org-agenda-files ts-active
			     :from :to :title "This week"
			     :super-groups org-super-agenda-groups
			     :sort (priority)]
	  34
	  "Show items with an active timestamp during this calendar week."
	  nil])
     ("Calendar: Next week"
      . #[0
	  "\301\302\303\304 #\305\306\307\310\307\311\307\301\302\304 \312\1!\10>\204 \0\313\314\315\3D\"\210\211\303H\204\236\0\211\303\316\317\320\312\6\6!\10>\204<\0\313\314\315\6\10D\"\210\5\321H\204\227\0\5\321\312\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\203\221\0\4\203\221\0\3\203\221\0\2\203\221\0\1\203\221\0\211\203\221\0\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\202\225\0\330 \266\206I\210\5\321H\"!I\210\211\303H\262\1[\6\12#&\7\305\306\332\310\333\311\333\301\302\327\304 \312\1!\10>\204\304\0\313\314\315\3D\"\210\211\303H\204B\1\211\303\316\317\320\312\6\6!\10>\204\340\0\313\314\315\6\10D\"\210\5\321H\204;\1\5\321\312\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\2035\1\4\2035\1\3\2035\1\2\2035\1\1\2035\1\211\2035\1\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\2029\1\330 \266\206I\210\5\321H\"!I\210\211\303H\262\1Z\6\13#&\7\334\335 \336\337\5\340\6\6\257\5\341\342\343\344\345\346&\10\207"
	  [cl-struct-ts-tags ts-adjust day 7 ts-now ts-apply :hour 0
			     :minute :second type-of signal
			     wrong-type-argument ts string-to-number
			     format-time-string "%w" 17 3 2 1 4 5 6
			     float-time encode-time 23 59
			     org-ql-search org-agenda-files ts-active
			     :from :to :title "Next week"
			     :super-groups org-super-agenda-groups
			     :sort (priority)]
	  34
	  "Show items with an active timestamp during the next calendar week."
	  nil])
     ("Review: Recently timestamped" . org-ql-view-recent-items)
     (#("Review: Dangling tasks" 0 22
	(help-echo "Tasks whose ancestor is done"))
      :buffers-files org-agenda-files :query
      (and (todo) (ancestors (done))) :title
      #("Review: Dangling tasks" 0 22
	(help-echo "Tasks whose ancestor is done"))
      :sort (todo priority date) :super-groups ((:auto-parent t)))
     (#("Review: Stale tasks" 0 19
	(help-echo "Tasks without a timestamp in the past 2 weeks"))
      :buffers-files org-agenda-files :query
      (and (todo) (not (ts :from -14))) :title
      #("Review: Stale tasks" 0 19
	(help-echo "Tasks without a timestamp in the past 2 weeks"))
      :sort (todo priority date) :super-groups ((:auto-parent t)))
     (#("Review: Stuck projects" 0 22
	(help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :buffers-files org-agenda-files :query
      (and (todo) (descendants (todo))
	   (not (descendants (todo "NEXT"))))
      :title
      #("Review: Stuck projects" 0 22
	(help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :sort (date priority) :super-groups org-super-agenda-groups)))
 '(org-safe-remote-resources '("\\`\\[\\[file:early-init\\.el]]\\'"))
 '(org-startup-with-latex-preview t)
 '(org-use-sub-superscripts nil)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((jinx-local-words
      . "UdU eq eqref expval mathbb mathbf mathrm notacao otimes propto relacao sqrt su varsigma")
     (eval and (fboundp 'gptel-mode) (gptel-mode 1))
     (jinx-languages . "pt_BR") (jinx-languages . "en_US pt_BR")
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (shell-command "pandoc README.org -o README.md"))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#f9e2af" :foreground "#f9e2af"))))
 '(diff-hl-delete ((t (:background "#f38ba8" :foreground "#f38ba8"))))
 '(diff-hl-insert ((t (:background "#a6e3a1" :foreground "#a6e3a1"))))
 '(italic ((t (:slant italic))))
 '(jinx-misspelled ((t (:underline (:color "red" :style wave :position nil)))))
 '(org-document-title ((t (:weight bold :height 1.0))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-level-1 ((t (:extend nil :foreground "medium blue" :weight bold :height 1.2 :family "CMU Serif"))))
 '(org-level-2 ((t (:foreground "#d20f39" :weight bold :height 1.1 :family "CMU Serif"))))
 '(org-level-3 ((t (:foreground "#883aef" :weight bold :height 1.00005 :family "CMU Serif"))))
 '(org-modern-date-active ((t (:inherit (default fixed-pitch) :background "gray90" :foreground "black" :height 0.8 :family "Ligamonacop"))))
 '(org-modern-date-inactive ((t (:inherit (default fixed-pitch) :background "gray90" :foreground "gray30" :height 0.8))))
 '(org-modern-time-active ((t (0.8 (default fixed-pitch) :inherit :height :background "gray35" :foreground "white" :weight semibold))))
 '(org-modern-time-inactive ((t (:inherit (default fixed-pitch) :background "gray50" :foreground "gray95" :height 0.8))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit fixed-pitch :foreground "#fe640b"))))
 '(visual-shorthands-face ((t (:inherit font-lock-keyword-face :extend nil :background "#e0e0e0")))))
