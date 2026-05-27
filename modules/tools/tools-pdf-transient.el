;;; tools-pdf-transient.el --- Transient menu for PDF annotations -*- lexical-binding: t -*-

;;; Commentary:
;; A transient menu for pdf-tools annotations.

;;; Code:

(require 'transient)
(require 'pdf-tools nil t)
(require 'pdf-annot nil t)

(defun tools-pdf-annot-add-highlight-and-edit ()
  "Add highlight annotation and edit its contents."
  (interactive)
  (let ((annot (call-interactively #'pdf-annot-add-highlight-markup-annotation)))
    (when annot (pdf-annot-edit-contents annot))))

(defun tools-pdf-annot-add-underline-and-edit ()
  "Add underline annotation and edit its contents."
  (interactive)
  (let ((annot (call-interactively #'pdf-annot-add-underline-markup-annotation)))
    (when annot (pdf-annot-edit-contents annot))))

(defun tools-pdf-annot-add-strikeout-and-edit ()
  "Add strikeout annotation and edit its contents."
  (interactive)
  (let ((annot (call-interactively #'pdf-annot-add-strikeout-markup-annotation)))
    (when annot (pdf-annot-edit-contents annot))))

(defun tools-pdf-annot-add-squiggly-and-edit ()
  "Add squiggly annotation and edit its contents."
  (interactive)
  (let ((annot (call-interactively #'pdf-annot-add-squiggly-markup-annotation)))
    (when annot (pdf-annot-edit-contents annot))))

(defun tools-pdf-annot-add-text-and-edit ()
  "Add text annotation and edit its contents."
  (interactive)
  (let ((annot (call-interactively #'pdf-annot-add-text-annotation)))
    (when annot (pdf-annot-edit-contents annot))))

(transient-define-prefix tools-pdf-transient ()
  "Transient menu for PDF annotations."
  [["Add Markup"
    ("h" "Highlight" tools-pdf-annot-add-highlight-and-edit)
    ("u" "Underline" tools-pdf-annot-add-underline-and-edit)
    ("s" "Strikeout" tools-pdf-annot-add-strikeout-and-edit)
    ("q" "Squiggly"  tools-pdf-annot-add-squiggly-and-edit)]
   ["Add Other"
    ("t" "Text"      tools-pdf-annot-add-text-and-edit)]
   ["Manage"
    ("d" "Delete"     pdf-annot-delete)
    ("e" "Edit annot" pdf-annot-edit)
    ("a" "Attachment" pdf-annot-attachment-dired)
    ("l" "List"       pdf-annot-list-annotations)]])

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-M-s-a") #'tools-pdf-transient))

(provide 'tools-pdf-transient)
;;; tools-pdf-transient.el ends here
