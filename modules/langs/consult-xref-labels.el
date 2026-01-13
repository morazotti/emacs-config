(defun my/xref-labels ()
  "Return a list of (XREF . LABEL) for LaTeX labels."
  (save-excursion
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "\\\\label{\\([^}]+\\)}" nil t)
        (let* ((label (match-string 1))
               (file (buffer-file-name))
               (line (line-number-at-pos))
               (xref (xref-make
                      (format "\\label{%s}" label)
                      (xref-make-file-location file line 0))))
          (push (cons xref label) results)))
      (nreverse results))))

(defun my/insert-ref-via-xref ()
  "Select a LaTeX label using xref + consult and insert \\ref{} at point."
  (interactive)
  (let* ((origin (point))
         (origin-buf (current-buffer))
         (pairs (my/xref-labels))
         ;; Hash tables para lookup
         (label-table (make-hash-table :test 'equal))
         (marker-table (make-hash-table :test 'equal))
         (display-strings
          (mapcar
           (lambda (pair)
             (let* ((xref (car pair))
                    (label (cdr pair))
                    (loc (xref-item-location xref))
                    (file (xref-file-location-file loc))
                    (line (xref-file-location-line loc))
                    (marker (with-current-buffer (find-file-noselect file)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- line))
                                (point-marker))))
                    (display (xref-item-summary xref)))
               (puthash display label label-table)
               (puthash display marker marker-table)
               display))
           pairs))
         ;; State customizado para preview
         (preview-state
          (let ((saved-buf nil)
                (saved-pos nil)
                (saved-win nil))
            (lambda (action cand)
              (pcase action
                ('setup
                 (setq saved-buf (current-buffer)
                       saved-pos (point)
                       saved-win (selected-window)))
                ('preview
                 (when cand
                   (let ((marker (gethash cand marker-table)))
                     (when (and marker (markerp marker) (marker-buffer marker))
                       (switch-to-buffer (marker-buffer marker))
                       (goto-char marker)
                       (recenter)))))
                ('exit
                 (when saved-buf
                   (select-window saved-win)
                   (switch-to-buffer saved-buf)
                   (goto-char saved-pos)))))))
         (selected
          (consult--read
           display-strings
           :prompt "Choose label: "
           :require-match t
           :state preview-state)))
    (when selected
      (let ((label (gethash selected label-table)))
        (insert (format "\\ref{%s}" label))))))
(setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)

(provide 'consult-xref-labels)
