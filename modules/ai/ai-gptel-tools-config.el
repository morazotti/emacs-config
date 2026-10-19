;;; ai-gptel-config.el --- GPTel configuration -*- lexical-binding: t; -*-

(gptel-make-tool
 :name "git"
 :description "Execute a non-interactive git command to inspect history, branch status, diffs, or create commits. Do not run interactive commands."
 :args (list '(:name "command"
		     :type "string"
		     :description "The git command and arguments to execute, excluding the 'git' prefix. For example: 'status', 'log -n 5', 'diff HEAD~1', or 'commit -am \"message\"'."))
 :function (lambda (command)
	     (let ((default-directory (or (vc-root-dir) default-directory)))
	       (with-temp-buffer
		 (let ((exit-code (call-process-shell-command
				   (concat "git --no-pager " command)
				   nil t nil)))
		   (concat (if (= exit-code 0)
			       "Success:\n"
			     (format "Failed (exit code %d):\n" exit-code))
			   (buffer-string)))))))

(gptel-make-tool
 :name "magit_status"
 :description "Open Magit status buffer for the current repository in an Emacs window for the user."
 :args nil
 :function (lambda ()
	     (require 'magit)
	     (let ((root (or (vc-root-dir) default-directory)))
	       (magit-status-setup-buffer root)
	       (format "Magit status buffer opened for %s" root))))

(provide 'ai-gptel-tools-config)
