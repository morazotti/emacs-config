(require 'transient)

(defun my/straight-update-package ()
  (interactive)
  (let ((pkg (straight--select-package "Select package to update:")))
    (straight-fetch-package pkg)
    (straight-pull-package pkg)
    (straight-rebuild-package pkg)))

(defun my/straight-visit-package-magit ()
  (interactive)
  (let ((pkg (straight--select-package "Select package to update:")))
    (straight-visit-package pkg)
    (magit-status)))

(transient-define-prefix straight-main-dispatch ()
  "Interactive dashboard and unified dispatch menu for `straight'."
  [:description
   "Straight"
   [""
    ("i" "Install..." straight-use-package)
    ("u" "Update..." my/straight-update-package)]
   [""
    ("f" "Fetch" straight-fetch-dispatch)
    ("l" "Pull" straight-pull-dispatch)
    ("p" "Push" straight-push-dispatch)]
   [""
    ("c" "Check" straight-check-dispatch)
    ("m" "Merge" straight-merge-dispatch)
    ("r" "Rebuild" straight-rebuild-dispatch)]
   [""
    ("v" "Version Control" straight-version-control-dispatch)
    ("w" "Watcher..." straight-watcher-dispatch)
    ("o" "Other..." straight-other-dispatch)]])

(transient-define-prefix straight-check-dispatch ()
  "Perform checks on packages using `straight'."
  [:description
   "Straight > Check"
   [""
    ("p" "Package..." straight-check-package)
    ("a" "All packages" straight-check-all)]])

(transient-define-prefix straight-merge-dispatch ()
  "Merge packages with their sources using `straight'."
  [:description
   "Straight > Merge"
   [""
    ("p" "Package..." straight-merge-package)
    ("d" "Package and dependencies..." straight-merge-package-and-deps)
    ("a" "All packages" straight-merge-all)]])

(transient-define-prefix straight-fetch-dispatch ()
  "Fetch updates for packages using `straight'."
  [:description
   "Straight > Fetch"
   [""
    ("p" "Package..." straight-fetch-package)
    ("d" "Package and dependencies..." straight-fetch-package-and-deps)
    ("a" "All packages" straight-fetch-all)]])

(transient-define-prefix straight-pull-dispatch ()
  "Pull updates for packages using `straight'."
  [:description
   "Straight > Pull"
   [""
    ("p" "Package..." straight-pull-package)
    ("d" "Package and dependencies..." straight-pull-package-and-deps)
    ("l" "Recipe repositories" straight-pull-recipe-repositories)
    ("a" "All packages" straight-pull-all)]])

(transient-define-prefix straight-push-dispatch ()
  "Push changes to package sources using `straight'."
  [:description
   "Straight > Push"
   [""
    ("p" "Package..." straight-push-package)
    ("a" "All packages" straight-push-all)]])

(transient-define-prefix straight-rebuild-dispatch ()
  "Rebuild packages using `straight'."
  [:description
   "Straight > Rebuild"
   [""
    ("p" "Package..." straight-rebuild-package)
    ("a" "All packages" straight-rebuild-all)]])

(transient-define-prefix straight-version-control-dispatch ()
  "Manage versions of packages using `straight'."
  [:description
   "Straight > VC"
   [""
    ("n" "Normalize..." straight-normalize-package)
    ("N" "Normalize all packages" straight-normalize-all)
    ("z" "Freeze" straight-freeze-versions)
    ("w" "Thaw" straight-thaw-versions)]])

(transient-define-prefix straight-other-dispatch ()
  "Perform other package management tasks using `straight'."
  [:description
   "Straight > Other"
   ["Maintenance"
    ("p" "Prune build" straight-prune-build)
    ("r" "Remove unused repositories" straight-remove-unused-repos)
    ("g" "Get recipe of..." straight-get-recipe)]
   ["Dependencies"
    ("d" "Dependencies of..." straight-dependencies)
    ("D" "Dependents on..." straight-dependents)]
   ["Visit"
    ("v" "Visit..." straight-visit-package)
    ("m" "Magit..." my/straight-visit-package-magit)
    ("w" "Visit website..." straight-visit-package-website)]])

(transient-define-prefix straight-watcher-dispatch ()
  "Control the package watcher using `straight'."
  [:description
   "Straight > Watcher"
   [""
    ("s" "Start watcher" straight-watcher-start)
    ("S" "Stop watcher" straight-watcher-stop)]])

(provide 'tools-transient-straight-config)
