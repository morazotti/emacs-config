(transient-define-prefix straight-main-dispatch ()
  "Interactive dashboard and unified dispatch menu for `straight'."
  [:description
   "Transient for straight"
   [""
    ("i" "Install..." straight-use-package)]
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
   "Transient for straight"
   ["Check"
    ("p" "Package..." straight-check-package)
    ("a" "All packages" straight-check-all)]])

(transient-define-prefix straight-merge-dispatch ()
  "Merge packages with their sources using `straight'."
  [:description
   "Transient for straight"
   ["Merge"
    ("p" "Package..." straight-merge-package)
    ("d" "Package and dependencies..." straight-merge-package-and-deps)
    ("a" "All packages" straight-merge-all)]])

(transient-define-prefix straight-fetch-dispatch ()
  "Fetch updates for packages using `straight'."
  [:description
   "Transient for straight"
   ["Fetch"
    ("p" "Package..." straight-fetch-package)
    ("d" "Package and dependencies..." straight-fetch-package-and-deps)
    ("a" "All packages" straight-fetch-all)]])

(transient-define-prefix straight-pull-dispatch ()
  "Pull updates for packages using `straight'."
  [:description
   "Transient for straight"
   ["Pull"
    ("p" "Package..." straight-pull-package)
    ("d" "Package and dependencies..." straight-pull-package-and-deps)
    ("l" "... recipe repositories" straight-pull-recipe-repositories)
    ("a" "All packages" straight-pull-all)]])

(transient-define-prefix straight-push-dispatch ()
  "Push changes to package sources using `straight'."
  [:description
   "Transient for straight"
   ["Push"
    ("p" "Package..." straight-push-package)
    ("a" "All packages" straight-push-all)]])

(transient-define-prefix straight-rebuild-dispatch ()
  "Rebuild packages using `straight'."
  [:description
   "Transient for straight"
   ["Rebuild"
    ("r" "Package..." straight-rebuild-package)
    ("a" "All packages" straight-rebuild-all)]])

(transient-define-prefix straight-version-control-dispatch ()
  "Manage versions of packages using `straight'."
  [:description
   "Transient for straight"
   ["Version Control"
    ("n" "Normalize..." straight-normalize-package)
    ("N" "Normalize all packages" straight-normalize-all)
    ("z" "Freeze" straight-freeze-versions)
    ("w" "Thaw" straight-thaw-versions)]])

(transient-define-prefix straight-other-dispatch ()
  "Perform other package management tasks using `straight'."
  [:description
   "Transient for straight"
   ["Other"
    ("p" "Prune build" straight-prune-build)
    ("r" "Remove unused repositories" straight-remove-unused-repos)
    ("g" "Get recipe of..." straight-get-recipe)]
   ["Dependencies"
    ("d" "Dependencies of..." straight-dependencies)
    ("D" "Dependents on..." straight-dependents)]
   ["Visit"
    ("v" "Visit..." straight-visit-package)
    ("w" "Visit website..." straight-visit-package-website)]])

(transient-define-prefix straight-watcher-dispatch ()
  "Control the package watcher using `straight'."
  [:description
   "Transient for straight"
   ["Watcher"
    ("s" "Start watcher" straight-watcher-start)
    ("S" "Stop watcher" straight-watcher-stop)]])

(provide 'transient-straight)
