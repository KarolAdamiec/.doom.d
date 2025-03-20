;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

(package! speed-type)   ;speed typing at my fingertips!
                                        ;(package! keycast :pin "c47fa154c756abd044da4a1353c30b978782f7dc")      ;shows commands in modeline AKA "wtf did i just do?"
(package! beacon)       ;beacon of light when needed, to show where the point is and active window.
(package! keycast)
(package! gptel)
(package! google-translate)
(package! gif-screencast)
(package! impatient-mode)
(package! kubel)
(package! difftastic)
(package! mise) ;use mise setup vars per buffer and in eshell



;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! clj-refactor :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")
;;(package! cider :pin  "8b3dabeefa8a3352d7a30a9fd9027c05a4c4f6e24f4a521")        ;;Cider 1.1.1 YAY!
;;(package! clojure-mode :pin "0e886656c83e6e8771f748ec698bb173adcb0968")  ;; clojure-mode 5.13.0
;;(package! clj-refactor :pin "b24ce76acefe792975f00147c94b4dd784e65b80") ;;v 2.5.1
                                        ;(package! doom-modeline :pin "bf880ae56f3f6aab7bd334de9bd9b455c63a24c0")
;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
