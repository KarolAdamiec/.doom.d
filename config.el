;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Karol Adamiec"
      user-mail-address "karol.adamiec@icloud.com")
(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt
             mac-right-option-modifier  'alt)))
(setq confirm-kill-emacs nil)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka SS04" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;STOP ASKING!
(map! "C-x k" 'kill-current-buffer)

(map! "M-o" 'ace-window)
;;Set modified buffer to orange, default red is too much...
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))


(use-package! keycast
  :after (doom-modeline)
  :config
  (setq keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
                                        ;(setq keycast-mode-line-insert-after "%e")
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  :hook
  (doom-modeline-mode . keycast-mode-line-mode))

(beacon-mode t)

;;show counter when searching
(setq isearch-lazy-count t)
;;treat space in isearch as non greedy any content
;;(setq search-whitespace-regexp ".*?")
(use-package! gptel
  :config

  (setq! gptel-temperature 0))

(add-hook! (clojure-mode emacs-lisp-mode lisp-mode
                         cider-repl-mode racket-mode racket-repl-mode)
  (enable-paredit-mode))

(setq +format-on-save-disabled-modes
      '(clojure-mode
        clojurescript-mode
        ))
;; Reuse buffers for Dired, dont like when it creates a separate one each time.
(after! dired
  (setf dired-kill-when-opening-new-dired-buffer t))

;; Cider PopUp goes to right, instead of bottom.

;; (after! cider
;;   (set-popup-rules!
;;     '(("^\\*cider-repl"
;;        :side right
;;        :width 100
;;        :quit nil
;;        :ttl nil))))

(after! cider
  ;; work around logging issues, figwheel-main vs cider ... fight!
  ;; FORTUM from magnusn
  (defun cider-figwheel-workaround--boot-up-cljs ()
    (format "(boot-up-cljs %s)" cider-figwheel-main-default-options))
  (map! "C-c l l" 'cider-repl-clear-buffer)
  (map! "C-c c f" 'cider-format-defun)
  (cider-register-cljs-repl-type 'boot-up-cljs #'cider-figwheel-workaround--boot-up-cljs))
