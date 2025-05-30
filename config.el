;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Karol Adamiec"
      user-mail-address "karol.adamiec@icloud.com")
(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'nil
                                        ; mac-function-modifier      'hyper
             mac-right-option-modifier  'super))
      (IS-WINDOWS
       (setq w32-pass-rwindow-to-system nil
             w32-rwindow-modifier       'meta ; Right Windows key
             mac-option-modifier        'alt
             mac-right-option-modifier  'alt)))
(setq confirm-kill-emacs nil)

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
(setq doom-font (font-spec :family "Iosevka SS04" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

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
(map! "C-x k" #'kill-current-buffer)

(map! "M-o" #'ace-window)
(map! "M-g M-g" #'avy-goto-char)
(map! "M-g g" #'avy-goto-line)


;;Yegge tip:
;;Use back kill word more often
(map! "C-w" #'backward-kill-word)
(map! "C-x C-w" #'kill-region)

;;Set modified buffer to orange, default red is too much...
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))



(defmacro comment (&rest a))
(comment)  (use-package! keycast
             :after (doom-modeline)
             :config
             (setq keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
                                        ;(setq keycast-mode-line-insert-after "%e")
             (add-to-list 'global-mode-string '("" keycast-mode-line))
             :hook
             (doom-modeline-mode . keycast-mode-line-mode))

(use-package! mise
  :hook
  (after-init-hook . global-mise-mode))

(use-package! difftastic
  :after magit
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(beacon-mode t)

;;show counter when searching
(setq isearch-lazy-count t)

;;treat space in isearch as non greedy any content
;;(setq search-whitespace-regexp ".*?")

(use-package! gptel
  :defer t
  :config

  (setq! gptel-temperature 0))


;;check if this is still needed
(add-hook! (clojure-mode emacs-lisp-mode lisp-mode
                         cider-repl-mode racket-mode racket-repl-mode)
  (enable-paredit-mode))


;;see https://github.com/doomemacs/doomemacs/issues/7599#issuecomment-1884831702
(setq-hook! 'clojure-mode-hook apheleia-inhibit t)
(setq-hook! 'clojurescript-mode-hook apheleia-inhibit t)


(use-package! gif-screencast
  :defer t)

(use-package! apheleia) ;; Load formatting lib immediatelly, so the above disabled modes are procesed corectly. does not work really...
(setq +format-on-save-disabled-modes
      '(clojure-mode
        clojurescript-mode))


(map! "C-c t t" #'google-translate-smooth-translate)

(setq google-translate-translation-directions-alist
      '(("no" . "en") ("en" . "no") ("en" . "sv") ("en" . "fi") ))
(setq google-translate-output-destination 'kill-ring)

;; Reuse buffers for Dired, dont like when it creates a separate one each time.
(after! dired
  (setf dired-kill-when-opening-new-dired-buffer t))



(comment
 (after! emacs-everywhere
   ;; Easier to match with a bspwm rule:
   ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
   (setq emacs-everywhere-frame-name-format "emacs-anywhere")

   ;; The modeline is not useful to me in the popup window. It looks much nicer
   ;; to hide it.
   (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

   ;; Semi-center it over the target window, rather than at the cursor position
   ;; (which could be anywhere).
   (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
     :override #'emacs-everywhere-set-frame-position
     (cl-destructuring-bind (x y width height)
         (emacs-everywhere-window-geometry window-info)
       (set-frame-position frame
                           (+ x (/ width 2) (- (/ width 2)))
                           (+ y (/ height 2)))))))




;;(custom-set-faces! '(default :background "#000000"))


;;(custom-set-faces! '(echo-area :background "#000000"))

;; (setq emacs-everywhere-frame-parameters
;;       `((name . "emacs-everywhere")
;;         (fullscreen . nil)              ; Helps on GNOME at least
;;         (width . 100)
;;         (height . 30)))

;; Cider PopUp goes to right, instead of bottom.

;; (after! cider
;;   (set-popup-rules!
;;     '(("^\\*cider-repl"
;;        :side right
;;        :width 100
;;        :quit nil
;;        :ttl nil))))
(after! clj-refactor
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-favor-private-functions nil)
  (setq cljr-insert-newline-after-require nil)
  (setq cljr-assume-language-context "clj")

  (cljr-add-keybindings-with-modifier "C-M-S-s-")
  )

;; LSP package name is called lsp-mode.
(after! lsp-mode
  (setq lsp-keymap-prefix "C-M-s-l")
  )


(after! projectile
  (define-key projectile-mode-map (kbd "s-,") 'projectile-command-map))

(after! cider
  ;; work around logging issues, figwheel-main vs cider ... fight!
  ;; FORTUM from magnars
  (defun cider-figwheel-workaround--boot-up-cljs ()
    (format "(boot-up-cljs %s)" cider-figwheel-main-default-options))

  (cider-register-cljs-repl-type 'boot-up-cljs #'cider-figwheel-workaround--boot-up-cljs))

(map! :after cider
      :map cider-mode-map
      :prefix "C-c"
      "l l" 'cider-repl-clear-buffer
      "c f" 'cider-format-defun)
;;;;;;;;;;;;;
;;;;;;;;;;;;;TODO: Move this gold to separate file and clean up
(load! "functions")

(add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'undecorated nil)
(set-frame-parameter nil 'fullScreen 'maximized)
