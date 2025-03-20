;;; functions.el -*- lexical-binding: t; -*-


;;; Some cool expansion stuff
;;; 
(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun he--closest-in-this-buffer (old beg-function search-function)
  (let (expansion)
    (unless old
      (he-init-string (funcall beg-function) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (funcall search-function he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (funcall search-function he-search-string))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (he--closest-in-this-buffer old #'he-dabbrev-beg #'he-dabbrev-search))

(defun try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (unless (equal he-search-string "")
      (save-excursion
        (save-restriction
          (when hippie-expand-no-restriction
            (widen))

          (let (forward-point
                backward-point
                forward-distance
                backward-distance
                forward-expansion
                backward-expansion
                chosen)

            ;; search backward
            (goto-char he-search-loc-backward)
            (setq expansion (he-line-search he-search-string
                                            strip-prompt t))

            (when expansion
              (setq backward-expansion expansion)
              (setq backward-point (point))
              (setq backward-distance (- he-string-beg backward-point)))

            ;; search forward
            (goto-char he-search-loc-forward)
            (setq expansion (he-line-search he-search-string
                                            strip-prompt nil))

            (when expansion
              (setq forward-expansion expansion)
              (setq forward-point (point))
              (setq forward-distance (- forward-point he-string-beg)))

            ;; choose depending on distance
            (setq chosen (cond
                          ((and forward-point backward-point)
                           (if (< forward-distance backward-distance) :forward :backward))

                          (forward-point :forward)
                          (backward-point :backward)))

            (when (equal chosen :forward)
              (setq expansion forward-expansion)
              (set-marker he-search-loc-forward forward-point))

            (when (equal chosen :backward)
              (setq expansion backward-expansion)
              (set-marker he-search-loc-backward backward-point))

            ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

(defun he-sexp-search (pattern &optional reverse limit)
  (when (if reverse
            (search-backward pattern nil t)
          (search-forward pattern nil t))
    (ignore-errors
      (buffer-substring-no-properties (if reverse
                                          (point)
                                        (save-excursion
                                          (paredit-backward-up 1)
                                          (point)))
                                      (save-excursion
                                        (if reverse
                                            (paredit-forward 1)
                                          (paredit-forward-up 1))
                                        (paredit-backward-down 1)
                                        (point))))))

(defun he-sexp-beg ()
  (save-excursion (paredit-backward-up 1) (point)))

(defun try-expand-sexp-closest-first (old)
  "Try to complete the current sexp to an entire sexp in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string). It returns t if a new completion is found, nil otherwise."
  (he--closest-in-this-buffer old #'he-sexp-beg #'he-sexp-search))

(defun try-expand-sexp-all-buffers (old)
  "Try to expand sexp \"dynamically\", searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (he--all-buffers old #'he-sexp-beg #'he-sexp-search))

;; Hippie expand: sometimes too hip
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-closest-first
                                         try-complete-file-name
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Create own function to expand lines (C-S-.)
(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         (if paredit-mode
             '(try-expand-sexp-closest-first
               try-expand-sexp-all-buffers)
           '(try-expand-line-closest-first
             try-expand-line-all-buffers))))
    (unless paredit-mode (end-of-line))
    (hippie-expand nil)
    (indent-region he-string-beg (point))))

;; Don't case-fold when expanding with hippe
(defun hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

;;(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)


(map! :after paredit
      :map paredit-mode-map
      "C-," #'hippie-expand-lines)

(defun my-cider-pprint-eval-sexp-up-to-point (&optional  output-to-current-buffer)
  "Evaluate the current sexp form up to point and pretty print.
 It constructs an expression to eval in the following manner:

- It finds the code between the point and the start of the sexp expression;
- It balances this bit of code by closing the expression;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (up-list) (backward-list) (point)))
         (beg-delimiter (save-excursion (up-list) (backward-list) (char-after)))
         (beg-set?  (save-excursion (up-list) (backward-list) (char-before)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (code (if (= beg-set? ?#) (concat (list beg-set?) code) code))
         (code (concat code (list (cider--matching-delimiter beg-delimiter))))
         (buffer (current-buffer))
         (result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-handler result-buffer code buffer)))
    (cider-interactive-eval code
                            handler
                            (list beg-of-sexp (point))
                            (cider--nrepl-pr-request-map))))
(map! :after cider
      :map cider-mode-map
      "C-c C-v C-o" #'my-cider-pprint-eval-sexp-up-to-point)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument makes the windows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))


(map! "C-x w t" #'toggle-window-split)
(map! "C-x w r" #'rotate-windows)
