;; Functions

(require 'cl)
;; (defun* rk-start-term (name commands)
;;   (let ((buffer-name (get-buffer (s-concat "*" name "*")))) (let (
;;     (buffer buffer-name)
;;     (window (get-buffer-window buffer-name))
;;   )
;;     (when (and buffer window)
;;       (select-window window)
;;       (return-from rk-start-term)
;;     )
;;     (when buffer
;;       (switch-to-buffer buffer)
;;       (return-from rk-start-term)
;;     )
;;     (ansi-term "/bin/bash" name)
;;     (dolist (command commands)
;;       (term-send-raw-string (s-concat command "\n"))
;;     )
;;   ))
;; )

(defun rk-replace-multiple (replacements string)
  "Replace multiple substrings in `string` according to `replacements`."
  (dolist (replace replacements string)
    (setq string (replace-regexp-in-string (car replace) (cdr replace) string t t)))
  string)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rk-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun rk--move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun rk-indent-according-to-mode ()
  (interactive)
  (if (use-region-p)
    (indent-region (region-beginning) (region-end))
    (indent-according-to-mode)
  )
)


(defun rk-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rk-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun rk-just-one-space ()
  (interactive)
  ;(cycle-spacing 0 t 'single-shot)
  (just-one-space -1)
  (just-one-space 0)
)

(defun rk-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun rk-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun rk-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun rk-stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun rk-switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun rk-mv-char-fwd ()
  (interactive)
  (set-mark-command nil)
  (right-char 1)
  (whole-line-or-region-kill-region 1)
  (right-word 1)
  (whole-line-or-region-yank nil)
  (left-char 1))

(defun rk-mv-char-bwd ()
  (interactive)
  (set-mark-command nil)
  (right-char 1)
  (whole-line-or-region-kill-region 1)
  (left-word 1)
  (whole-line-or-region-yank nil)
  (left-char 1))

(require 'hi-lock)
(defun rk-unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
  (interactive)
  (unhighlight-regexp t))

(defun rk-python-import-popwin ()
  (interactive)
  (python-import)
  (import-popwin)
)

(defun rk-kill-buffer-and-delete-window ()
  (interactive)
  (kill-this-buffer)
  (delete-window)
)

(defun rk-ansi-term-bash ()
  (interactive)
  (ansi-term "/bin/bash")
)

; (require 'dash)

(require 'cl)
(defun rk-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers regexp)))

(defun rk-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun rk-mc-mark-next-like-this-symbol ()
  (interactive)
  (mc/mark-next-like-this-symbol -1)
)

(defun rk-swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end
window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))

(defun rk-query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open files"
  (interactive "sQuery Replace in open Buffers: \nsquery with: ")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (beginning-of-buffer)
       (query-replace arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))

(defvar rk-you-cant-touch-this-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'rk-you-cant-touch-this-mode)

(defun rk-you-cant-touch-this-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq rk-you-cant-touch-this-mode (not rk-you-cant-touch-this-mode))
  (set-window-dedicated-p (selected-window) (if
    rk-you-cant-touch-this-mode
    "weakdedication"
    ,
nil
  ))
  (set-frame-parameter nil 'unsplittable rk-you-cant-touch-this-mode)
  (if (not (assq 'rk-you-cant-touch-this-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(rk-you-cant-touch-this-mode " UCTT")
                  minor-mode-alist)))
  (force-mode-line-update)
)

(defun rk-scroll-current-line-to-top ()
  (interactive)
  (let ((current-prefix-arg 0))
    (call-interactively 'recenter-top-bottom))
)

(defun rk-open-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  )

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))


(defun rk-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun rk-kill-other-buffers () 
  (interactive)                                                                   
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun rk-back-window ()
  (interactive)
  (other-window -1))

(defun rk-zap-up-to-char-reverse()
  (interactive)
  (let ((current-prefix-arg '(-1)))
    (call-interactively 'zap-up-to-char)))


;; (defhydra hydra-windows ()
;;   "C-arrow = switch, S-arrow = size, M-arrow = move"
;;   ("<left>" windmove-left nil)
;;   ("<right>" windmove-right nil)
;;   ("<up>" windmove-up nil)
;;   ("<down>" windmove-down nil)
;;   ("S-<left>" hydra-move-splitter-left nil)
;;   ("S-<right>" hydra-move-splitter-right  nil)
;;   ("S-<up>" hydra-move-splitter-up nil)
;;   ("S-<down>" hydra-move-splitter-down nil)
;;   ("M-<left>" buf-move-left nil)
;;   ("M-<right>" buf-move-right nil)
;;   ("M-<up>" buf-move-up nil)
;;   ("M-<down>" buf-move-down nil)
;;   ("p" previous-buffer "prev-buf")
;;   ("n" next-buffer "next-buf")
;;   ("1" delete-other-windows "1")
;;   ("d" delete-window "del")
;;   ("k" kill-buffer "kill")
;;   ("s" save-buffer "save")
;;   ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
;;   ("r" winner-redo "redo")
;;   ("b" helm-mini "helm-mini" :exit t)
;;   ("f" helm-find-files "helm-find" :exit t)
;;   ("|" (lambda () (interactive) (split-window-right) (windmove-right)))
;;   ("_" (lambda () (interactive) (split-window-below) (windmove-down)))
;;   ("q" nil "cancel")
;;   )

;; (global-set-key (kbd "M-#") 'hydra-windows/body)

(defun rk-transpose-sexps ()
  "If point is after certain chars transpose chunks around that.
Otherwise transpose sexps."
  (interactive "*")
  (if (not (looking-back "[,]\\s-*" (point-at-bol)))
      (progn (transpose-sexps 1) (forward-sexp -1))
    (let ((beg (point)) end rhs lhs)
      (while (and (not (eobp))
                  (not (looking-at "\\s-*\\([,]\\|\\s)\\)")))
        (forward-sexp 1))
      (setq rhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (re-search-backward "[,]\\s-*" nil t)
      (setq beg (point))
      (while (and (not (bobp))
                  (not (looking-back "\\([,]\\|\\s(\\)\\s-*" (point-at-bol))))
        (forward-sexp -1))
      (setq lhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (insert rhs)
      (re-search-forward "[,]\\s-*" nil t)
      (save-excursion
        (insert lhs)))))

(defun rk-ag-clean ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-matching-lines "docs/")
    (delete-matching-lines "/tests/")
    (delete-matching-lines "tests\.py")))

(defun rk-open-file-in-clipboard ()
  (interactive)
  (find-file (current-kill 0 t))
)

(defun rk-open-file-in-primary ()
  (interactive)
  (find-file (gui-get-primary-selection))
)

(defun rk-nl-replace ()
  (interactive)
  (beginning-of-buffer)
  (while (search-forward "\\n" nil t)
    (replace-match "\n"))
)

(defun rk-airbrake-error ()
  (interactive)
  (beginning-of-buffer)
  (while (search-forward "\\n" nil t)
    (replace-match "\n"))

  (beginning-of-buffer)
  (while (search-forward "\\\"" nil t)
    (replace-match "\""))

  (beginning-of-buffer)
  (while (search-forward "/venv/lib/" nil t)
    (replace-match "/home/r/.bender_venvs/sag/lib/"))

  (beginning-of-buffer)
  (while (search-forward "/code/project/" nil t)
    (replace-match "/home/r/ws/sag/"))
  
  (compilation-shell-minor-mode t)
  (end-of-buffer)
  (search-backward "/home/r/ws/sag")
  (require 'pulse)
  (pulse-momentary-highlight-one-line (point))
)

(defun rk-new-airbrake-error ()
  (interactive)
  (find-file "*airbrake-error*")
  (erase-buffer)
  (insert (gui-get-primary-selection))
  (rk-airbrake-error)
)

(defun hyp()
  (interactive)
  (cd "~/ws/django-hypergen/examples/")
  (pyvenv-activate "/home/r/.bender_venvs/hypergen")
  ;; (setq-default lsp-pyright-extra-paths [])
)

(defun sag()
  (interactive)
  (cd "~/ws/sag/src/")
  (pyvenv-activate "/home/r/.bender_venvs/sag")
  ;; (setq lsp-pyright-extra-paths ["/home/r/ws/sag/src/vendor"])
  ;; (setq-default lsp-pyright-extra-paths ["/home/r/ws/sag/src/vendor"])
  ;(lsp-workspace-restart "/home/r/ws/sag/src")
)

(defun sag2()
  (interactive)
  (cd "~/ws/sag2/src/")
  (pyvenv-activate "/home/r/.bender_venvs/sag2")
  ;; (setq lsp-pyright-extra-paths ["/home/r/ws/sag2/src/vendor"])
  ;; (setq-default lsp-pyright-extra-paths ["/home/r/ws/sag2/src/vendor"])
  ;; (lsp-workspace-restart "/home/r/ws/sag2/src")
)

(defun ai()
  (interactive)
  (cd "~/ws/ai/src/cloudflare/ai-pages/")
  ;; (pyvenv-activate "/home/r/.bender_venvs/sag")
  ;; (setq lsp-pyright-extra-paths ["/home/r/ws/sag/src/vendor"])
  ;; (setq-default lsp-pyright-extra-paths ["/home/r/ws/sag/src/vendor"])
  ;(lsp-workspace-restart "/home/r/ws/sag/src")
)

(defun rk-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

Prefixed with \\[universal-argument], expand the file name to
its fully canocalized path.  See `expand-file-name'.

Prefixed with \\[negative-argument], use relative path to file
name from current directory, `default-directory'.  See
`file-relative-name'.

The default with no prefix is to insert the file name exactly as
it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(global-set-key "\C-c\C-i" 'rk-insert-file-name)

(defun dot-auto-preview ()
  "Sync org file to Raspberry Pi with external script."
  (when (eq major-mode 'graphviz-dot-mode)
    (graphviz-dot-preview)))

(add-hook 'after-save-hook #'dot-auto-preview)

(defun rk-sql-indent-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))

(provide 'defuns)
