;; Variables customized via customize-group and customize-variable UI.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; General settings on vanilla emacs.
(setq
  inhibit-startup-message t       ; Disables the startup splash screen.
  inhibit-startup-echo-area-message t ; Suppresses startup messages in the echo area.
  case-replace t                  ; Respects case in search-and-replace operations.
  save-interprogram-paste-before-kill t ; Saves clipboard text before killing.
  yank-pop-change-selection t     ; Allows yank-pop to change the X11 clipboard.
  mouse-yank-at-point t           ; Middle click pastes at point, not at click.
  ring-bell-function 'ignore      ; Disables the audible bell.
  byte-compile-warnings '(not obsolete) ; Ignores warnings about obsolete features.
  window-safe-min-height 0        ; Allows very small window heights.
  window-min-height 0             ; Permits zero line window height.
  column-number-mode t            ; column numbers in mode line
  ; disable noob prompts
  confirm-kill-processes nil      ; No confirmation for killing processes on exit.
  use-short-answers t             ; Enables 'y'/'n' instead of 'yes'/'no'.
  confirm-nonexistent-file-or-buffer nil ; No prompts for nonexistent files/buffers.
  ; No kill buffer confirmation for running processes.
  kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
  revert-without-query '(".*")    ; Auto-reverts files without asking.
  confirm-kill-emacs nil          ; Don't nag about unsaved files
  vc-follow-symlinks t            ; just follow the god dam symlink, man
  compilation-ask-about-save nil
  ; Very big undo limits. History can be precious.
  undo-limit 10000000
  undo-strong-limit 12000000
  undo-outer-limit 150000000
  ; ediff better defaults
  ediff-split-window-function 'split-window-horizontally
)

; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start in fullscreen
; Quit means quit.
; TODO: This crashes killing emacs. Why?
;; (defadvice save-buffers-kill-emacs (around no-y-or-n activate)
;;   (flet ((yes-or-no-p (&rest args) t)
;;          (y-or-n-p (&rest args) t))
;;     ad-do-it))

; Global modes on/off
(scroll-bar-mode -1)              ; Disable the scroll bar
(tool-bar-mode -1)                ; Disable the tool bar
(menu-bar-mode -1)                ; Disable the menu bar
(winner-mode 1)                   ; Enable winner mode to undo/redo window configuration changes
(delete-selection-mode 1)         ; Enable typing over selected text to replace it
(global-auto-revert-mode t)       ; Automatically reload files when they change on disk
(electric-pair-mode -1)           ; Automatic parens pairing
(electric-indent-mode -1)         ; Automatic parens pairing
(desktop-save-mode 1)             ; Remember open buffers and layout after restart

; Advice some functions to center the line.
;; Goto line recenter
(defadvice goto-line (after rk activate)
  (recenter-top-bottom)
)

;; Environment
(setq temporary-file-directory "/tmp/")
(add-to-list 'load-path "~/.emacs.d/libs/")
(cd "/home/r/ws/sag")

; Encoding
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Meta files.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Requires
(require 'bind-key)
(require 'defuns) ; My custom functions
(require 'read-buffer-or-recent) ; Adds recent history to c-o
(require 'indention-setup) ; Custom indention code
(require 'secrets) ; Custom indention code

;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq 
  straight-use-package-by-default t
  use-package-always-ensure t
)

;; auth
(setq auth-sources '("~/.authinfo"))

;; Various packages p1
(use-package expand-region)
(use-package iedit)
(use-package ace-jump-mode)
(use-package diminish)
(use-package multiple-cursors)
(use-package cython-mode)
(use-package lua-mode)
(use-package pdf-tools)
(use-package nim-mode)
(use-package crux)
(use-package try)
(use-package vterm :init (setq vterm-use-vterm-prompt nil))
(use-package whole-line-or-region :config (whole-line-or-region-global-mode t))
(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))

; magit
(use-package magit
  :config
  ;; (custom-set-faces
  ;;  '(magit-diff-added-highlight ((t (:background "green" :foreground "black" :extend t))))
  ;;  '(magit-diff-removed-highlight ((t (:background "red" :foreground "white" :extend t))))
  ;;  '(magit-diff-hunk-heading-highlight ((t (:inherit default :extend t)))))
  
  )

;(magit-jump-to-staged)

(defun rk-magit-stage-commit-push ()
  (interactive)
  (magit-refresh)
  (let ((magit-no-confirm '(stage-all-changes)))
    (magit-stage-modified t)  ; Stage all unstaged changes without confirmation
    (magit-stage-untracked t) ; Stage all untracked files
  )
  (magit-refresh)
  (magit-diff-staged)
  (let ((commit-message (read-string "Commit message: ")))
    (magit-call-git "commit" "-m" commit-message) ; Commit with message
    (magit-push-current-to-upstream nil)) ; Push to upstream
  (magit-refresh)
  (let ((buffers (buffer-list)))
    (dolist (buffer buffers)
      (when (string-prefix-p "magit-diff:" (buffer-name buffer))
        (kill-buffer buffer)))))

(defun rk-global-magit-stage-commit-push ()
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (magit-status)
    (rk-magit-stage-commit-push)))

(define-key magit-mode-map (kbd "s-x") 'rk-magit-stage-commit-push)
(global-set-key (kbd "s-x") 'rk-global-magit-stage-commit-push)

(defun magit-diff-range-master ()
  (interactive)
  (magit-diff-range "master...HEAD"))

(transient-append-suffix 'magit-diff "d"
  '("m" "Diff against master" magit-diff-range-master))

(defun my-magit-diff-upstream... (&optional args files)
    (interactive (magit-diff-arguments))
    (let ((merge-base (magit-git-string "merge-base" "@{u}" "HEAD")))
      (if merge-base
          (magit-diff-working-tree (magit-git-string "merge-base" "@{u}" "HEAD") args files)
        (let ((upstream (magit-get-upstream-ref)))
          (cond
           ((null upstream) (error "Cannot determine merge base: upstream is not set"))
           (t (error "Cannot determine merge base")))))))

  (transient-append-suffix 'magit-diff "r" '("U" "Diff upstream...worktree" my-magit-diff-upstream...))

(use-package magit-delta
  ;:hook (magit-mode . magit-delta-mode)
)
(use-package forge :after magit)
;; chatgpt, etc.
; see secrets.el
(straight-use-package 'gptel)
(require 'ai) ; Chatgpt, etc.

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

(setq chatgpt-shell-system-prompt 2) ; Programming

; todo: broken.
;; (use-package dall-e-shell
;;   :requires shell-maker
;;   :straight (:host github :repo "xenodium/chatgpt-shell" :files ("dall-e-shell.el")))

;; devdocs
(use-package devdocs)
(custom-set-faces
 '(shr-text ((t :height 1.0)))
 '(shr-h1 ((t :height 1.0 :weight bold :underline t)))
 '(shr-h2 ((t :height 1.0 :weight bold :underline t)))
 '(shr-h3 ((t :height 1.0 :weight bold)))
 '(shr-h4 ((t :height 1.0 :weight bold)))
 '(shr-h5 ((t :height 1.0 :weight bold)))
 '(devdocs-code-block ((t :background "#2e302e" :extend t)))
)

;; which-key
(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (setq which-key-side-window-max-width 0.4)
  ; (which-key-mode 1)
  
  )

;;; Looks ;;;
;; Monokai
(use-package monokai-theme
  :config
  (load-theme 'monokai t)
)
(setq default-font-size-pt 12)
(set-face-attribute 'default nil :height 120)

(use-package auto-dim-other-buffers)
;;; END looks ;;;

;;; Searching ;;;
;; ag
(use-package ag
  :init (setq
     ag-group-matches nil
     ag-reuse-buffers t
  )
)

;; Ag wgrep
(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
)
;;; END searching ;;;

;;; BEGIN completion ;;;
(fido-mode 1) ; the vanilla successor of ido-mode, flex, smex, ido-everywhere+, etc. etc

;; find-file-in-project
; https://github.com/redguardtoo/find-file-in-project
; sudo apt install fd-find
; sudo ln -s /usr/bin/fdfind /usr/bin/f
(use-package find-file-in-project
  :init
  (setq
    ffip-use-rust-fd t
  )
)

; counsel-ag breaks fido-mode, but helm works.
(use-package helm-ag
 :init
  (custom-set-variables
    '(helm-ag-base-command "rg --no-heading --line-number --color never")
    `(helm-ag-success-exit-status '(0 2)))
)
;;; END completion ;;;

;;; python ;;;
;; python settings
(setq
  python-shell-interpreter "/home/r/.bender_venvs/sag/bin/python"
)

;; virtualenv
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-activate "/home/r/.bender_venvs/sag"))
;;; END python

;; nim
(setq nim-compile-default-command
  '("r" "--verbosity:0" "--hint[Processing]:off" "--excessiveStackTrace:on"))

;; Hippie expand.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line))

;; Wrap region
(use-package wrap-region
  :config
  (wrap-region-mode t)
)

;; minibuffer.
; Kill minibuffer when leaving by mouse click.
(setq minibuffer-scroll-window nil)

;; dumb-jump
(use-package dumb-jump
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

;;; lsp-bridge and dependencies. ;;;
;; pip install epc orjson sexpdata==1.0.0 six setuptools paramiko rapidfuzz
(use-package yasnippet
  :init
  (setq yas-installed-snippets-dir "~/.emacs.d")
  :config
  (yas-global-mode 1))

(use-package markdown-mode
  :init
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (set-face-attribute 'markdown-header-face-1 nil :weight 'normal :height 1.0)
  (set-face-attribute 'markdown-header-face-2 nil :weight 'normal :height 1.0)
  (set-face-attribute 'markdown-header-face-3 nil :weight 'normal :height 1.0)
  (set-face-attribute 'markdown-header-face-4 nil :weight 'normal :height 1.0)
  (set-face-attribute 'markdown-header-face-5 nil :weight 'normal :height 1.0)
  (set-face-attribute 'markdown-header-face-6 nil :weight 'normal :height 1.0))

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :config
;;   (global-set-key (kbd "M-.") 'lsp-bridge-find-def)
;;   (global-set-key (kbd "s-.") 'lsp-bridge-find-def-other-window)
;;   (global-set-key (kbd "M-,") 'lsp-bridge-find-def-return)
;;   (setq 
;;     lsp-bridge-enable-search-words nil
;;     lsp-bridge-enable-hover-diagnostic t
;;     lsp-bridge-enable-mode-line nil
;;     ; lsp-bridge-complete-manually t
;;     acm-backend-search-file-words-max-number 0
;;     acm-backend-lsp-match-mode "prefixCaseSensitive"
;;     acm-enable-copilot t
;;   )
;;   :init
;;   (global-lsp-bridge-mode)
;; )

(add-to-list 'load-path "/home/r/ws/lsp-bridge")
(require 'lsp-bridge)
(defun lsp-bridge--mode-line-format () "")
(global-set-key (kbd "M-.") 'lsp-bridge-find-def)
(global-set-key (kbd "s-.") 'lsp-bridge-find-def-other-window)
(global-set-key (kbd "M-,") 'lsp-bridge-find-def-return)
(setq 
  lsp-bridge-enable-search-words nil
  lsp-bridge-enable-hover-diagnostic t
  ; lsp-bridge-enable-mode-line nil
  acm-backend-search-file-words-max-number 0
  acm-backend-lsp-match-mode "prefixCaseSensitive"
  
  acm-enable-copilot t
)
(global-lsp-bridge-mode)

;;; END lsp-bridge ;;;

;; web-mode ;;
(use-package web-mode
  :init
  (setq
    web-mode-engines-alist '(("django" . "\\.html\\'"))
    web-mode-enable-auto-indentation nil
  )
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  
)

;; format-all
(use-package format-all :config
  (add-hook 'python-mode-hook 'format-all-ensure-formatter)
  (add-hook 'python-mode-hook 'format-all-mode)
)

;;; save state ;;;
;; save-place
(save-place-mode 1)

;; Recentfiles.
(require 'recentf)
(recentf-mode 1)
(setq
  recentf-max-menu-items 1000
  recentf-max-saved-items 1000
  recentf-auto-cleanup 'never)

;; History.
(setq
 savehist-file "~/.emacs.d/savehist"
 history-length 1000
 savehist-additional-variables
  '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)
 savehist-autosave-interval 60)

(savehist-mode +1)

;; Bookmarks.
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.d/bookmarks")
;;; END save state ;;;

;; Dired
(require 'dired-x)
(setq-default dired-omit-files-p t)
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired )
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Undo
(use-package undo-fu)
(setq undo-fu-ignore-keyboard-quit t)

;; Indention
(require 'missing)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode -1)

;; Allow external programs to open files in emacs
(ignore-errors
  (require 'cmd-server)
;(ignore-errors
  (cmd-server-start))
;)

;;; org-mode ;;;
(setq org-support-shift-select 1)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation 0)
(setq ob-async-no-async-languages-alist '("ipython"))
(setq org-babel-min-lines-for-block-output 1)
(setq org-startup-indented nil)
(setq org-indent-indentation-per-level 0)
(setq org-hide-leading-stars nil)

(eval-after-load "org"
  '(progn
     ;; Establishing your own keybindings for org-mode.
     ;; Variable org-mode-map is available only after org.el or org.elc is loaded.
     ;(define-key org-mode-map (kbd "<M-right>") nil) ; erasing a keybinding.
     ;(define-key org-mode-map (kbd "<M-left>") nil) ; erasing a keybinding.

     (defun my-org-mode-hook ()
       ;; The following two lines of code is run from the mode hook.
       ;; These are for buffer-specific things.x1
       ;; In this setup, you want to enable flyspell-modes
       ;; and run org-reveal for every org buffer.
       ;(flyspell-mode 1)
       ;(org-reveal)
      (define-key org-mode-map (kbd "<s-tab>") 'org-cycle)
      (define-key org-mode-map (kbd "<s-iso-lefttab>") 'org-shifttab)
      (define-key org-mode-map (kbd "<return>") 'newline)
      (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
      (define-key org-mode-map (kbd "<S-iso-lefttab>") 'missing-dedent)
      ; TODO make work.
      ; (org-bullets-mode t)
      ; (org-indent-mode t)
     )
     (add-hook 'org-mode-hook 'my-org-mode-hook)))

;; Hack for https://github.com/gregsexton/ob-ipython/issues/135#issuecomment-397463174
(defun ob-ipython--collect-json ()
  ;; hacks here
  (when (re-search-forward "{" nil t)
    (backward-char))
  ;; hacks end
  (let ((json-array-type 'list))
    (let (acc)
      (while (not (= (point) (point-max)))
        (setq acc (cons (json-read) acc))
        (forward-line))
      (nreverse acc))))

(advice-add 'ob-ipython--collect-json :before
            (lambda (&rest args)
              (when (re-search-forward "{" nil t)
                (backward-char))))

;; org-reveal
(setq org-reveal-root "file:///home/r/ws/reveal.js")
;;; END org-mode ;;;

;; tramp-mode
(defun my-disable-modes-in-tramp ()
  (when (file-remote-p default-directory)
    (lsp-bridge-mode -1)                ; Disable lsp-mode
    (flycheck-mode -1)          ; Disable flycheck-mode
    ;; Add any other modes you want to disable below
    ))

(add-hook 'after-change-major-mode-hook 'my-disable-modes-in-tramp)

;; scratch
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
; Never kill scratch
(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
  (progn (bury-buffer)
	 nil)
t))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)

;; abbrev-mode
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs 'silently)

;; rst-mode
(require 'rst)
(modify-syntax-entry ?_ "_" rst-mode-syntax-table)
(add-hook 'rst-mode-hook #'visual-line-mode)

;; syntax table
(defun rk-great-syntax-table ()
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w"))

(add-hook 'after-change-major-mode-hook 'rk-great-syntax-table)

;; Keyboard commands
;; Keybindings.
; C=ctrl, M=alt, s=super, h=hyper

; Move to bind-keys* if possible.
(global-set-key [f2] 'revert-buffer)
(global-set-key [(shift f2)] 'revert-buffer)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "s-i") 'previous-buffer)
(global-set-key (kbd "s-o") 'next-buffer)
(global-set-key [(control x) (control r)] 'find-file-root)

(bind-keys*
  ; $ or ½
  ("½" . ace-jump-mode) 
  ; mode
  ; 2
  ; 3
  ; 4
  ; 5
  ; 6
  ; 7
  ; 8
  ("s-M-8" .  rk-open-scratch) 
  ; 9
  ("s-M-9" .  rk-kill-buffer-and-delete-window) 
  ; 0
  ("s-M-0" .  delete-window) 
  ; +
  ; ´
  ; Q
  ("s-q" . (lambda () (interactive) (rk-toggle-or-run-mode 'chatgpt-shell 'chatgpt-shell-mode)))
  ; W
  ("s-w" .  kill-this-buffer) 
  ; E
  ("s-e" .  find-file-in-project) 
  ("s-M-e" .  find-file-in-current-directory) 
  ; R
  ("s-r" . recompile) 
  ; T
  ("s-t" . magit-status)
  ("C-t" . rk-transpose-sexps)
  ; Y
  ; U
  ; I
  ("M-i" .  rk-back-window) 
  ; O
  ("M-o" .  other-window) 
  ("C-o" .  find-file) 
  ("s-M-o" .  recentf-open-files)
  ; P
  ; Å
  ; ¨
  ; A
  ("s-a" . (lambda () (interactive) (rk-toggle-or-run-mode 'vterm 'vterm-mode)))
  ; S
  ; D
  ; F
  ; ("s-s" .  rk-dwim-project-dir)
  ("s-s" .  ag-project)
  ("s-M-s" . ag)
  ; G
  ; H
  ("s-M-h" .  highlight-symbol-at-point)
  ("s-C-h" .  rk-unhighlight-all-in-buffer)
  ; J
  ("s-j" . rk-just-one-space)
  ; K
  ("s-k" .  rk-open-line-below)
  ("s-M-k" .  rk-open-line-above)
  ; L
  ("s-l" .  rk-duplicate-current-line-or-region)
  ; Æ
  ("s-å" .  display-line-numbers-mode)
  ; Ø
  ("M-ø" .  visual-line-mode)
  ("s-ø" .  window-swap-states)
  ; '
  ("C-'" .  dumb-jump-go)
  ("C-*" .  dumb-jump-go-other-window)
  ("M-'" .  dumb-jump-back)
  ("C-M-'" .  dumb-jump-go-prompt)
  ; >
  ; Z
  ("C-z" . (lambda ()
                       (interactive)
                       (if (eq major-mode 'vterm-mode)
                           (vterm-undo)
                         (undo-fu-only-undo))))
  ("C-S-z" .  undo-fu-only-redo)
  ("M-z" . zap-up-to-char)
  ("s-M-z" . rk-zap-up-to-char-reverse)
  ("s-z" . rk-gptel-rewrite-and-replace)
  ; X
  ; C
  ; ("s-c" . rk-open-file-in-clipboard)
  ; ("s-x" . rk-open-file-in-primary)
  ; V
  ; B
  ("s-b" .  switch-to-buffer)
  ("s-M-b" .  list-buffers)
  ; N
  ("s-n" . rk-narrow-or-widen-dwim)
  ; M
  ("s-m" . mc/edit-lines )
  ;("s-," . notmuch)
  ; .
  ("C-." . rk-imenu-clear-and-run)
  ; -
  ("s--" . er/expand-region)
  ; RET
  ("C-<return>" .  save-buffer)
  
  ; SPACE
  ("s-SPC" . helm-do-ag-project-root)
  ("s-M-SPC" . helm-do-ag-this-file)
  
  ; ARROWS
  ("M-<up>" . (lambda () (interactive) (if (derived-mode-p 'org-mode) (org-metaup) (call-interactively #'missing-move-up))))
  ("M-<down>" . (lambda () (interactive) (if (derived-mode-p 'org-mode) (org-metadown) (call-interactively #'missing-move-down))))
  ("M-<left>" . (lambda () (interactive) (if (derived-mode-p 'org-mode) (org-metaleft) (call-interactively #'rk--move-beginning-of-line))))
  ("M-<right>" . (lambda () (interactive) (if (derived-mode-p 'org-mode) (org-metaright) (call-interactively #'move-end-of-line))))
  ; TAB
  ;("C-<tab>" . rk-indent-according-to-mode);
  ; Mouse heresy
  ("<C-M-drag-mouse-1>" . rk-swap-window-buffers-by-dnd)
)

;; Don't conflict with rk-swap-window-buffers-by-dnd
(global-unset-key (kbd "C-M-<down-mouse-1>"))

;; vterm
(push (list "find-file-below"
            (lambda (path)
              (if-let* ((buf (find-file-noselect path))
                        (window (display-buffer-below-selected buf nil)))
                  (select-window window)
                (message "Failed to open file: %s" path))))
      vterm-eval-cmds)

(defun rk-vterm-copy-mode-toggle ()
  "Toggle vterm between normal and copy mode."
  (interactive)
  (if (not vterm-copy-mode)
      (vterm-copy-mode 1)
    (vterm-copy-mode -1)))

(define-key vterm-mode-map (kbd "s-c") 'rk-vterm-copy-mode-toggle)
(define-key vterm-copy-mode-map (kbd "s-c") 'rk-vterm-copy-mode-toggle)
(define-key vterm-mode-map (kbd "M-w") #'kill-ring-save)

; (define-key vterm-mode-map (kbd "C-S-z") 'vterm-redo)

;; Frame title
(setq frame-title-format "love")

;; Mode line
(require 'vc)
(defun vc-mode-line (file &optional backend) "")
(setq-default 
  mode-line-front-space nil
  mode-line-mule-info nil
  mode-line-client nil
  mode-line-remote nil
  mode-line-modified nil
  mode-line-buffer-identification
    (let ((file-name '(:eval (abbreviate-file-name (or buffer-file-name "%b")))))
      (list file-name))
)
(require 'diminish)
(eval-after-load "filladapt" '(diminish 'filladapt-mode))
(diminish 'elmacro-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'size-indication-mode)
(diminish 'superword-mode)
(diminish 'auto-dim-other-buffers-mode)
(diminish 'highlight-indentation-mode)
(diminish 'guide-key-mode)
(diminish 'hungry-delete-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'whole-line-or-region-local-mode)
(diminish 'whole-line-or-region-global-mode)
(diminish 'wrap-region-mode)
(diminish 'yas-minor-mode)
(diminish 'company-mode)
(diminish 'eldoc-mode)
(diminish 'python-mode)
(diminish 'abbrev-mode)
(diminish 'format-all-mode)
;(diminish 'lsp-bridge-mode)
(diminish 'smartparens-mode)
(diminish 'which-key-mode)

; the format
(defun my-mode-line-format (modes)
  "Return a string representation of `modes` post replacements."
  (let ((replacements '(("LSPB" . "l")
                        ("\\[" . "")
                        ("\\]" . "")
                        ("(" . "")
                        (")" . ""))))
    (rk-replace-multiple replacements (format-mode-line modes))))

(setq-default mode-line-format
  '("%e" mode-line-front-space
    mode-line-frame-identification mode-line-buffer-identification 
    (vc-mode vc-mode)
    "  " (:eval (my-mode-line-format mode-line-modes))
    mode-line-misc-info mode-line-end-spaces
    (:eval (propertize " " 'display `((space :align-to (- right ,(length (format-mode-line "%l,%c  ")))))))
    "%l,%c"
   )
)
