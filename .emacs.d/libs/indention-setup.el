(defun my-setup-indent (n)
  (setq c-basic-offset n)
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  (setq yaml-indent-offset n)
)

(defun basic-setup ()
  (local-set-key (kbd "<tab>") 'missing-indent)
  (local-set-key (kbd "<backtab>") 'missing-dedent)
  ; (rk-lsp-bridge-init) ; ordering issues with lsp-bridge and binding RET with acm popup.
  (local-set-key (kbd "RET") 'rk-newline-and-indent-pairs)
  (smartparens-mode 1)
)

(defun indent-4 ()
  (interactive)
  (set (make-local-variable 'tab-stop-list) (number-sequence 4 200 4))
  (my-setup-indent 4)
  (basic-setup)
)

(defun indent-2 ()
  (interactive)
  (set (make-local-variable 'tab-stop-list) (number-sequence 2 200 2))
  (my-setup-indent 2)
  (basic-setup)
)

(defun rk-indention-settings ()
  (cond
   ((or (eq major-mode 'web-mode)
        (eq major-mode 'lisp-mode)
        (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'yaml-mode))
    (indent-2))  ; Apply indent-2 for these specific modes
   ((or (derived-mode-p 'text-mode)
        (derived-mode-p 'prog-mode))
    (indent-4))))  ; Apply indent-4 for all other text and prog modes

(add-hook 'after-change-major-mode-hook 'rk-indention-settings)

(defun rk-newline-and-indent-pairs ()
  "Insert a new line and indent, then add closing pair on its own line and indent."
  (interactive)
  (newline-and-indent)
  (when (looking-at "[\])}'`]")
    (sp-remove-active-pair-overlay) ; smartparens adds a "region" around inserted pairs to allow chaining.
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(provide 'indention-setup)
