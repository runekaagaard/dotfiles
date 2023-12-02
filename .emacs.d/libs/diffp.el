(make-variable-buffer-local
 (defvar diffp-project-root-dir nil))

(make-variable-buffer-local
 (defvar diffp-diff-command nil))

(defun diffp--init ()
  (setq diffp-project-root-dir (read-directory-name "Diff root folder: "))
  (setq diffp-diff-command (read-shell-command "Diff command: "))
  (diffp-refresh)
)

(defun diffp-refresh ()
  (interactive)
  (let (
    (x (line-number-at-pos))
    (rom (bound-and-true-p read-only-mode))
    (a (make-temp-file "diffp-a"))
    (b (make-temp-file "diffp-a"))
  )
    (cd diffp-project-root-dir)
    (read-only-mode 0)
    (write-region (point-min) (point-max) a t)
    (shell-command (format "%s >> %s" diffp-diff-command b))
    (setf (buffer-string) "")
    (insert (shell-command-to-string (format "diffp.py %s %s" a b)))
    (goto-line x)
    (read-only-mode rom)
    (delete-file a)
    (delete-file b)
  )
)

;;;###autoload
(define-minor-mode diffp-mode
  "Refresh a diff"
  :lighter " diffp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-g") 'diffp-refresh)
            map)
  (if diffp-mode (diffp--init))
)

(add-hook 'diff-mode-hook (lambda ()
  (setq comment-start "# ")
))

(provide 'diffp)
