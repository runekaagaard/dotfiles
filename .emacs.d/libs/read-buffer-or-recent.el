;;; https://tsdh.org/posts/2023-03-21-a-read-buffer-function-also-suggesting-recent-files.html

(defconst th/read-buffer-or-recentf-command-alist
  '((kill-buffer buffers)
    (switch-to-buffer buffers-except recentf)
    (pop-to-buffer buffers-except recentf))
  "Alist with entries of the form (CMD . COMPLETES).
COMPLETES is a list defining what's completed where entries can
be:

- `buffers':        completion for all buffers
- `buffers-except': completion for all buffers except the current one
- `recentf':        completion for recent files which will be found on demand")

(defun th/read-buffer-or-recentf (prompt &optional
                                         def require-match predicate)
  (let* ((tables (or
                  (mapcar
                   (lambda (syms)
                     (pcase syms
                       ('buffers #'internal-complete-buffer)
                       ('buffers-except (internal-complete-buffer-except
                                         (current-buffer)))
                       ('recentf (completion-table-dynamic
                                  (lambda (s) recentf-list)))
                       (unknown  (error "Unknown case %S" unknown))))
                   (cdr (assoc this-command
                               th/read-buffer-or-recentf-command-alist)))
                  (list #'internal-complete-buffer
                        (completion-table-dynamic
                         (lambda (s) recentf-list)))))
         (completion-table (apply #'completion-table-in-turn tables)))
    ;; `read-buffer-to-switch' (called by `switch-to-buffer') already sets
    ;; `internal-complete-buffer' as `minibuffer-completion-table' using
    ;; `minibuffer-with-setup-hook' before `read-buffer-function' is invoked by
    ;; `read-buffer', so we'd be restricted to buffers by default.  Therefore,
    ;; append a function setting our completion table.
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (setq-local minibuffer-completion-table completion-table)))
      (when-let ((result (completing-read prompt completion-table
                                          predicate require-match nil
                                          'buffer-name-history def)))
        (cond
         ((get-buffer result) result)
         ((file-exists-p result) (buffer-name (find-file-noselect result)))
         (t result))))))

(setq read-buffer-function #'th/read-buffer-or-recentf)
(provide 'read-buffer-or-recent)
