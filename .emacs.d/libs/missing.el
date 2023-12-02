(provide 'missing)

(defun missing--tab-to-prev-tab-stop ()
  "Deletes until previous defined tab-stop column."
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (let ((prevtab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to prevtab)))


(defun missing--area-beginning ()
  (save-excursion
    (if (use-region-p)
      (goto-char (region-beginning))
    )
    (line-beginning-position)
  )
)

(defun missing--area-end ()
  (save-excursion
    (if (use-region-p)
      (goto-char (region-end))
    )
    (line-end-position)
  )
)

(defun missing--select-area (&optional line-below)
  "If the first or last line are not fully selected, select them completely."
  (if (use-region-p)
    (progn
      (let ((beginning (missing--area-beginning))
            (end (missing--area-end)))
        (push-mark beginning nil t)
        (goto-char end)
      )
    )
    (progn
      (end-of-line)
      (push-mark (line-beginning-position))
    )
  )
  (if line-below
    (if (eobp) (newline) (forward-char))
  )
)

(defun missing-move-up ()
  (interactive)
  (setq uses-region (use-region-p))
  (setq column (current-column))
  (missing--select-area t)
  (setq content (delete-and-extract-region (region-beginning) (region-end)))
  (forward-line -1)
  (let (deactivate-mark)
    (set-mark (point))
    (insert content)
    (left-char)
  )
  (if uses-region
    (setq deactivate-mark nil)
    (progn
      (deactivate-mark)
      (move-to-column column)
    )
  )
)

(defun missing-move-down ()
  (interactive)
  (setq uses-region (use-region-p))
  (setq column (current-column))
  (missing--select-area t)
  (setq content (delete-and-extract-region (region-beginning) (region-end)))
  (forward-line 1)
  (let (deactivate-mark)
    (set-mark (point))
    (insert content)
    (left-char)
  )
  (if uses-region
    (setq deactivate-mark nil)
    (progn
      (deactivate-mark)
      (move-to-column column)
    )
  )
)


(defun missing-indent ()
  (interactive)
  (if (use-region-p)
    (progn
      (missing--select-area)
      (indent-rigidly-right-to-tab-stop (region-beginning) (region-end))
      (setq deactivate-mark nil)
    )
    (progn
      (back-to-indentation)
      (tab-to-tab-stop)
      (move-end-of-line nil)
    )
  )
)

(defun missing-dedent ()
  (interactive)
  (if (use-region-p)
    (progn
      (missing--select-area)
      (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
      (setq deactivate-mark nil)
    )
    (progn
      (back-to-indentation)
      (missing--tab-to-prev-tab-stop)
      (move-end-of-line nil)
    )
  )
)
