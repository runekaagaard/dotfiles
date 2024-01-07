;;; chatgpt-shell ;;;
(defun rk-chatgpt-shell ()
  "Switch to a buffer starting with '*chatgpt*', or run 'chatgpt-shell' command if none found."
  (interactive)
  (let* ((chatgpt-buffer (catch 'found
                            (dolist (buf (buffer-list))
                              (when (string-prefix-p "*chatgpt*" (buffer-name buf))
                                (throw 'found buf))))))
    (if chatgpt-buffer
        (if (eq (current-buffer) chatgpt-buffer)
            ;; If the chatgpt buffer is the current buffer, switch to the previous one
            (switch-to-buffer (other-buffer chatgpt-buffer t))
          (let ((chatgpt-window (get-buffer-window chatgpt-buffer t)))
            (if chatgpt-window
                ;; If the chatgpt buffer is visible, switch to its window
                (select-window chatgpt-window)
              ;; If the chatgpt buffer exists but isn't visible, switch to it
              (switch-to-buffer chatgpt-buffer))))
      ;; If no chatgpt buffer is found, run chatgpt-shell
      (chatgpt-shell))))

(defun rk-shell-maker-welcome-message (config) "")

;;; gtpel ;;
(require 'gptel)

(defun rk-insert-indented (s)
  (let ((indentation (buffer-substring-no-properties
                      (line-beginning-position) (point))))
    (insert (replace-regexp-in-string
             "\n" (concat "\n" indentation) s nil t))))

(defun rk-language ()
  (cond ((eq major-mode 'web-mode)
         (let ((ext (file-name-extension (buffer-file-name))))
           (if (equal ext "js")
               "javascript"
             ext)))
        (t (string-remove-suffix "-mode" (symbol-name major-mode)))))

(defun rk-gptel-rewrite-and-replace (bounds &optional directive)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
         (read-string "Instructions: " "")))
  (gptel-request
   (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
   :system (concat "You are a " (rk-language)
                   " programmer. You inputs will be code and you will modify the code and output the "
                   "modifications. Only output code, nothing else. Don't introduce new content unless asked."
                   "Modify the code like so: "
                   directive
                   )
   :buffer (current-buffer)
   :context (cons (set-marker (make-marker) (car bounds))
                  (set-marker (make-marker) (cdr bounds)))
   :callback
   (lambda (response info)
     (if (not response)
         (message "ChatGPT response failed with: %s" (plist-get info :status))
       (let* ((bounds (plist-get info :context))
              (beg (car bounds))
              (end (cdr bounds))
              (buf (plist-get info :buffer)))
         (with-current-buffer buf
           (save-excursion
             (goto-char beg)
             (kill-region beg end)
             (rk-insert-indented (replace-regexp-in-string "\n*``.*\n*" "" response))
             ;; (set-marker beg nil)
             ;; (set-marker end nil)
             (message "Rewrote content. Original content saved to kill-ring."))))))))

(provide 'ai)
