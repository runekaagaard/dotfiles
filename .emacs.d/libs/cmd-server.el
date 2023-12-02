(defvar cmd-server-port 10000
    "port of the cmd server")
  
(defvar cmd-server-clients '() 
    "alist where KEY is a client process and VALUE is the string")


(defun cmd-server-start nil
    "starts an emacs cmd server"
    (interactive)
    (unless (process-status "cmd-server")
      (make-network-process :name "cmd-server" :buffer "*cmd-server*" :family 'ipv4 :service cmd-server-port :sentinel 'cmd-server-sentinel :filter 'cmd-server-filter :server 't) 
      (setq cmd-server-clients '())
      )
    )
  
(defun cmd-server-stop nil
  "stop an emacs cmd server"
  (interactive)
  (while  cmd-server-clients
    (delete-process (car (car cmd-server-clients)))
    (setq cmd-server-clients (cdr cmd-server-clients)))
  (delete-process "cmd-server")
  )

(defun cmd-server-filter (proc string)   
  (let ((pending (assoc proc cmd-server-clients))
      message
      index)
    ;;create entry if required
    (unless pending
      (setq cmd-server-clients (cons (cons proc "") cmd-server-clients))
      (setq pending  (assoc proc cmd-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (process-send-string proc (substring message 0 index))
      (cmd-server-log  (substring message 0 index) proc)
      (eval-string (substring message 0 index))
      (setq message (substring message index)))
    (setcdr pending message))
  )

(defun cmd-server-sentinel (proc msg)
 (when (string= msg "connection broken by remote peer\n")
  (setq cmd-server-clients (assq-delete-all proc cmd-server-clients))
  (cmd-server-log (format "client %s has quit" proc))))

;;from server.el
(defun cmd-server-log (string &optional client)
    "If a *cmd-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*cmd-server*")
      (with-current-buffer "*cmd-server*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s:" client) " ")
                string)
        (or (bolp) (newline)))))

(defun eval-string (str)
  "Read and evaluate all forms in str.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case err
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret)))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))

(provide 'cmd-server)
