;; This file contains a set of functions for my own custom use.

(defun gdb-test ()
  "Run gdb with the pid of the gdb currectly executing a test"
  (interactive)
  (gdb (concat gud-gdb-command-name (format " -p %s" (system-process-pid "gdb.*-nw.*-nx")))))
    
(defun system-process-pid (regex-str)
  "Return the pid for a process with args matching regex-str"
  (dolist (pid (list-system-processes))
    (let ((attrs (process-attributes pid)))
      (if (string-match-p regex-str
                          (cdr (assoc 'args attrs)))
          (return pid)))))

;; See https://groups.google.com/forum/#!topic/mu-discuss/JqHEGycEyKI
;; Emacs compiled with xwidgets is needed.
;; apt-get install libwebkitgtk-3.0-dev
;; ../configure .. --with-xwidgets
(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let* ((html (mu4e-message-field msg :body-html))
          (txt (mu4e-message-field msg :body-txt))
          (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      ;; simplistic -- but note that it's only an example...
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

(add-to-list 'mu4e-view-actions
  '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)

(provide 'custom-lib)
