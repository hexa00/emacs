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

(provide 'custom-lib)
