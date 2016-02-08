
;;; Code:
(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)
(require 'ido)

(require 'mu4e-message)
(require 'mu4e-meta)

(defun mu4e-action-stg-import (msg)
  "Stg import this patch message."
  (let ((path (ido-read-directory-name "Target directory: "
                                       (car ido-work-directory-list)
                                       "~/" t)))
    (setf ido-work-directory-list
          (cons path (delete path ido-work-directory-list)))
    (shell-command
      (format "cd %s; stg import -m %s"
	path
	(mu4e-message-field msg :path)))))

(provide 'mu4e-my-actions)
