;;; Customizations

;;; Basic stuff
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; Set directory for ~ backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Important to get a black background in emacsclient -t
;; http://denilab.blogspot.ca/2012/10/emacs-live-tmux-yes-we-can.html
;; too bad can't be done auto
(defun quirk-black-background ()
(interactive)
(set-face-background 'default "color-16"))

;; Transparently open compressed files
(auto-compression-mode t)

;;; keys

; Goto line number
(global-set-key '[f12] 'goto-line)

;; Hide welcome screen
(setq inhibit-splash-screen t)

;; Column mode always on
(setq column-number-mode t)

;; Disable toolbar
(tool-bar-mode -1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;;; y-n to yes /no
(defalias 'yes-or-no-p 'y-or-n-p)

; set default font
(set-default-font "DejaVu Sans Mono-10")

;;; Replace this with the proper var ?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-auto-activate t)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-compile-window-height 6)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ecb-default-highlight-face ((((class color) (background dark)) (:background "darkgreen"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "RoyalBlue"))))
 '(header-line ((t (:background "black" :foreground "white" :box nil))))
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "white")))))


(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Reload file on change
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(global-set-key (kbd "<f5>") 'revert-buffer)
(setq ff-search-directories'("." "../src" "../include" "../inc"))

;; show colums
(setq column-number-mode t)

;; Wrap at 80 col
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 74)

;; Mini buffer easy file navigation etc
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;changelog info
(setq add-log-full-name "Antoine Tremblay")
(setq add-log-mailing-address "antoine.tremblay@ericsson.com")

(setq mu4e-mu-binary "/usr/local/mu-git/bin/mu")
(setq mu4e-get-mail-command "~/scripts/update-mail"
      mu4e-update-interval 60)
(setq
 user-mail-address "antoine.tremblay@ericsson.com"
 user-full-name  "Antoine Tremblay")

;; Skip duplicate mail (inbox mess)
(setq mu4e-headers-skip-duplicates t)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use fancy chars
(setq mu4e-use-fancy-chars t)

;; default folders
(setq
  mu4e-maildir       "~/Maildir"   ;; top-level Maildir
  mu4e-sent-folder   "/Sent Items"       ;; folder for sent messages
  mu4e-drafts-folder "/Drafts"     ;; unfinished messages
  mu4e-trash-folder  "/Trash"      ;; trashed messages
  mu4e-refile-folder "/Archive")   ;; saved messages

(add-to-list 'mu4e-bookmarks '("maildir:/INBOX NOT flag:trashed NOT to:elx-users@mailman.lmera.ericsson.se NOT cc:elx-users@mailman.lmera.ericsson.se NOT to:gdb-patches@sourceware.org NOT cc:gdb-patches@sourceware.org NOT to:gdb@sourceware.org NOT cc:gdb@sourceware.org" "Inbox" ?i))
(add-to-list 'mu4e-bookmarks '("maildir:/gdb-patches" "gdb patches" ?p))
(add-to-list 'mu4e-bookmarks '("maildir:/gdb-patches to:antoine.tremblay@ericsson.com OR cc:antoine.tremblay" "my threads" ?t))
(add-to-list 'mu4e-view-actions
	     '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-to-list 'mu4e-view-actions
	     '("istg import" . mu4e-action-stg-import) t)

;; thread folding branch from zakkak
(define-key 'mu4e-headers-mode-map (kbd "TAB") 'mu4e-headers-toggle-thread-folding)

;; shr2text work well but is very slow
;(setq mu4e-html2text-command 'mu4e-shr2text)
;;(setq shr-color-visible-luminance-min 80)
;; Doesn't preserve link etc but is fast , use a V to see in browser
(setq mu4e-html2text-command "w3m -T text/html")


(provide 'custom-defs)
