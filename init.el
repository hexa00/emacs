;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq use-package-verbose t)

;; Load path etc.
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/use-package")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Init the package load path etc
(package-initialize)

(require 'use-package)

;; Always download packages if not present
(setq use-package-always-ensure t)

;; Save current place in file
(use-package saveplace
  :defer t
  :init
  (setq save-place t)
  (setq save-place-file "~/.emacs.d/saved-places")
  )

;; Enable URLs in open file
(use-package ffap
  :defer t)

;; Better unique names rather then file<1> file<2>
;;; FIXME not working ?
;;(use-package uniquify)

;; Support shell color codes
(use-package ansi-color
  :defer t)

(use-package diff-mode
             :mode "COMMIT_EDITMSG$"
             :config
             (set-face-foreground 'diff-added "green4")
             (set-face-background 'diff-added "black")
             (set-face-foreground 'diff-removed "red3")
             (set-face-background 'diff-removed "black")
             )

(use-package cc-mode
  :defer t
  :bind  ("C-c o" . ff-find-other-file)
  :config
  (add-hook 'c-mode-common-hook 'helm-gtags-mode)
  ;; For gdb style based on gnu ident case in switch statements
  ;; may not be really needed
  ;;(c-set-offset 'case-label '+)
  )

(use-package semantic
  :defer t
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  )

(use-package ecb
  :defer t
  :init
  (semantic-mode 1)
  (setq stack-trace-on-error t)
  )

(use-package compile-dir
  :ensure f
  :bind ("<f7>" . gdb-compile)
  )

(use-package bm
  :bind (("<f8>" . bm-toggle)
	 ("<f9>" . bm-previous)
	 ("<f10>" .  bm-next))
  )

;;Save desktop mode auto save on idle
(use-package desktop
  :defer t
  :init
  (desktop-save-mode 1)
  :config
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save))


(use-package helm-config
  :ensure f
  :defer t)

(use-package helm-gtags
  :defer t
  :init
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t)
  :config
  (bind-key "C-c g a" 'helm-gtags-tags-in-this-function helm-gtags-mode-map)
  (bind-key "C-j" 'helm-gtags-select helm-gtags-mode-map)
  (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
  (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
  (bind-key "C-c <" 'helm-gtags-previous-history helm-gtags-mode-map)
  (bind-key "C-c >" 'helm-gtags-next-history helm-gtags-mode-map)
)

(use-package helm-ack
  :defer t
  :init
  (setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t
      helm-ack-use-ack-grep     t
      helm-ack-version 1.92)
  :bind (("C-x c C-o" . helm-occur)
	 ("C-x c C-a" . helm-ack))
  :config
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	  helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f")

)
)

;; Set colors for company below
(use-package color)

;; Auto Completion
;; Fixme TAB not working properly
(use-package company
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (defvar company-key-map (make-sparse-keymap))
  :config
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common)
      (indent-according-to-mode)))
;;  (bind-key "<tab>" 'complete-or-indent company-key-map)
)

(use-package whitespace
  :defer t
  :init
  (setq-default whitespace-style '(face trailing lines empty))
  (setq-default whitespace-line-column 80)
  (global-whitespace-mode 1)
  (defun switch-whitespace-max-line (max)
    (progn
      (whitespace-mode)
      (setq-default whitespace-line-column max)
      (whitespace-mode)))
  (defun switch-whitespace-max-line-i (max)
    (interactive "nMax column for lines: ")
    (switch-whitespace-max-line max)
    )
  :bind (("C-c w" . whitespace-mode)
	 ("C-c t" . whitespace-toggle-options)
	 ("C-c l" . switch-whitespace-max-line-i))
)

;; Stacked git
(use-package stgit)

;; Git
(use-package magit
  :defer t)

(use-package work
  :ensure f)

(use-package custom-defs
  :ensure f)

(use-package doremi-cmd
  :defer t)

(use-package org-mode
  :ensure f
  :init
  (setq org-default-notes-file (concat org-directory "/notes/notes.org"))

  ;;https://lists.gnu.org/archive/html/emacs-orgmode/2008-05/msg00039.html
  (defun my-link-to-line-number-in-c-mode ()
    "When in c-mode, use line number as search item."
    (when (eq major-mode 'c-mode)
      (number-to-string (org-current-line))))

  (add-hook 'org-create-file-search-functions
	  'my-link-to-line-number-in-c-mode)

  (setq org-capture-templates
      '(("c" "Code" entry (file+headline "~/notes/notes.org" "Scratch")
             "** Snippet\n %i\n%a")))
  :bind ("C-c c" . org-capture)
)

(load-theme 'manoj-dark-mod t)
