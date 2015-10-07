;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq use-package-verbose t)

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path "~/.emacs.d/lib/use-package")
;; color theme, somehow version 6.6.x has lost it's themes
(add-to-list 'load-path "~/.emacs.d/lib/color-theme-6.5.5")

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
(use-package uniquify)

;; Support shell color codes
(use-package ansi-color
  :defer t)

(use-package diff-mode
             :mode "COMMIT_EDITMSG$"
             :config
             (set-face-foreground 'diff-added "green4")
             (set-face-foreground 'diff-removed "red3")
             )

(use-package cc-mode
  :defer t
  :init
  :bind  ("C-c o" . ff-find-other-file)
  :config
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

(use-package color-theme
  :config
  (color-theme-clarity))

(use-package compile-dir
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
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
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
)

;; Stacked git
(use-package stgit
  :defer t)

(use-package work)

(require 'custom-defs)
