;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(require 'package)

(package-initialize)

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Use UTF-8 everywhere
(mapc (lambda (fn) (funcall fn 'utf-8))
      '(set-terminal-coding-system
        set-keyboard-coding-system
        prefer-coding-system))

;; Turn off that damn bell!
(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; whitespace-mode
(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

(show-paren-mode t)
(electric-indent-mode t)

(use-package evil-leader
  :commands (global-evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")

  (evil-leader/set-key
    "ff" 'find-file)
)

;; Be evil
(use-package evil
  :commands (evil-mode evil-map)
  :init (evil-mode t)
  :config
  (setq evil-shift-width 2)

  (defun evil-map (key def &rest bindings)
    (evil-leader--def-keys evil-normal-state-map key def bindings)
    (evil-leader--def-keys evil-visual-state-map key def bindings)
    (evil-leader--def-keys evil-motion-state-map key def bindings)
    (evil-leader--def-keys evil-operator-state-map key def bindings))
)

(use-package uniquify
  :demand t)

;;; Undo Tree
(use-package undo-tree
  :commands (undo-tree-undo undo-tree-redo)
  :init
  (evil-map "u" 'undo-tree-undo
            "C-r" 'undo-tree-redo
            "C-u" 'undo-tree-visualize))

;(define-key ctl-x-map "w" 'evil-window-map)
;(evil-leader/set-key "w" 'evil-window-map)
;(define-key evil-window-map "d" 'evil-window-left)
;(define-key evil-window-map "D" 'evil-window-move-far-left)
;(define-key evil-window-map "h" 'evil-window-down)
;(define-key evil-window-map "H" 'evil-window-move-very-bottom)
;(define-key evil-window-map "t" 'evil-window-up)
;(define-key evil-window-map "T" 'evil-window-move-very-top)
;(define-key evil-window-map "n" 'evil-window-right)
;(define-key evil-window-map "N" 'evil-window-move-far-right)
;(define-key evil-window-map "w" 'evil-window-new)
;(define-key evil-window-map "k" 'evil-window-delete)

(use-package helm-config
  :demand t
  :config
  (progn
    (use-package helm-mode
      :init (helm-mode 1)
      :diminish helm-mode)))

(use-package monokai-theme
  :defines (monokai))
;; Set color scheme
(add-hook 'after-init-hook (lambda () (load-theme 'monokai t)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda org-capture)
  :init
  )

(use-package undo-tree
  :commands (global-undo-tree-mode)
  :diminish undo-tree-mode)
(global-undo-tree-mode 1)

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;;; flycheck configuration

(use-package flycheck
  :commands (flycheck-mode)
  :config
  (setq flycheck-checkers (delq 'haskell-hlint flycheck-checkers)))

;;; ghc-mod configuration

(use-package company
  :ensure
  :bind ("C-<SPC>" . company-complete)
  :config
  (setq company-show-numbers t)
  :init (global-company-mode))


(use-package ghc
  :ensure
  :commands ghc-init ghc-debug
  :config
  (use-package company-ghc
    :ensure
    :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  )
  :init
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

;;; haskell-mode configuration

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode)
         ("\\.cabal\'" . haskell-cabal-mode))
  :commands (haskell-mode haskell-cabal-mode)
  :config
  (setq haskell-indentation-cycle-warn nil)
  (setq haskell-indentation-starter-offset 2)
  (setq haskell-literate-default 'tex)
  (setq haskell-process-auto-import-loaded-modules nil)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines nil)
  (setq haskell-process-type 'cabal-repl)
  (add-hook 'haskell-mode-hook (lambda () (linum-mode 1)))
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'electric-indent-functions
            (lambda (c) (when (or (eq 'haskell-mode major-mode)
                                  (eq 'haskell-cabal-mode major-mode))
                          'no-indent))))

;;; nix-mode configuration

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :commands (nix-mode))

;(require 'evil-surround)
;(global-evil-surround-mode 1)

;(require 'evil-indent-textobject)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(provide 'init)
;;; init.el ends here
