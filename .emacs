(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
(require 'evil)
(evil-mode 1)
(add-to-list 'load-path "/urs/share/git/emacs")
(require 'git)
(require 'git-blame)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'linum-relative)
(global-linum-mode t)
(require 'sr-speedbar)
;; (sr-speedbar-open)
(add-hook 'c-mode-common-hook (lambda ()
  (cppcm-reload-all)
  ; fixed rinari's bug.
  (remove-hook 'find-file-hook 'rinari-launch)
  ))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Load CEDET
(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(global-cedet-m3-minor-mode 1)
;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
(semantic-load-enable-gaudy-code-helpers)
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
	c-basic-offset 4)
(defun indent-buffer ()
 "Indents an entire buffer using the default intenting scheme."
 (interactive)
 (point-to-register 'o)
 (delete-trailing-whitespace)
 (indent-region (point-min) (point-max) nil)
 (untabify (point-min) (point-max))
 (jump-to-register 'o))

(global-set-key (kbd "C-S-d") 'indent-buffer)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'compile)
(put 'upcase-region 'disabled nil)
(setq scroll-step            1
      scroll-conservatively  10000)

(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(require 'haskell-indent)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'haskell-mode-hook 'haskell-font-lock-symbols t)
(set 'haskell-font-lock-symbols t)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(put 'downcase-region 'disabled nil)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
