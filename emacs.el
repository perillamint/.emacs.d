(defvar emacs-home "~/.emacs.d")
(defvar racer-home (concat emacs-home "/racer"))
(defvar lfe-home (concat emacs-home "/lfe"))
(setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
(setq racer-cmd (concat racer-home "/target/release/racer"))
(setq tramp-default-method "ssh")
(setq indent-tabs-mode nil)
(setq-default tab-width 4)

(add-to-list 'load-path (concat racer-home "/editors"))
(add-to-list 'load-path (concat lfe-home "/emacs"))
(add-to-list 'load-path (concat emacs-home "/PG/generic"))
(add-to-list 'load-path (concat emacs-home "/js-doc"))
;;(add-to-list 'load-path (concat emacs-home "/lib"))

;;Theme
(add-to-list 'custom-theme-load-path (concat emacs-home
                                             "/emacs-open-color-theme"))

;;Get CAcerts file and set tls-program
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;Disable splash
(setq inhibit-splash-screen t)

(load (concat emacs-home "/linuxkern.el"))
;;Common lisp
;;(require 'cl)

;;req-package
(require 'req-package)

;;Set font
(load (concat emacs-home "/font.el"))
(set-font 110)

;;Workspace-detection function -- Office machine is Nimrodel.
(defun is-office-machine () (string= system-name "nimrodel.gentoo.moe"))

;;Download all packages from firstrun
(setq use-package-always-ensure t)

;; Packages
(req-package evil
             :config
             (evil-mode 1))

(req-package bind-key
  :config
  (load (concat emacs-home "/keybinding.el")))

(req-package undo-tree)

(req-package twittering-mode
             :config
             (setq twittering-use-master-password t)
             (setq twittering-private-info-file (concat emacs-home
                                                        "/twittering-mode.gpg"))
             (load (concat emacs-home "/twittering-official-app.el")))

;;(req-package evernote-mode
;;             :config
;;             (setq evernote-developer-token
;;                   ""))

(req-package geeknote)
(req-package gist)

(req-package whitespace)

(req-package tabbar-ruler
             :require cl
             :config
             (setq tabbar-ruler-global-tabbar t)
             (setq tabbar-ruler-global-ruler t)
             (setq tabbar-ruler-movement-timer-delay 0.1)

             (defun my-tabbar-buffer-groups ()
               "Custom tabbar groups"
               (list
                (cond
                 ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                 ((eq major-mode 'emacs-lisp-byte-code-mode) "emacs")
                 ((eq major-mode 'dired-mode) "dired")
                 ((eq major-mode 'html-mode) "web")
                 ((eq major-mode 'javascript-mode) "web")
                 ((eq major-mode 'js-mode) "web")
                 ((eq major-mode 'js2-mode) "web")
                 ((eq major-mode 'rust-mode) "rust")
                 (t "others"))))

             (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups))

(req-package js2-mode)

;; For typescript
(req-package tss
  :require typescript
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(req-package web-mode
             :config
             (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.inc$" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode)))

(req-package nasm-mode
             :config
             (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode)))

(req-package rust-mode)

(req-package racer)

(req-package company)
(req-package flycheck)
(req-package flycheck-rust
             :require (flycheck rust-mode racer))

(req-package lua-mode
  :require (flymake-lua company company-lua)
  :config
  (add-to-list 'company-backends 'company-lua)
  (add-hook 'lua-mode-hook 'flymake-lua-load))

(req-package yaml-mode
  :require (flymake-yaml)
  :config
  (add-hook 'yaml-mode-hook 'flymake-yaml-load))

(req-package flymake-yaml
  :require (flycheck))

(req-package auto-complete)

(req-package yasnippet
  :config
  (yas/initialize)
  (add-to-list 'ac-sources 'ac-source-yasnippet))

;;Elixir Alchemist mode
(req-package alchemist
  :require (company)
  :config
  (add-hook 'alchemist-mode-hook 'company-mode))

(req-package flycheck-elixir
  :require (flycheck))

(req-package-finish)

;;LFE mode
(require 'lfe-start)


;;Built-in modules

;;html-mode
(add-hook 'html-mode-hook 'ac-html-enable)
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

;;js2-mode
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))

(if (is-office-machine)
    (setq js-indent-level 4) (setq js-indent-level 4))

;;Javascript flycheck lint
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(add-hook 'typescript-mode-hook
          (lambda () (flycheck-mode t)))

(require 'js-doc)

(setq js-doc-mail-address (if (is-office-machine)
                              "yhban@gentoo.moe"
                            "perillamint@gentoo.moe"))

(setq js-doc-author (format (if (is-office-machine)
                                "Yong-hyu, ban <%s>"
                              "perillamint <%s>") js-doc-mail-address))

(add-hook 'js-mode-hook
          #'(lambda ()
              (define-key js-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js-mode-map "@" 'js-doc-insert-tag)))

;;TODO: fill this.
(setq js-doc-url "")
(setq js-doc-license "")

;;Proof General
(require 'proof-site)

;;common configs
;; Tab is evil.
(setq-default indent-tabs-mode nil)

;;Auto-complete config
(require 'auto-complete-config)

(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

;;Custom compile command.
(global-set-key (kbd "<f5>") 'compile)

;;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;;(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Theme

(setq frame-background-mode 'dark)
(load-theme 'open-color t)
(enable-theme 'open-color)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (if (display-graphic-p frame)
                (progn (set-frame-parameter frame 'background-mode 'dark)
                 (enable-theme 'open-color)))))
