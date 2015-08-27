(defvar racer-home "~/.emacs.d/racer")
(setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
(setq racer-cmd (concat racer-home "/target/release/racer"))
(setq tramp-default-method "ssh")
(setq indent-tabs-mode nil)
(setq-default tab-width 4)

(add-to-list 'load-path (concat racer-home "/editors"))

(require 'package)
(package-initialize)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

;;Common lisp
;;(require 'cl)

;;req-package
(require 'req-package)

;; Font setting
;; TODO: Add yethangul support.
;; TODO: Add emoji support.

;;Default font - Source Code Pro
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'light
                    :width 'normal)

;;Korean fallback - Noto Sans CJK KR
;;TODO: Find way to emulate fixedwidth
(set-fontset-font "fontset-default"
                  '(#xAC00 . #xD7A3)
                  (font-spec
                   :family "Noto Sans CJK KR"
                   :height 100
                   :weight 'light
                   :width 'normal))

;;Tengwar fallback

;;Workspace-detection function
(defun is-office-machine () (string= system-name "pmint-opengen.opengen.com"))

;;Download all packages from firstrun
(setq use-package-always-ensure t)

;;Packages
(req-package evil
             :config
             (evil-mode 1))

(req-package twittering-mode
             :config
             (setq twittering-use-master-password t))

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

(req-package web-mode
             :config
             (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.inc$" . web-mode)))

(req-package nasm-mode
             :config
             (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode)))

(req-package rust-mode)

(req-package racer)

(req-package company)
(req-package flycheck)
(req-package flycheck-rust
             :require (flycheck rust-mode racer))

(req-package-finish)

;;Built-in modules

;;html-mode
(add-hook 'html-mode-hook 'ac-html-enable)
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

;;js-mode
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js-mode))

(if (is-office-machine)
    (setq js-indent-level 2) (setq js-indent-level 4))

;;Javascript flycheck lint
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;;common configs
;; Tab is evil.
(setq-default indent-tabs-mode nil)

;;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;;(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
