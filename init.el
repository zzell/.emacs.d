;;; package -- Summary
;;; Commentary:
;;
;;    (      )
;;    ))    ((
;;   //      \\
;;  | \\____// |
;; \~/ ~    ~\/~~/
;;  (|    _/o  ~~
;;   /  /     ,|
;;  (~~~)__.-\ |
;;   ``~~    | |
;;    |      | |
;;    |        |
;;   /          \
;;  `\          /'
;;    `\_    _/'
;;       ~~~~
;;
;; ~~~~~~~~~~~~~~~
;;; Code:

(require 'package)
(package-initialize)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)
(load (concat user-emacs-directory "golang.el"))

;; ~~~ CORE ~~~

(server-start)

;; utf-8 staff
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-fringe-mode 0)
(save-place-mode 1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(electric-pair-mode t)
(global-font-lock-mode 1)

(add-to-list 'default-frame-alist '(width . 130))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill buffer after terminal exit."
  (kill-buffer))

(setq x-underline-at-descent-line t)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq mouse-wheel-follow-mouse 't
      scroll-conservatively 1000
      scroll-margin 1
      scroll-step 1
      mouse-wheel-scroll-amount '(6 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      redisplay-dont-pause t
      fast-but-imprecise-scrolling nil
      jit-lock-defer-time 0)

(setq-default ring-bell-function 'ignore)

(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; startup screen
(setq inhibit-startup-screen t
      initial-scratch-message (format ";; %s" (current-time-string)))

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t
              cursor-in-non-selected-windows t)

(setq-default display-line-numbers-type 'relative)
(setq-default auto-window-vscroll nil
              fill-column 80
              help-window-select t
              sentence-end-double-space nil
              show-trailing-whitespace nil)

(defvar autosave-dir (concat user-emacs-directory "autosave/"))
(when (not (file-exists-p autosave-dir)) (make-directory autosave-dir t))
(setq make-backup-files nil             ;; fuck them up, 2019 out there, everybody use git
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-default t               ;; auto-save every buffer that visits a file
      auto-save-timeout 20              ;; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200)           ;; number of keystrokes between auto-saves (default: 300)

(defun aggressive-prog-mode ()
  "Common rules for all prog modes."
  (display-line-numbers-mode))

(defun aggressive-elisp-mode ()
  "Elisp specific rules."
  (aggressive-indent-mode t))

;; hooks
(add-hook 'prog-mode-hook 'aggressive-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'aggressive-elisp-mode)

;; ~~~ PACKAGES ~~~

(use-package imenu
  :config
  (setq imenu-auto-rescan t
        imenu-use-popup-menu nil)
  (semantic-mode 1))

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config

  ;; fix underscore
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    (setq-default evil-symbol-word-search t))
  
  (evil-mode t)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  (setq evil-normal-state-cursor '(box "gainsboro")
        evil-insert-state-cursor '((bar . 2) "light steel blue")
        evil-visual-state-cursor '((hbar . 2) "light steel blue")))

(use-package general
  :config (general-define-key
           :states '(normal visual)
           :prefix ","
           "1" '(eyebrowse-switch-to-window-config-1 :which-key "layout 1")
           "2" '(eyebrowse-switch-to-window-config-2 :which-key "layout 2")
           "3" '(eyebrowse-switch-to-window-config-3 :which-key "layout 3")
           "4" '(eyebrowse-switch-to-window-config-4 :which-key "layout 4")
           "5" '(eyebrowse-switch-to-window-config-5 :which-key "layout 5")
           "j" '(avy-goto-char-timer :which-key "avy-jump")
           "/" '(swiper :which-key "swiper")
           "*" '(counsel-projectile-grep :which-key "grep-regex")
           "t" '(ansi-term :which-key "terminal")
           "p" '(counsel-yank-pop :which-key "yank-pop")
           "TAB" '(alternate-buffer :which-key "alternate-buffer")
           "i" '(counsel-imenu :which-key "imenu")
           "d" '(dired-other-frame :which-key "dired-other-frame")

           "f" '(:ignore t :which-key "files")
           "ff" '(counsel-find-file :which-key "find")
           "fp" '(counsel-projectile-find-file :which-key "projectile-find-file")
           "fr" '(counsel-recentf :which-key "recent")

           "b" '(:ignore t :which-key "buffers")
           "bb" '(counsel-ibuffer :which-key "ivy-switch")
           "bi" '(ibuffer-other-window :which-key "ibuffer")
           "br" '(rename-buffer :which-key "rename")

           "g"  '(:ignore t :which-key "golang")
           "gf" '(gofmt :which-key "gofmt")
           "ga" '(go-import-add :which-key "add-import")
           "gr" '(go-remove-unused-imports :which-key "remove-unused-imports")
           "gj" '(godef-jump :which-key "godef-jump")
           "gJ" '(go-guru-referrers :which-key "guru-referrers")
           "gd" '(godef-describe :which-key "godef-describe")
           "gc" '(godoc-at-point :which-key "godoc-at-point")

           "c"  '(:ignore t :which-key "flycheck")
           "cj" '(flycheck-next-error :which-key "next-error")
           "ck" '(flycheck-previous-error :which-key "previous-error")
           "cl" '(flycheck-list-errors :which-key "list-errors")

           "l" '(:ignore t :which-key "lisp")
           "le" '(eval-region :which-key "eval-region")
           ))

;; autocomplete
(use-package company
  :diminish company-mode
  :config
  (custom-set-faces '(company-preview ((t (:underline nil)))))
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-global-modes '(not gud-mode))
  (setq company-tooltip-minimum-width 30)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)

  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous)
  (define-key prog-mode-map (kbd "C-SPC") 'company-complete))

;; for C/C++ use rtags: https://github.com/Andersbakken/rtags
;; (setq-local company-backends '(company-clang))
(add-hook 'c-mode-hook (lambda()
                         (setq-local company-backends
                                     '(company-clang
                                       company-dabbrev-code
                                       company-keywords
                                       company-files
                                       company-dabbrev))
                         (company-mode 1)))

;; layouts
(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (setq-default eyebrowse-new-workspace t))

(use-package js2-mode
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (setq-local indent-line-function 'js-indent-line))))

;; restclient - orgstruct for .http files
(use-package restclient
  :config
  (defun http-restclient ()
    "Use restclient mode in .http files."
    (when (and (stringp buffer-file-name)
               (string-match "\\.http\\'" buffer-file-name))
      (restclient-mode)
      (orgstruct-mode)
      (setq-default orgstruct-heading-prefix-regexp "\\#+\\")))
  (add-hook 'find-file-hook 'http-restclient))

;; Syntax check
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change idle-buffer-switch new-line))
  (setq flycheck-indication-mode nil))

(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
  (setq flycheck-pos-tip-timeout 20))

;; Ivy
(use-package counsel
  :diminish ivy-mode
  :init
  (setq counsel-yank-pop-separator
        (concat "\n\n" (concat (apply 'concat (make-list 50 "---")) "\n")))
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (t . ivy--regex-fuzzy))))

(use-package neotree
  :config (global-set-key (kbd "M-1") 'neotree-toggle)
  (setq neo-theme 'ascii)
  (setq neo-window-width 35)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-default neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-force-change-root t)
  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") (neotree-make-executor
                 :file-fn 'neo-open-file
                 :dir-fn 'neo-open-dir)
    (kbd "TAB") (neotree-make-executor
                 :dir-fn 'neo-open-dir)
    "R" 'neotree-change-root
    "gr" 'neotree-refresh
    "q" 'neotree-hide
    "H" 'neotree-hidden-file-toggle
    "c" 'neotree-create-node
    "y" 'neotree-copy-node
    "d" 'neotree-delete-node
    "r" 'neotree-rename-node
    "J" 'neotree-dir
    "+" 'neotree-stretch-toggle
    "|" (neotree-make-executor
         :file-fn 'neo-open-file-vertical-split)
    "-" (neotree-make-executor
         :file-fn 'neo-open-file-horizontal-split)
    )
  )

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :config (projectile-mode t))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; hide minor modes
(use-package diminish
  :config
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'eldoc-mode)))

;; extrimely fucking cool tool
(use-package xclip
  :config (xclip-mode))

(use-package evil-escape
  :config
  (global-set-key (kbd "<escape>") 'evil-escape))

(use-package magit
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

;; ------------------------

;;   (global-git-gutter-mode +1)
;;   ;; (git-gutter:linum-setup)
;;   ;; REVERT
;;   ;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;;   )

;; (use-package diff-mode
;;   :ensure nil
;;   :config
;;   (set-face-attribute 'diff-added nil :background nil)
;;   (set-face-attribute 'diff-removed nil :background nil))

;; (use-package ediff-init
;;   :ensure nil
;;   :config
;;   (me/unboldify '(ediff-fine-diff-A
;;                   ediff-fine-diff-B
;;                   ediff-fine-diff-C)))

;; (use-package ediff-wind
;;   :ensure nil
;;   :config
;;   (setq-default
;;    ediff-split-window-function #'split-window-horizontally
;;    ediff-window-setup-function #'ediff-setup-windows-plain))

;; (use-package smerge-mode
;;   :ensure nil
;;   :config
;;   (zenburn-with-color-variables
;;    (set-face-attribute 'smerge-mine nil :background zenburn-red-2)
;;    (set-face-attribute 'smerge-other nil :background zenburn-green)
;;    (set-face-attribute 'smerge-refined-added nil :background zenburn-green-1)
;;    (set-face-attribute 'smerge-refined-removed nil :background zenburn-red-4)))

(use-package dired
  :ensure nil
  :delight dired-mode "Dired"
  :preface
  (defun me/dired-directories-first ()
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (advice-add 'dired-readin :after #'me/dired-directories-first)
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))))
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (setq-default
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-hide-details-hide-symlink-targets nil
   dired-listing-switches "-alh"
   dired-ls-F-marks-symlinks nil
   dired-recursive-copies 'always))

;; improve fuzzy finder
(use-package flx)

;; sexy color scheme
(use-package kaolin-themes
  :config
  ;; (load-theme 'kaolin-dark t)
  (load-theme 'kaolin-galaxy t)
  )

;; Easy-motion
(use-package avy)

;; "gc" comments
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode 1))

;; which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :diminish which-key-mode
  :config (which-key-mode))

(use-package highlight-parentheses
  :diminish 'highlight-parentheses-mode
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (setq hl-paren-colors '("Springgreen3" "IndianRed1" "IndianRed3" "IndianRed4" "firebrick4" "red4" "red4" "red4" "red4")))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

;; (use-package json-mode)
;; (use-package yaml-mode)
;; (use-package js2-mode :mode "\\.js\\'")

;; ~~~ CUSTOM ~~~

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun alternate-buffer
    (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer (cl-find-if (lambda (buffer)
                                    (not (eq buffer current-buffer)))
                                  (mapcar #'car (window-prev-buffers window))))))

(defun term-mode-paste ()
  "Paste into Emacs terminal like with bash."
  (define-key term-raw-map (kbd "C-y") 'term-paste))
(add-hook 'term-mode-hook 'term-mode-paste)

;; ~~~ END ~~~
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ycmd auto-complete-exuberant-ctags auto-complete-c-headers auto-complete-clang auto-complete aggressive-indent highlight-parentheses which-key evil-commentary avy kaolin-themes flx diff-hl evil-magit magit evil-escape xclip diminish counsel-projectile projectile neotree counsel flycheck-pos-tip restclient js2-mode eyebrowse general evil company-go flycheck-golangci-lint go-eldoc go-fill-struct go-guru go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:underline nil)))))
