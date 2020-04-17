;;; package -- Summary
;;; Commentary:
;;
;;     _-`````-,           ,- '- .
;;   .'   .- - |          | - -.  `.
;;  /.'  /                     `.   \
;; :/   :      _...   ..._      ``   :
;; ::   :     /._ .`:'_.._\.    ||   :
;; ::    `._ ./  ,`  :    \ . _.''   .
;; `:.      /   |  -.  \-. \\_      /
;;   \:._ _/  .'   .@)  \@) ` `\ ,.'
;;      _/,--'       .- .\,-.`--`.
;;        ,'/''     (( \ `  )
;;         /'/'  \    `-'  (
;;          '/''  `._,-----'
;;           ''/'    .,---'
;;            ''/'      ;:
;;              ''/''  ''/
;;                ''/''/''
;;                  '/'/'
;;                   `;
;;     ______
;;    / ____/___ ___  ____ ___________
;;   / __/ / __ `__ \/ __ `/ ___/ ___/
;;  / /___/ / / / / / /_/ / /__(__  )
;; /_____/_/ /_/ /_/\__,_/\___/____/
;;       The One True Program.
;;; Code:

;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow.
(setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process. Again
;; the emacs default is too low 4k considering that the some of the language
;; server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)

;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

(setq use-package-always-ensure t)
(load (concat user-emacs-directory "golang.el"))

;; ###########################################

(server-start)

;; utf-8 staff
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

;; UI
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
(setq x-stretch-cursor nil)

;; font-size
(set-face-attribute 'default nil :height 140)

;; default windows width
(add-to-list 'default-frame-alist '(width . 130))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill buffer after terminal exit."
  (kill-buffer))

(setq x-underline-at-descent-line t)
(setq confirm-kill-emacs #'y-or-n-p)

;; mouse
(setq mouse-wheel-follow-mouse 't
      scroll-conservatively 1000
      scroll-margin 1
      scroll-step 1
      mouse-wheel-scroll-amount '(6 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      redisplay-dont-pause t
      fast-but-imprecise-scrolling nil
      jit-lock-defer-time 0)

(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(setq-default ring-bell-function 'ignore)
(setq inhibit-startup-screen t
      initial-scratch-message (format ";; %s" (current-time-string)))

(setq-default indent-tabs-mode t
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
(setq make-backup-files nil
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-default nil   ;; auto-save every buffer that visits a file
      auto-save-timeout 20    ;; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200) ;; number of keystrokes between auto-saves (default: 300)

(setq vc-follow-symlinks t)

;; Remove trailing whitespace upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face t))))))

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; use aggressive-indent only for certain modes
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

;; ###########################################

(use-package company-lsp
	:config
	(push 'company-lsp company-backends)
	(setq company-lsp-async t)
	(setq company-lsp-enable-recompletion t))

(use-package lsp-mode
  :config
	(setq lsp-prefer-capf t)
	(push "[/\\\\]vendor$" lsp-file-watch-ignored))

(use-package python-mode
	:config (add-hook 'python-mode-hook 'lsp-deferred))

(defun lsp-save-hooks ()
	"."
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-save-hooks)
(add-hook 'python-mode-hook #'lsp-save-hooks)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)

  :config

  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")))

	(setq evil-disable-insert-state-bindings t)

  (evil-mode t)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  (setq evil-normal-state-cursor '(box)
        evil-insert-state-cursor '((bar . 2))
        evil-visual-state-cursor '((hbar . 2)))

	(with-eval-after-load 'evil-maps
		(define-key evil-normal-state-map (kbd "/") 'swiper)))

(use-package evil-escape
  :config
  (global-set-key (kbd "<escape>") 'evil-escape))

;; "gc" comments
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (go-mode . yas-minor-mode))

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
  (setq company-minimum-prefix-length 1)

  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key prog-mode-map (kbd "C-SPC") 'company-complete)

  ;; for C/C++ use rtags: https://github.com/Andersbakken/rtags
  (add-hook 'c-mode-hook (lambda()
                           (setq-local company-backends
                                       '(company-clang
                                         company-dabbrev-code
                                         company-keywords
                                         company-files
                                         company-dabbrev))
                           (company-mode 1))))

;; http client + orgstruct for .http files
(use-package restclient
  :config
  (defun http-restclient ()
    "Use restclient mode in .http files."
    (when (and (stringp buffer-file-name) (string-match "\\.http\\'" buffer-file-name))
      (restclient-mode)
      (orgstruct-mode)
      (setq-default orgstruct-heading-prefix-regexp "\\#+\\")))
  ;; improve JSON intentations
  (add-hook 'restclient-mode-hook (lambda () (setq-local indent-line-function 'js-indent-line)))
  (add-hook 'find-file-hook 'http-restclient))

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

;; this one needed for ivy
(use-package flx)

(use-package js2-mode
  :mode(".js"))

(use-package protobuf-mode
  :mode (".yml" ".yaml"))

(use-package yaml-mode
  :mode (".yml" ".yaml"))

(use-package json-mode
  :mode (".json" ".imp"))

(use-package dockerfile-mode
  :mode ("Dockerfile"))

(use-package asm-mode
  :mode ("\\.s\\'"))

(use-package markdown-mode
  :mode (".md" ".markdown"))

(use-package imenu
  :config
  (setq imenu-auto-rescan t
        imenu-use-popup-menu nil))

;; highlight matching parenthesis
(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0) ;; doesn't load
  (setq show-paren-style 'parenthesis))

;; darker parens
(use-package paren-face
  :config (global-paren-face-mode))

(use-package hydra)

(defhydra hydra-buffers (:color blue)
	"#"
  ("l" next-buffer "next" :color red)
  ("h" previous-buffer "prev" :color red)
  ("d" kill-this-buffer "delete" :color red)
  ("b" counsel-ibuffer "switch")
	("i" ibuffer-other-window "ibuffer")
	("r" rename-buffer "rename" :color red))

(defhydra hydra-flycheck (:color blue)
	"#"
	("j" flycheck-next-error "next" :color red)
	("k" flycheck-previous-error "prev" :color red)
	("l" flycheck-list-errors "list"))

(defhydra hydra-vc (:color blue)
	"VC"
	("r" diff-hl-revert-hunk "revert")
	("j" diff-hl-next-hunk "next" :color red)
	("k" diff-hl-previous-hunk "prev" :color red))

(defhydra hydra-eyebrowse (:color blue)
	"Eyebrowse"
	("l" eyebrowse-next-window-config "next" :color red)
	("h" eyebrowse-prev-window-config "prev" :color red)
	("n" eyebrowse-create-window-config "new" :color red)
	("1" eyebrowse-switch-to-window-config-1 "1")
	("2" eyebrowse-switch-to-window-config-2 "2")
	("3" eyebrowse-switch-to-window-config-3 "3")
	("4" eyebrowse-switch-to-window-config-4 "4")
	("5" eyebrowse-switch-to-window-config-5 "5"))

(defhydra hydra-windows (:color blue)
	"W"
	("k" shrink-window "shrink-v" :color red)
	("j" enlarge-window "enlarge-v" :color red)
	("h" shrink-window-horizontally "shrink-h" :color red)
	("l" enlarge-window-horizontally "enlarge-h" :color red))

(use-package general
  :config
	(general-define-key

	 :states '(normal visual)
	 :prefix "g"
	 "d" '(lsp-find-definition :wich-key "definition")
	 "r" '(lsp-find-references :wich-key "references")
	 "i" '(lsp-find-implementation :wich-key "implementation")
	 "n" '(lsp-rename :wich-key "definition"))

	(general-define-key
	 :states '(normal visual)
	 :prefix "SPC"
	 "/" '(counsel-projectile-ag :wich-key "ag")
	 "TAB" '(alternate-buffer :which-key "alternate-buffer")
	 "j" '(avy-goto-char-timer :which-key "jump avy-goto-char-timer")
	 "t" '(ansi-term /bin/bash :which-key "ansi-term")
	 "p" '(counsel-yank-pop :which-key "yank-pop")
	 "i" '(counsel-imenu :which-key "imenu")
	 "d" '(dired-jump :which-key "dired")

	 "f" '(:ignore t :which-key "files")
	 "ff" '(counsel-find-file :which-key "find")
	 "fr" '(counsel-recentf :which-key "recent")
	 "f/" '(counsel-projectile-find-file :wich-key "find")
	 "fw" '(counsel-projectile-find-file-dwim :wich-key "find dwim")
	 "fg" '(counsel-git :wich-key "git")

	 "w" '(hydra-windows/body :which-key "windows")
	 "0" '(hydra-eyebrowse/body :which-key "eyebrowse")
	 "b" '(hydra-buffers/body :which-key "buffers")
	 "c" '(hydra-flycheck/body :which-key "flycheck")
	 "v" '(hydra-vc/body :which-key "vc")))

(use-package neotree
  :config ;;(global-set-key (kbd "M-1") 'neotree-toggle)
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

;; layouts
(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (setq-default eyebrowse-new-workspace t))

;; Syntax check
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change idle-buffer-switch new-line))
  (setq flycheck-indication-mode nil))

;; (use-package flycheck-pos-tip
;;   :config
;;   (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
;;   (setq flycheck-pos-tip-timeout 20))

;; hide minor modes
(use-package diminish
  :config
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'eldoc-mode)))

;; copy and paste from clipboard in terminal
(use-package xclip
  :config (xclip-mode))

;; highlight changes in file under version control
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

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

;; sexy color scheme
(use-package kaolin-themes
  :config (load-theme 'kaolin-galaxy t))

;; easymotion
(use-package avy
	:config
	(setq avy-background t))

;; which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :diminish which-key-mode
  :config (which-key-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

;; sudo apt-get install silversearcher-ag
(use-package ag
	:config
	(setq ag-highlight-search t)
	(setq ag-reuse-window 't))

(use-package wgrep-ag)

;; ###########################################

(defun alternate-buffer
    (&optional window)
  "Switch between alternate buffers, WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer (cl-find-if (lambda (buffer)
                                    (not (eq buffer current-buffer)))
                                  (mapcar #'car (window-prev-buffers window))))))

;; ###########################################
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(python-mode lsp-treemacs ace-jump-mode wgrep-ag csv-mode fzf go-stacktracer go-rename go-playground go-add-tags go-tag gorepl-mode gore-mode yasnippet yaml-mode xclip which-key use-package restclient request rainbow-delimiters protobuf-mode persp-mode paren-face nord-theme neotree minimap lsp-haskell kaolin-themes json-mode js2-mode indent-guide highlight-parentheses highlight-indentation highlight-indent-guides gruvbox-theme go-guru go-fill-struct go-eldoc go-autocomplete ggtags general focus flycheck-status-emoji flycheck-pos-tip flycheck-golangci-lint flx eyebrowse exec-path-from-shell evil-magit evil-escape evil-commentary evil-cleverparens dumb-jump doom-themes doom dockerfile-mode diminish diff-hl darktooth-theme counsel-projectile company-lsp company-go company-ebdb color-theme-sanityinc-tomorrow avy aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:underline nil)))))
(put 'dired-find-alternate-file 'disabled nil)
