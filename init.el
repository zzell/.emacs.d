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

;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq use-package-always-ensure t)
(load (concat user-emacs-directory "golang.el"))
(load (concat user-emacs-directory "evil.el"))

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

;; default windows width
(add-to-list 'default-frame-alist '(width . 130))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill buffer after terminal exit."
  (kill-buffer))

(setq x-underline-at-descent-line t)
(setq confirm-kill-emacs #'yes-or-no-p)

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

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (aggressive-indent-mode t)))

;; ###########################################

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil)

  ;; prefer aggressive-indent-mode
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-eldoc-render-all nil)
  (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :hook (lsp-ui-mode . lsp-ui-sideline-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-symbol nil)

  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next))

(use-package yasnippet
  :diminish yas-minor-mode)

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

(use-package general
  :config (general-define-key
           :states '(normal visual)
           :prefix ","
           "1" '(eyebrowse-switch-to-window-config-1 :which-key "layout 1")
           "2" '(eyebrowse-switch-to-window-config-2 :which-key "layout 2")
           "3" '(eyebrowse-switch-to-window-config-3 :which-key "layout 3")
           "j" '(avy-goto-char-timer :which-key "avy-goto-char-timer")
           "/" '(swiper :which-key "swiper")
           "t" '(ansi-term :which-key "ansi-term")
           "p" '(counsel-yank-pop :which-key "yank-pop")
           "TAB" '(alternate-buffer :which-key "alternate-buffer")
           "i" '(counsel-imenu :which-key "imenu")

           "d" '(:ignore t :which-key "dired")
           "dd" '(dired-jump :which-key "jump")
           "df" '(dired-find :which-key "find")

           "f" '(:ignore t :which-key "files")
           "ff" '(counsel-find-file :which-key "find")
           "fr" '(counsel-recentf :which-key "recent")

           "b" '(:ignore t :which-key "buffers")
           "bb" '(counsel-ibuffer :which-key "switch")
           "bi" '(ibuffer-other-window :which-key "ibuffer")
           "br" '(rename-buffer :which-key "rename")

           "g"  '(:ignore t :which-key "golang")
           "gf" '(gofmt :which-key "gofmt")
           "ga" '(go-import-add :which-key "add-import")
           "gn" '(go-goto-function-name :which-key "go-goto-function-name")
           "gd" '(godoc :which-key "godoc")
           "gj"  '(:ignore t :which-key "jump")
           "gjo" '(godef-jump-other-window :which-key "godef-jump-other-window")
           "gjj" '(godef-jump :which-key "godef-jump")

           "c"  '(:ignore t :which-key "flycheck")
           "cj" '(flycheck-next-error :which-key "next-error")
           "ck" '(flycheck-previous-error :which-key "previous-error")
           "cl" '(flycheck-list-errors :which-key "list-errors")

           "e" '(:ignore t :which-key "elisp")
           "ee" '(eval-region :which-key "eval-region")

           "l" '(:ignore t :which-key "lsp")
           "lr" '(lsp-rename :which-key "rename")
           "lf" '(lsp-ui-peek-jump-forward :which-key "jump-forward")
           "lb" '(lsp-ui-peek-jump-backward :which-key "jump-backward")
           "ll" '(lsp-ui-flycheck-list :which-key "flycheck-list")
           "lp" '(:ignore t :which-key "peek-findfind")
           "lpd" '(lsp-ui-peek-find-definitions :which-key "definitions")
           "lpr" '(lsp-ui-peek-find-references :which-key "references")
           "li" '(lsp-ui-imenu :which-key "imenu")
           "ls" '(lsp-ui-sideline-toggle-symbols-info :which-key "sideline")
           "ld" '(lsp-describe-thing-at-point :which-key "describe")

           "v" '(:ignore t :which-key "version control")
           "vr" '(diff-hl-revert-hunk :which-key "revert")
           "vn" '(diff-hl-next-hunk :which-key "next")
           "vp" '(diff-hl-previous-hunk :which-key "previous")
           ))

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

(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
  (setq flycheck-pos-tip-timeout 20))

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
(use-package avy)

;; which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :diminish which-key-mode
  :config (which-key-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

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

;; ;; do I need this?
;; (use-package highlight-parentheses
;;   :diminish 'highlight-parentheses-mode
;;   :config
;;   (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
;;   (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
;;   (setq hl-paren-colors '("Springgreen3" "IndianRed1" "IndianRed3" "IndianRed4" "firebrick4" "red4" "red4" "red4" "red4")))

;; ;; improve fuzzy finder
;; (use-package flx)

;; ;; newer use it
;; (use-package projectile
;;   :diminish projectile-mode
;;   :init
;;   (setq projectile-completion-system 'ivy)
;;   (setq projectile-switch-project-action 'neotree-projectile-action)
;;   :config (projectile-mode t))
;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

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


;; ;; why don't use dired instead?
;; (use-package neotree
;;   :config (global-set-key (kbd "M-1") 'neotree-toggle)
;;   (setq neo-theme 'ascii)
;;   (setq neo-window-width 35)
;;   (setq projectile-switch-project-action 'neotree-projectile-action)
;;   (setq-default neo-smart-open t)
;;   (setq neo-show-hidden-files t)
;;   (setq neo-force-change-root t)
;;   (evil-define-key 'normal neotree-mode-map
;;     (kbd "RET") (neotree-make-executor
;;                  :file-fn 'neo-open-file
;;                  :dir-fn 'neo-open-dir)
;;     (kbd "TAB") (neotree-make-executor
;;                  :dir-fn 'neo-open-dir)
;;     "R" 'neotree-change-root
;;     "gr" 'neotree-refresh
;;     "q" 'neotree-hide
;;     "H" 'neotree-hidden-file-toggle
;;     "c" 'neotree-create-node
;;     "y" 'neotree-copy-node
;;     "d" 'neotree-delete-node
;;     "r" 'neotree-rename-node
;;     "J" 'neotree-dir
;;     "+" 'neotree-stretch-toggle
;;     "|" (neotree-make-executor
;;          :file-fn 'neo-open-file-vertical-split)
;;     "-" (neotree-make-executor
;;          :file-fn 'neo-open-file-horizontal-split)
;;     )
;;   )

;; ###########################################
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fzf go-stacktracer go-rename go-playground go-add-tags go-tag gorepl-mode gore-mode yasnippet yaml-mode xclip which-key use-package restclient request rainbow-delimiters protobuf-mode persp-mode paren-face nord-theme neotree minimap lsp-haskell kaolin-themes json-mode js2-mode indent-guide highlight-parentheses highlight-indentation highlight-indent-guides gruvbox-theme go-guru go-fill-struct go-eldoc go-autocomplete ggtags general focus flycheck-status-emoji flycheck-pos-tip flycheck-golangci-lint flx eyebrowse exec-path-from-shell evil-magit evil-escape evil-commentary evil-cleverparens dumb-jump doom-themes doom dockerfile-mode diminish diff-hl darktooth-theme counsel-projectile company-lsp company-go company-ebdb color-theme-sanityinc-tomorrow avy aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:underline nil)))))
