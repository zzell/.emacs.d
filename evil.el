;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config

  ;; treat underscore as part of word
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

(use-package evil-escape
  :config
  (global-set-key (kbd "<escape>") 'evil-escape))

;; "gc" comments
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode 1))

;;; evil ends here
