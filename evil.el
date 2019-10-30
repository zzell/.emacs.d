;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config

  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")))

  (evil-mode t)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  (setq evil-normal-state-cursor '(box)
        evil-insert-state-cursor '((bar . 2))
        evil-visual-state-cursor '((hbar . 2)))
  )

(use-package evil-escape
  :config
  (global-set-key (kbd "<escape>") 'evil-escape))

;; "gc" comments
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode 1))

;;; evil ends here
