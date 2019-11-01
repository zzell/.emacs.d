;;; package --- Summary
;;; Commentary:
;;; Code:

(let ((tools '(
               ("gocode"        . "go get -u github.com/nsf/gocode")
               ("gopls"         . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
               ("golangci-lint" . "go get -u github.com/golangci/golangci-lint/cmd/golangci-lint")
               ("goimports"     . "go get -u -v golang.org/x/tools/cmd/goimports")
               ("guru"          . "go get -u -v golang.org/x/tools/cmd/guru")
               ("gorename"      . "go get -u -v golang.org/x/tools/cmd/gorename")
               ("fillstruct"    . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct")


               ("gomodifytags"  . "go get -u https://github.com/fatih/gomodifytags")
               ("gore"          . "GO111MODULE=off go get -u github.com/motemen/gore/cmd/gore") ;; TODO how to use?

               )))
  (mapc (lambda (element)
          (let ((name (car element)) (cmd (cdr element)))
            (when (equal
                   (shell-command-to-string
                    (format "%s%s%s" "if command -v " name " >/dev/null; then printf 0; else printf 1; fi")) "1")
              (progn (message "installing %s..." name) (shell-command-to-string cmd))))) tools))

;; (see URL ‘https://github.com/flycheck/flycheck’) or flymake in combination
;; with goflymake (see URL ‘https://github.com/dougm/goflymake’), gocode
;; (see URL ‘https://github.com/nsf/gocode’), go-eldoc
;; (see URL ‘github.com/syohex/emacs-go-eldoc’) and yasnippet-go
;; (see URL ‘https://github.com/dominikh/yasnippet-go’)

;; ;; godef? nope
;; go get -u github.com/sqs/goreturns
;; go get -u github.com/mgechev/revive
;; go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
;; go get -u github.com/zmb3/gogetdoc
;; go get -u github.com/zmb3/goaddimport
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/tpng/gopkgs
;; go get -u github.com/ramya-rao-a/go-outline

;; (use-package go-tag)
;; (use-package go-add-tags)
;; (use-package go-playground)
;; (use-package go-rename)
;; (use-package go-stacktracer)
;; (use-package gore-mode)
;; (use-package gorepl-mode
;;   :ensure t
;;   :config
;;   (add-hook 'go-mode-hook #'gorepl-mode))

(add-hook 'go-mode-hook (lambda ()
                          (add-hook 'lsp-eldoc-hook 'go-eldoc-setup)
                          (lsp)
                          ;; (lsp-ui-mode)
                          (aggressive-indent-mode t)
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          ))

(use-package go-mode
  :config (setq-default gofmt-command "goimports"))


;; quick docs in minibuffer
(use-package go-eldoc)

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-disable-linters '("lll" "dupl" "maligned" "prealloc" "gochecknoglobals" "gosec"))
  (setq flycheck-golangci-lint-fast t)
  )


;; ;; go-code refrence
;; (use-package go-guru
;;   :config
;;   (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)
;;   ;; (add-to-list 'display-buffer-alist
;;   ;;              `(,(rx bos "*go-guru-output*" eos)
;;   ;;                (display-buffer-reuse-window
;;   ;;                 display-buffer-in-side-window)
;;   ;;                (side            . bottom)
;;   ;;                (reusable-frames . visible)
;;   ;;                (window-height   . 0.33)))
;;   )

;; (use-package go-fill-struct)

;;; golang.el ends here
