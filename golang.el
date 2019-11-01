;;; package --- Summary
;;; Commentary:

(let ((tools '(("gopls"         . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
               ("gocode"        . "go get -u github.com/nsf/gocode")
               ("golangci-lint" . "curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh| sh -s -- -b $(go env GOPATH)/bin v1.21.0")
               ("goimports"     . "go get -u -v golang.org/x/tools/cmd/goimports")
               ("godef"         . "go get github.com/rogpeppe/godef")
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


(setq-default gofmt-command "goimports")

(add-hook 'go-mode-hook (lambda ()
                          ;; not the greates solution, but works
                          (add-to-list 'flycheck-checkers 'golangci-lint)
                          (flycheck-add-next-checker 'lsp-ui '(t . golangci-lint))
                          (add-hook 'lsp-eldoc-hook 'go-eldoc-setup)
                          (aggressive-indent-mode t)
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (lsp)
                          ))
;;; Code:
(use-package go-mode)

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

(use-package go-fill-struct)

;; quick docs in minibuffer
(use-package go-eldoc)

;; linter
;; golangci-lint can't run linters on fly, because it needs a file-tree to analyze
(use-package flycheck-golangci-lint
  ;; :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-disable-linters '("lll" "dupl" "maligned" "prealloc" "gochecknoglobals" "gosec"))
  (setq flycheck-golangci-lint-fast t))

;; ;; autocomplete
;; (use-package company-go
;;   :config
;;   (add-to-list 'company-backends 'company-go)
;;   ;; (add-hook 'go-mode-hook
;;   ;;           (lambda ()
;;   ;;             (set (make-local-variable 'company-backends) '(company-go))
;;   ;;             ;; (company-mode)
;;   ;;             ))
;;   )


;;; golang.el ends here
