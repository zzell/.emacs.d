;;; package --- Summary
;;; Commentary:
;;; Code:

(let ((tools '(("bingo"         . "go get -u github.com/saibing/bingo")
               ;; not stable yet
               ;; ("gopls"         . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
               ("gocode"        . "go get -u github.com/mdempsky/gocode")
               ("golangci-lint" . "curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh| sh -s -- -b $(go env GOPATH)/bin v1.21.0")
               ("goimports"     . "go get -u -v golang.org/x/tools/cmd/goimports")
               ("guru"          . "go get -u -v golang.org/x/tools/cmd/guru")
               ("gorename"      . "go get -u -v golang.org/x/tools/cmd/gorename")
               ("fillstruct"    . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct")
               ("gomodifytags"  . "go get -u github.com/fatih/gomodifytags")
               ("godef"         . "go get -u github.com/rogpeppe/godef")
               )))

  (mapc (lambda (element)
          (let ((name (car element)) (cmd (cdr element)))
            (when (equal
                   (shell-command-to-string
                    (format "%s%s%s" "if command -v " name " >/dev/null; then printf 0; else printf 1; fi")) "1")
              (progn (message "installing %s..." name) (shell-command-to-string cmd))))) tools))

(add-hook 'go-mode-hook (lambda () (lsp)
                          ;; not the greates solution, but works
                          (add-to-list 'flycheck-checkers 'golangci-lint)
                          (flycheck-add-next-checker 'lsp-ui '(t . golangci-lint))
                          (aggressive-indent-mode t)
                          (add-hook 'lsp-eldoc-hook 'go-eldoc-setup)
                          (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package go-mode
  :config (setq-default gofmt-command "goimports"))
(use-package go-fill-struct)
(use-package go-eldoc)
(use-package go-tag)
(use-package go-add-tags) ;; experimental
(use-package go-playground)
(use-package go-stacktracer)
(use-package go-fill-struct)
(use-package flycheck-golangci-lint
  :config
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-disable-linters '("lll" "dupl" "maligned"
                                                 "prealloc" "gochecknoglobals" "gosec"
                                                 "funlen" "wsl" "gocognit")))

;;; golang.el ends here
