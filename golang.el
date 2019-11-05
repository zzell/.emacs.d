;;; package --- Summary
;;; Commentary:
;;; Code:

(let ((tools '(("bingo"         . "go get -u github.com/saibing/bingo")
               ;; not stable yet
               ;; ("gopls"         . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
               ("golangci-lint" . "curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh| sh -s -- -b $(go env GOPATH)/bin v1.21.0")
               ("goimports"     . "go get -u -v golang.org/x/tools/cmd/goimports")
               ("guru"          . "go get -u -v golang.org/x/tools/cmd/guru")
               ("fillstruct"    . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct")
               ("gomodifytags"  . "go get -u github.com/fatih/gomodifytags")
               ("godef"         . "go get -u github.com/rogpeppe/godef"))))

  (mapc (lambda (element)
          (let ((name (car element)) (cmd (cdr element)))
            (when (equal
                   (shell-command-to-string
                    (format "%s%s%s" "if command -v " name " >/dev/null; then printf 0; else printf 1; fi")) "1")
              (progn (message "installing %s..." name) (shell-command-to-string cmd))))) tools))

(use-package go-mode
  :config
  (setq-default gofmt-command "goimports")
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'lsp-after-open-hook
            '(lambda ()
               (when (eq major-mode 'go-mode)
                 ;; append golangci-lint after lsp-ui
                 (add-to-list 'flycheck-checkers 'golangci-lint)
                 (flycheck-add-next-checker 'lsp-ui '(t . golangci-lint))))))

(use-package go-fill-struct)
(use-package go-eldoc)
(use-package go-tag)
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


(setq go-packages-function 'blb)

(defvar *custom-go-cached-imports* (list))

(defun blb ()
  "Blank."
  *custom-go-cached-imports*)

(start-process "qwer" "quer-buffer" 'custom-go-imports)

(defun custom-go-imports ()
  "Blank."
  (interactive)

  (let ((root (custom-go-find-project-root)))
    (if root
        (progn
          (setq *custom-go-cached-imports* (list))
          (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (go-packages-native)))
          (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-local-imports-list root)))
          (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-vendored-imports-list root))))
      (setq *custom-go-cached-imports* (list))
      (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-all-imports)))))

  )

(defvar *custom-go-gopath* (shell-command-to-string "printf $GOPATH"))

(defun custom-go-find-project-root ()
  "Return project root or nil if project is not in GOPATH."
  (let ((gopath-src (concat *custom-go-gopath* "/src"))
        (pwd (shell-command-to-string "pwd | tr -d '\n'")))
    (when (string-match-p gopath-src pwd)
      (let ((elems (split-string (substring pwd (1+ (length gopath-src)) nil) "/")))
        (when (>= (length elems) 3)
          (concat (nth 0 elems) "/" (nth 1 elems) "/" (nth 2 elems)))))))

(defun custom-go-local-imports-list (path)
  "Fast. PATH - project root path."
  (process-lines "go" "list" "-e" (concat path "/...")))

(defun custom-go-vendored-imports-list (path)
  "Slow. PATH - project root path."
  (let* ((absolute-path (concat *custom-go-gopath* "/src/" path))
         (vendor-path (shell-command-to-string (format "find %S -type d -name 'vendor' | tr -d '\n'" absolute-path))))
    (unless (string= vendor-path "")
      (let ((imports (process-lines "go" "list" "-e"
                                    (concat (substring vendor-path
                                                       (length (concat *custom-go-gopath* "/src/"))) "/...")))
            (unprefix (list)))
        (dolist (i imports) (push (replace-regexp-in-string "^.*\/vendor\/" "" i) unprefix)) unprefix))))

(defun custom-go-all-imports ()
  "Extremely slow."
  (process-lines "go" "list" "-e" "all"))


;;; golang.el ends here
