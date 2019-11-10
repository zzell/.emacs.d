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

(setq go-packages-function 'go-custom-packages-function)
(add-hook 'go-mode-hook `(lambda () (go-custom-refresh-imports t)))

(defconst go-custom-gopath (shell-command-to-string "printf $GOPATH"))
(defvar go-custom-cache (list))
(defvar go-custom-package nil)
(defvar go-custom-init nil)

(defun go-custom-packages-function ()
  "Return imports list for 'go-packages-function'."
  go-custom-cache)

(defun go-custom-refresh-imports (i)
  "Refresh cached imports.
Should be used as hook for 'go-mode'.
Looks for imports depending on project. If new buffer belongs
to previously opened project it does nothing, unless called interactively.
Otherwise loads imports for project. If file isn't in GOPATH loads
system-wide imports. I used to determine if function is called interactively."
  (interactive "i")
  (let* ((pkg (go-custom--find-package))
         (same-pkg (string= go-custom-package pkg)))
    (when (or (not i) (not same-pkg) (not go-custom-init))
      (setq go-custom-package pkg)
      (setq go-custom-cache (list))
      (setq go-custom-init t)
      (if pkg (progn
                (go-custom--find-native-imports
                 `(lambda (imports)
                    (go-custom--append-to-cache imports)
                    (if imports (message "Found %S native imports." (length imports)))
                    (go-custom--find-vendored-imports
                     ,pkg '(lambda (imports)
                             (go-custom--append-to-cache imports)
                             (if imports (message "Found %S vendored imports." (length imports))))))))
        (go-custom--find-all-imports
         '(lambda (imports)
            (go-custom--append-to-cache imports)
            (if imports (message "Found all %S imports." (length imports)))))))))

(defun go-custom--find-native-imports (callback)
  "Asynchronously find native imports.
CALLBACK - filter function that receives list of packages."
  (process-shell-command-stdout-list  "go list -e ..." callback))

(defun go-custom--find-vendored-imports (pkg callback)
  "Asynchronously find vendored imports (e.g. when using Glide).
PKG - go package where to look up.
CALLBACK - filter function that receives list of packages."
  (let* ((absolute-path (concat go-custom-gopath "/src/" pkg))
         (vendor-path (shell-command-to-string (format "find %S -type d -name 'vendor' -prune | tr -d '\n'" absolute-path))))
    (unless (string= vendor-path "")
      (process-shell-command-stdout-list
       (format "go list -e %S/..." (replace-regexp-in-string "^.*?\/src\/" "" vendor-path))
       `(lambda (imports) (,callback (go-custom--rm-vendor-prefix imports)))))))

(defun go-custom--find-all-imports (callback)
  "Asynchronously find all system imports.
CALLBACK - filter function that receives list of packages."
  (process-shell-command-stdout-list
   "go list -e all"
   `(lambda (imports) (,callback (go-custom--rm-vendor-prefix imports)))))

(defun go-custom--append-to-cache (imports)
  "Add IMPORTS to cache."
  (setq go-custom-cache (sort (copy-sequence (append go-custom-cache (delete-dups imports))) #'string-lessp)))

(defun go-custom--find-package ()
  "Return project in form 'github.com/user/project' or nil if project is not in GOPATH."
  (let ((gopath-src (concat go-custom-gopath "/src"))
        (pwd (shell-command-to-string "pwd | tr -d '\n'")))
    (when (string-match-p gopath-src pwd)
      (let ((elems (split-string (substring pwd (1+ (length gopath-src)) nil) "/")))
        (when (>= (length elems) 3)
          (concat (nth 0 elems) "/" (nth 1 elems) "/" (nth 2 elems)))))))

(defun go-custom--rm-vendor-prefix (imports)
  "Remove 'vendor' prefix from IMPORTS."
  (let ((unprefix (list)))
    (dolist (i imports) (push (replace-regexp-in-string "^.*?\/vendor\/" "" i) unprefix)) unprefix))

(defun process-shell-command-stdout-list (cmd callback)
  "Run CMD asynchronously and call CALLBACK on exit.
CALLBACK is filter function that receives one parameter - list of lines from CMD stdout."
  (let ((tmp-buf (generate-new-buffer "*loading*")))
    (set-process-sentinel
     (start-process (number-to-string (random)) tmp-buf shell-file-name shell-command-switch cmd)
     `(lambda (process signal)
        (when (memq (process-status process) '(exit signal))
          (with-current-buffer ,tmp-buf
            (goto-char (point-min))
            (let (lines)
	            (while (not (eobp))
	              (setq lines (cons (buffer-substring-no-properties
			                             (line-beginning-position)
			                             (line-end-position)) lines))
	              (forward-line 1))
              (kill-buffer ,tmp-buf)
              (funcall ,callback (nreverse lines)))))))))

;;; golang.el ends here
