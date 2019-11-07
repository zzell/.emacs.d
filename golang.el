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









;; (let ((proc (start-process-shell-command "proc" "test" "sleep 2 && ls")))
;;   (set-process-filter proc '(lambda (x y) (message "%S:%s" x y)))
;;   )

;; (defun lsp--info (format &rest args)
;;   "Display lsp info message with FORMAT with ARGS."
;;   (message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

;; (lsp--info "%S" "yet")



(defun process-shell-command-stdout-list (cmd callback)
  "Run CMD asynchronously and call CALLBACK on exit.
CALLBACK is filter function that receives one parameter
- list of lines from CMD stdout."
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
              (funcall ,callback (nreverse lines))
              ))
          )
        ))))



;; think about it
(defvar *gocustom-cached-imports* (list))
(defvar *gocustom-gopath* (shell-command-to-string "printf $GOPATH"))
(defvar *gocustom-project-root*)

(setq go-packages-function 'gocustom-packages)

(defun gocustom-packages ()
  "Blank."
  *gocustom-cached-imports*)

(defun gocustom--append-to-cache (imports)
  "IMPORTS."
  (setq *gocustom-cached-imports* (append *gocustom-cached-imports* imports)))

(message "%S" (length *gocustom-cached-imports*))

(defun gocustom-refresh-imports ()
  ;; should be called when open new file and running manually in different way
  "Blank."
  (interactive)
  (let ((root (gocustom--find-package-root)))
    (setq *gocustom-cached-imports* (list))
    (if root (progn
               (gocustom--load-native-imports
                `(lambda (imports)
                   (gocustom--append-to-cache imports)
                   (gocustom--load-package-imports
                    ,root '(lambda (imports)
                             (gocustom--append-to-cache imports)
                             (gocustom--load-vendored-imports
                              ,root '(lambda (imports) (gocustom--append-to-cache imports))))))))
      (gocustom--load-all-imports '(gocustom--append-to-cache imports)))))

(defun gocustom--find-package-root ()
  "Return projects root in form 'github.com/user/project' or nil if project is not in GOPATH."
  (let ((gopath-src (concat *gocustom-gopath* "/src"))
        (pwd (shell-command-to-string "pwd | tr -d '\n'")))
    (when (string-match-p gopath-src pwd)
      (let ((elems (split-string (substring pwd (1+ (length gopath-src)) nil) "/")))
        (when (>= (length elems) 3)
          (concat (nth 0 elems) "/" (nth 1 elems) "/" (nth 2 elems)))))))

(defun gocustom--load-native-imports (callback)
  "CALLBACK."
  (process-shell-command-stdout-list  "go list -e ..." callback))

(defun gocustom--load-package-imports (root callback)
  "Asynchronously load local imports per package. ROOT - package root path.
CALLBACK - filter function that receives list of packages."
  (process-shell-command-stdout-list (format "go list -e %S/..." root) callback))

(defun gocustom--load-vendored-imports (root callback)
  "Asynchronously load vendored imports (e.g. when using Glide).
ROOT - package root path. CALLBACK - filter function that receives list of packages."
  (let* ((absolute-path (concat *gocustom-gopath* "/src/" root))
         (vendor-path (shell-command-to-string (format "find %S -type d -name 'vendor' -prune | tr -d '\n'" absolute-path))))
    (unless (string= vendor-path "")
      (process-shell-command-stdout-list
       (format "go list -e %S/..." (replace-regexp-in-string "^.*?\/src\/" "" vendor-path))
       `(lambda (imports)
          (let ((unprefix (gocustom--unprefix imports)))
            (funcall ,callback unprefix)))))))

(defun gocustom--load-all-imports (callback)
  "CALLBACK."
  (process-shell-command-stdout-list
   "go list -e all"
   `(lambda (imports)
      (let ((unprefix (gocustom--unprefix imports)))
        (funcall ,callback unprefix)))))

(defun gocustom--unprefix (imports)
  "IMPORTS."
  (let ((unprefix (list)))
    (dolist (i imports) (push (replace-regexp-in-string "^.*?\/vendor\/" "" i) unprefix)) unprefix))

;;; golang.el ends here
