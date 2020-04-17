;;; package --- Summary
;;; Commentary:
;;; Code:

;; load path first
(use-package exec-path-from-shell
	:config
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(let ((tools '(("gopls"         . "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
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
  ;; (add-hook 'lsp-after-open-hook
  ;;           '(lambda ()
  ;;              (when (eq major-mode 'go-mode)
	;; 							 (flycheck-golangci-lint-setup)
	;; 							 (flycheck-add-next-checker 'golangci-lint 'lsp)
	;; 							 ;; (flycheck-define-checker)
	;; 							 ;; (add-to-list 'flycheck-checkers 'golangci-lint)
	;; 							 ;; (flycheck-add-next-checker 'lsp '(t . golangci-lint))
	;; 							 )))
	)

(use-package go-fill-struct)
(use-package go-eldoc)
(use-package go-tag)
(use-package go-playground)
(use-package go-stacktracer)
;; (use-package flycheck-golangci-lint
;;   :config
;;   (setq flycheck-golangci-lint-enable-all t)
;;   (setq flycheck-golangci-lint-fast t)
;;   (setq flycheck-golangci-lint-disable-linters '("lll" "dupl" "maligned"
;;                                                  "prealloc" "gochecknoglobals" "gosec"
;;                                                  "funlen" "wsl" "gocognit")))

;; (setq go-packages-function 'custom/go-packages-function)
;; (add-hook 'go-mode-hook `(lambda () (custom/go-refresh-packages t)))

;; (defvar custom/go-packages-cache (list))

;; (defun custom/go-packages-function ()
;;   "Return packages list for 'go-packages-function'."
;;   custom/go-packages-cache)

;; (defun custom/go-refresh-packages (i)
;;   "Refresh cached packages. I used to determine if fn called interactively.
;; This function should be called on 'go-mode-hook'.
;; It finds native and vendored imports asynchronously.
;; It does nothing if 'custom/go-packages-cache' isn't empty, unless called interactively."
;;   (interactive "i")

;;   (when (or (not i) (= (length custom/go-packages-cache) 0))
;;     (setq custom/go-packages-cache (list))
;;     (let ((add-to-cache
;;            '(lambda (packages)
;;               (setq custom/go-packages-cache
;;                     (sort
;;                      (copy-sequence (append custom/go-packages-cache (delete-dups packages)))
;;                      #'string-lessp)))))

;;       (custom/go-find-native-packages add-to-cache)
;;       (custom/go-find-vendored-packages add-to-cache))))

;; (defun custom/go-find-native-packages (callback)
;;   "Asynchronously find native packages. CALLBACK - function that receives list of packages."
;;   (process-shell-command-stdout-list  "(cd ~ && go list -e ...)" callback))

;; (defun custom/go-find-vendored-packages (callback)
;;   "Asynchronously find vendored packages. CALLBACK - function that receives list of packages."
;;   (let ((pkg (custom/go-find-current-package)))
;;     (when pkg
;;       (let* ((absolute-path (concat (getenv "GOPATH") "/src/" pkg))
;;              (vendor-path (shell-command-to-string (format "find %S -type d -name 'vendor' -prune | tr -d '\n'" absolute-path))))
;;         (unless (string= vendor-path "")
;;           (process-shell-command-stdout-list
;;            (format "go list -e %S/..." (replace-regexp-in-string "^.*?\/src\/" "" vendor-path))
;;            `(lambda (packages)
;;               (let ((unprefix (list)))
;;                 (dolist (p packages) (push (replace-regexp-in-string "^.*?\/vendor\/" "" p) unprefix))
;;                 (,callback unprefix)))))))))

;; (defun custom/go-find-current-package ()
;;   "Return current package."
;;   (let ((gosrc (concat (getenv "GOPATH") "/src/")))
;;     (when (string-match-p gosrc default-directory)
;;       (let ((elems (split-string (substring default-directory (length gosrc) nil) "/")))
;;         (when (>= (length elems) 3)
;;           (concat (nth 0 elems) "/" (nth 1 elems) "/" (nth 2 elems)))))))

;; (defun process-shell-command-stdout-list (cmd callback)
;;   "Run CMD asynchronously and exec CALLBACK on exit passing stdout as list of lines."
;;   (let ((tmp-buf (generate-new-buffer "*loading*")))
;;     (set-process-sentinel
;;      (start-process (number-to-string (random)) tmp-buf shell-file-name shell-command-switch cmd)
;;      `(lambda (process signal)
;;         (when (memq (process-status process) '(exit signal))
;;           (with-current-buffer ,tmp-buf
;;             (goto-char (point-min))
;;             (let (lines)
;; 	            (while (not (eobp))
;; 	              (setq lines (cons (buffer-substring-no-properties
;; 			                             (line-beginning-position)
;; 			                             (line-end-position)) lines))
;; 	              (forward-line 1))
;;               (kill-buffer ,tmp-buf)
;;               (funcall ,callback (nreverse lines)))))))))

;;; golang.el ends here
