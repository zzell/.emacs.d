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









(let ((proc (start-process-shell-command "proc" "test" "sleep 2 && ls")))
  (set-process-filter proc '(lambda (x y) (message "%S:%s" x y)))
  )

;; (defun lsp--info (format &rest args)
;;   "Display lsp info message with FORMAT with ARGS."
;;   (message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

;; (lsp--info "%S" "yet")



;; (setq go-packages-function 'blb)
;; (defun blb ()
;;   "Blank."
;;   *custom-go-cached-imports*)

;; (defvar *custom-go-cached-imports* (list))
;; (defvar *custom-go-gopath* (shell-command-to-string "printf $GOPATH"))

;; (let ((root (custom-go-find-project-root)))
;;   (if root
;;       (progn
;;         (setq *custom-go-cached-imports* (list))
;;         (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (go-packages-native)))
;;         (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-local-imports-list root)))
;;         (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-vendored-imports-list root))))
;;     (setq *custom-go-cached-imports* (list))
;;     (setq *custom-go-cached-imports* (append *custom-go-cached-imports* (custom-go-all-imports)))))


(defvar *gocustom-cached-imports* (list))
(defvar *gocustom-gopath* (shell-command-to-string "printf $GOPATH"))
(defvar *gocustom-project-root*)

(defun gocustom--append-to-cache (imports)
  "IMPORTS."
  (setq *gocustom-cached-imports* (append *gocustom-cached-imports* imports))
  )

(defun custom-go-refresh-imports ()
  "Blank."
  (interactive)

  (let ((root (gocustom--find-package-root)))
    (if root (progn
               (setq *gocustom-cached-imports* (list))
               (gocustom--load-local-imports
                "github.com/ContinuumLLC/platform-brightgauge-plugin"
                '(lambda (imports)
                   (gocustom--append-to-cache imports)
                   (message "%S" "loaded local imports")))
               )

      )
    ))

;; (gocustom--load-vendored-imports "github.com/ContinuumLLC/platform-brightgauge-plugin" '(lambda (im) (message "%S" im)))

(defun gocustom--find-package-root ()
  "Return projects root in form 'github.com/user/project' or nil if project is not in GOPATH."
  (let ((gopath-src (concat *gocustom-gopath* "/src"))
        (pwd (shell-command-to-string "pwd | tr -d '\n'")))
    (when (string-match-p gopath-src pwd)
      (let ((elems (split-string (substring pwd (1+ (length gopath-src)) nil) "/")))
        (when (>= (length elems) 3)
          (concat (nth 0 elems) "/" (nth 1 elems) "/" (nth 2 elems)))))))

(defun gocustom--load-local-imports (root filter)
  "Asynchronously load local imports per package. ROOT - package root path.
FILTER - callback function that receives list of packages."
  (process-shell-command-stdout (format "go list -e %S/..." root) filter))

(defun gocustom--load-vendored-imports (root filter)
  ;; "May be slow. Load vendored imports (e.g. when using Glide). ROOT - project root."

  (let* ((absolute-path (concat *gocustom-gopath* "/src/" root))
         (vendor-path (shell-command-to-string (format "find %S -type d -name 'vendor' | tr -d '\n'" absolute-path))))
    (unless (string= vendor-path "")
      (process-shell-command-stdout
       (format "go list -e %S/..." (replace-regexp-in-string "^.*?\/src\/" "" vendor-path))
       `(lambda (imports)
          (let ((unprefix (list)))
            (dolist (i imports) (push (replace-regexp-in-string "^.*?\/vendor\/" "" i) unprefix))
            (funcall ,filter unprefix)))))))

(async-shell-command-to-string "go list -e github.com/ContinuumLLC/platform-brightgauge-plugin/src/vendor/..." '(lambda (out) (message "%S" out)))

;; (message "%S" (process-lines "go" "list" "-e" "github.com/ContinuumLLC/platform-brightgauge-plugin/src/vendor/..."))

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (let*
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     `(lambda (process signal)
        (when (memq (process-status process) '(exit signal))
          (with-current-buffer output-buffer
            (let ((output-string
                   (buffer-substring-no-properties
                    (point-min)
                    (point-max))))
              (funcall callback-fun output-string)))
          (kill-buffer output-buffer))))
    output-buffer))

;; (defun process-shell-command-stdout (cmd filter)
;;   "Run CMD asynchronously and call FILTER on exit.
;; FILTER is callback function that receives one parameter
;; - list of lines from CMD stdout."

;;   (let ((proc (start-process-shell-command (number-to-string (random)) nil cmd)))



;;     (set-process-filter
;;      proc
;;      `(lambda (_ stdout)
;;         (with-temp-buffer
;;           (insert stdout)
;;           (goto-char (point-min))
;;           (let (lines)
;; 	          (while (not (eobp))
;; 	            (setq lines (cons (buffer-substring-no-properties
;; 			                           (line-beginning-position)
;; 			                           (line-end-position)) lines))
;; 	            (forward-line 1))
;;             (funcall ,filter (nreverse lines))))))

;;     )

;;   )


;; (defun process-shell-command-stdout (cmd filter)
;;   "Run CMD asynchronously and call FILTER on exit.
;; FILTER is callback function that receives one parameter
;; - list of lines from CMD stdout."
;;   (set-process-filter
;;    (start-process-shell-command (number-to-string (random)) nil cmd)
;;    `(lambda (_ stdout)
;;       (with-temp-buffer
;;         (insert stdout)
;;         (goto-char (point-min))
;;         (let (lines)
;; 	        (while (not (eobp))
;; 	          (setq lines (cons (buffer-substring-no-properties
;; 			                         (line-beginning-position)
;; 			                         (line-end-position)) lines))
;; 	          (forward-line 1))
;;           (funcall ,filter (nreverse lines)))))))

(defun gocustom--load-all-imports ()
  "Extremely slow."
  (process-lines "go" "list" "-e" "all"))

;;; golang.el ends here
