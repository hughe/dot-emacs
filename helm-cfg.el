(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; This Emacs is Powered by `HELM' using\n;; emacs program \"emacs\".\n;; This is a minimal `helm' configuration to discover `helm' or debug it.\n;; You can retrieve this minimal configuration in \"/tmp/helm-cfg.el\".\n;;
;; Some original Emacs commands are replaced by their `helm' counterparts:\n\n;; - `find-file'(C-x C-f)            =>`helm-find-files'\n;; - `occur'(M-s o)                  =>`helm-occur'\n;; - `list-buffers'(C-x C-b)         =>`helm-buffers-list'\n;; - `completion-at-point'(M-tab)    =>`helm-lisp-completion-at-point'[1]\n;; - `apropos-command'(C-h a)        =>`helm-apropos'\n;; - `dabbrev-expand'(M-/)           =>`helm-dabbrev'\n;; - `execute-extended-command'(M-x) =>`helm-M-x'\n\n
;; Some other Emacs commands are \"helmized\" by `helm-mode'.\n;; [1] Coming with emacs-24.4, `completion-at-point' is \"helmized\" by `helm-mode'\n
;; which provides Helm completion in many places like `shell-mode'.\n;; Find context help for most Helm commands with `C-h m'.\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))

(setq load-path (quote ))

(defvar default-package-manager nil)
;; /home/you/.emacs.d/.local/straight/build-27.1/helm
(defvar initial-package-directory (file-name-directory (file-truename "./elpa/helm-20240121.728/emacs-helm.sh")))

(defvar bootstrap-version)
(let* ((packages "")
       (pkg-list (and packages
                      (not (equal packages ""))
                      (split-string packages ",")))
       ;; /home/you/.emacs.d/.local/straight/build-27.1
       (straight-path (file-name-directory (directory-file-name initial-package-directory)))
       ;; /home/you/.emacs.d/.local/straight/build-27.1/async
       (async-path (expand-file-name "async" straight-path))
       ;; /home/you/.emacs.d/.local/straight/repos/straight.el/bootstrap.el
       (bootstrap-file
        (expand-file-name "repos/straight.el/bootstrap.el"
                          (file-name-directory (directory-file-name straight-path))))
       (bootstrap-version 5))
  (when (file-exists-p bootstrap-file)
    (setq default-package-manager 'straight)
    (load bootstrap-file nil 'nomessage)
    (add-to-list 'load-path async-path)
    (when pkg-list
      (dolist (pkg pkg-list)
        (let* ((pkg-path (expand-file-name pkg straight-path))
               (autoload-file (expand-file-name
                               (format "%s-autoloads.el" pkg)
                               pkg-path)))
          (add-to-list 'load-path pkg-path)
          (if (file-exists-p autoload-file)
              (load autoload-file nil 'nomessage)
            (straight-use-package (intern pkg))))))))

(unless (eq default-package-manager 'straight)
  (require 'package)
  ;; User may be using a non standard `package-user-dir'.
  ;; Modify `package-directory-list' instead of `package-user-dir'
  ;; in case the user starts Helm from a non-ELPA installation.
  (unless (file-equal-p package-user-dir (locate-user-emacs-file "elpa"))
    ;; Something like  /home/you/.emacs.d/somedir/else/elpa/
    ;; starting from default-directory is wrong in case helm.sh is a symlink
    ;; or e.g. helm --chdir foo have been used.
    (add-to-list 'package-directory-list (directory-file-name
                                          (file-name-directory
                                           (directory-file-name initial-package-directory)))))

  (let* ((str-lst "")
         (load-packages (and str-lst
                             (not (string= str-lst ""))
                             (split-string str-lst ","))))
    (setq package-load-list
          (if (equal load-packages '("all"))
              '(all)
            (append '((helm-core t) (helm t) (async t) (popup t) (wfnames t))
                    (mapcar (lambda (p) (list (intern p) t)) load-packages)))))

  (package-initialize))

(add-to-list 'load-path initial-package-directory)

(unless (> -1 0)
   (setq default-frame-alist '((vertical-scroll-bars . nil)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (fullscreen . nil))))
(blink-cursor-mode -1)
(load "helm-autoloads" nil t)
(helm-mode 1)
(with-eval-after-load 'tramp-cache (setq tramp-cache-read-persistent-data t))
(with-eval-after-load 'auth-source (setq auth-source-save-behavior nil))
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "/tmp/helm-cfg.el") (delete-file "/tmp/helm-cfg.el"))))
