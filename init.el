;; Packages:
;;
;; If this emacs does not seem to have an uptodate list of packages,
;; run package-install-selectqed-packages see the
;; ‘package-selected-packages’ variable which is saved by customize
;; (search for it below).\

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/") t)

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
    '("melpa" .
      "http://melpa.org/packages/"))
(package-initialize)

;; Load environment vars from the the shell (extra vars are loaded
;; using exec-path-from-shell-copy-env)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(defvar he-is-macos (not (eq nil (string-match "apple" (version)))))
(defvar he-is-aquamacs (not (eq nil (string-match "Aquamacs" (version)))))
(defvar he-is-windows (not (eq nil (string-match "mingw" (version)))))

;; Bind C-z to undo.  
(when window-system
  ;; need to bind this only when we have a window system.
  (global-set-key [(control ?z)] 'undo)
  ;; These will only make sense if there is a window system I think, I
  ;; don't think a terminal can produce C-.
  (global-set-key [(control ?.)] 'fixup-whitespace)
  (global-set-key [(control ?,)] 'join-line))

;; In Aquamacs, Home and End are bound in the special osx-key-mode-map
;; map to {beginning,end}-of-buffer, which is apparently mor Mac-like.
;; I can't get the hang of this.
(when he-is-aquamacs
  (define-key osx-key-mode-map [home] 'move-beginning-of-line)
  (define-key osx-key-mode-map [end] 'move-end-of-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure desktop mode
;; Enable desktop mode (restores your buffers when you start emacs).
(desktop-save-mode 1)

; Restore the first ten buffers immediately, then restore the rest
; when idle.
(setq desktop-restore-eager 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert TODOs

(require 's) ; The s string maniulation library https://github.com/magnars/s.el

(defvar he-insert-todo-who-history nil)
(defvar he-insert-todo-project-history nil)

(defun he-insert-todo (who project)
  (interactive
   (list
    (read-from-minibuffer "Who: " (car-safe he-insert-todo-who-history) nil nil 'he-insert-todo-who-history)
    (read-from-minibuffer "Project: " nil  nil nil 'he-insert-todo-project-history)))

  (let ((who-empty (string-equal "" who))
	(proj-empty (string-equal "" project)))
    (insert
     (concat comment-start
	     (if (s-suffix? " " comment-start) "" " ")
	     "TODO: "
	     (cond ((and (not who-empty) (not proj-empty))
		    (format "(%s, %s) " who project))
		   ((not who-empty)
		    (format "(%s) " who))
		   ((not proj-empty)
		    (format "(%s) " project))
		   (t
		    ""))
	     ))))

;; Bind it to C-x t
(global-set-key [(control x) ?t] 'he-insert-todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-mode

(exec-path-from-shell-copy-envs '("PATH" "GOPATH" "GOROOT" "PKG_CONFIG_PATH" "GVM_ROOT"))


(setq gofmt-command "goimports") ;; goreturns
(require 'go-mode)

(require 'flycheck)

(add-hook 'go-mode-hook
	  (lambda ()
	    (flycheck-mode t)))

(require 'compile)

(add-hook 'go-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (control c)] 'compile) 
	    (local-set-key [(control c) (\#)] 'comment-region)
	    (local-set-key [(control c) ?i] 'go-goto-imports)
	    (local-set-key [(control c) (control f)] 'gofmt)
	    (local-set-key [(control c) ?`] 'flycheck-next-error)
	    (local-set-key [(control c) (control t)] 'he-godep-test)

	    (setq tab-width 4)

	    (setq case-fold-search nil)
	    ))


(add-to-list 'compilation-error-regexp-alist-alist
	     '(go-stacktrace . ("^\t+\\(/.*\\.go\\):\\([0-9]+\\)$" 1 2)))

(add-to-list 'compilation-error-regexp-alist 'go-stacktrace)

(defun he-move-to-front (elt lis)
  (if (not (equal elt (car lis)))
      (cons elt	(delete elt lis))
    lis
    ))

(add-hook 'go-mode-hook
	  (lambda ()
	    ;; For the go-test regexps to work, they must be at the
	    ;; front of the list, otherwise the wong regexp matches and
	    ;; next-error fails.
	    (setq compilation-error-regexp-alist (he-move-to-front 'go-test compilation-error-regexp-alist))
	    ))

(add-hook 'go-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    ))

(defvar he-godep-test-cmd "go test -v -short -dev_no_log")

(defun he-godep-test ()
  (interactive)
  (compile he-godep-test-cmd))


;; Rebind C-c C-j to call godef-jump-other-window if there is a prefix
;; arg, and plain godef-jump if there is no prefix arg.  E.g., C-u C-c
;; C-j jumps to the def in the other window.
(defun he-godef-jump (point prefix)
  (interactive "d\nP")
  (if prefix
      (godef-jump-other-window point)
    (godef-jump point)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (control j)] 'he-godef-jump)
	    ))
		      
;; go-guru

(require 'go-guru)

; gvmrun guru so that it picks up the gvm config.
;(setq go-guru-command "gvmrun guru")

(add-hook 'go-mode-hook
	  (lambda ()
	    (go-guru-hl-identifier-mode)))

(defun he-go-grep (expression)
  "Grep the .go files in the current directory for regular EXPRESSION"
  (interactive "sExpression: ")
  (grep (concat grep-command "\'" expression "\' *.go")))

;; Load go-dlv for debugging go with gud
(require 'go-dlv)

;;;;;;;;;;;;;;;;;;;;
;; gometalinter

(require 'flycheck-gometalinter)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
(setq flycheck-gometalinter-vendor t)
;; only show errors
(setq flycheck-gometalinter-errors-only t)
;; only run fast linters
(setq flycheck-gometalinter-fast t)
;; use in tests files
(setq flycheck-gometalinter-test t)
;; disable linters
(setq flycheck-gometalinter-disable-linters '("gocyclo" "gotype"))  ;; gotype does not seem very mature
;; Only enable selected linters
;(setq flycheck-gometalinter-disable-all t)
;(setq flycheck-gometalinter-enable-linters '("golint"))
;; Set different deadline (default: 5s)
(setq flycheck-gometalinter-deadline "10s")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revert-all-buffers from http://www.emacswiki.org/emacs/RevertBuffer#toc4

(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode from https://github.com/yoshiki/yaml-mode

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/yaml-mode") t)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; (setq compilation-error-regexp-alist-alist
;;       (append '((pylint "^E:\\([0-9]+\\)," nil 1)) compilation-error-regexp-alist-alist))
;; (setq compilation-error-regexp-alist (append '(pylint) compilation-error-regexp-alist))

;; (setq compilation-error-regexp-alist-alist
;;       (assq-delete-all 'pylint  compilation-error-regexp-alist-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiling windows

(require 'windmove)
(require 'winner)
(require 'buffer-move)
(require 'tiling)

;; Split & Resize
(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x _") 'split-window-vertically)
(define-key global-map (kbd "C-{") 'shrink-window-horizontally)
(define-key global-map (kbd "C-}") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-^") 'enlarge-window)
;; Navgating: Windmove uses C-<up> etc.
(define-key global-map (kbd "C-<up>"   ) 'windmove-up)
(define-key global-map (kbd "C-<down>" ) 'windmove-down)
(define-key global-map (kbd "C-<right>") 'windmove-right)
(define-key global-map (kbd "C-<left>" ) 'windmove-left)
;; Swap buffers: M-<up> etc.
(define-key global-map (kbd "M-<up>"   ) 'buf-move-up)
(define-key global-map (kbd "M-<down>" ) 'buf-move-down)
(define-key global-map (kbd "M-<right>") 'buf-move-right)
(define-key global-map (kbd "M-<left>" ) 'buf-move-left)
;; Tile
(define-key global-map (kbd "C-\\") 'tiling-cycle) ; accepts prefix number
(define-key global-map (kbd "C-M-<up>") 'tiling-tile-up)
(define-key global-map (kbd "C-M-<down>") 'tiling-tile-down)
(define-key global-map (kbd "C-M-<right>") 'tiling-tile-right)
(define-key global-map (kbd "C-M-<left>") 'tiling-tile-left)
;; ;; Another type of representation of same keys, in case your terminal doesn't
;; ;; recognize above key-binding. Tip: C-h k C-up etc. to see into what your
;; ;; terminal tranlated the key sequence.
;; (define-key global-map (kbd "M-[ a"     ) 'windmove-up)
;; (define-key global-map (kbd "M-[ b"     ) 'windmove-down)
;; (define-key global-map (kbd "M-[ c"     ) 'windmove-right)
;; (define-key global-map (kbd "M-[ d"     ) 'windmove-left)
;; (define-key global-map (kbd "ESC <up>"   ) 'buf-move-up)
;; (define-key global-map (kbd "ESC <down>" ) 'buf-move-down)
;; (define-key global-map (kbd "ESC <right>") 'buf-move-right)
;; (define-key global-map (kbd "ESC <left>" ) 'buf-move-left)
;; (define-key global-map (kbd "ESC M-[ a" ) 'tiling-tile-up)
;; (define-key global-map (kbd "ESC M-[ b" ) 'tiling-tile-down)
;; (define-key global-map (kbd "ESC M-[ c" ) 'tiling-tile-right)
;; (define-key global-map (kbd "ESC M-[ d" ) 'tiling-tile-left)

;; Turn on winner-mode  "C-c <-" undoes the last window config change, "C-c ->" redoes it.
(winner-mode)

(defun set-window-width (n) 
  (interactive "nWidth: ")
  (let ((cur-width (window-width)))
    (if (< n cur-width)
	(shrink-window-horizontally (- cur-width n))
      (enlarge-window-horizontally (- n cur-width)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ding when a long compilation finishes.

(defvar he-compilation-start-time nil)
(make-variable-buffer-local 'he-compilation-start-time)
  
(add-hook 'compilation-start-hook
	  (lambda (proc) 
	    (setq he-compilation-start-time (float-time))))

(defun he-beep-on-compilation-finish (buf msg) 
  (with-current-buffer buf
    (if (and he-compilation-start-time 
	     (>= (- (float-time) he-compilation-start-time) 5))
	(progn
	  (ding t)
	  (run-with-timer 0.5 nil 'ding t)
	  (run-with-timer 1.0 nil 'ding t)))))

;; Beep three times when compilation finishes
(add-hook 'compilation-finish-functions 'he-beep-on-compilation-finish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protobuf.

(require 'protobuf-mode)

(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . t)))

(add-hook 'protobuf-mode-hook
	  
  (lambda () 
    (c-add-style "my-style" my-protobuf-style t)
    (local-set-key [(control c) (control c)] 'compile)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile mode

(add-hook 'makefile-mode-hook
  (lambda () 
    (local-set-key [(control c) (control c)] 'compile)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Mode
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/web-mode")  ;; Loaded from package now.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(add-hook 'web-mode-hook  
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2) ;; HTML ish stuf
	    (setq web-mode-css-indent-offset 4)
	    (setq web-mode-code-indent-offset 4)  ;; JS, PHP, ...

	    (local-set-key [(control c) ?/] 'web-mode-element-close)
	    ))

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'go-autocomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy mode

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; swiper doesn't seem to work with this ivy for some reason.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit

(require 'magit)


(define-key global-map (kbd "C-x g") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'magithub) ; GitHub integration


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsx mode - Doesn't indent or do anything useful.
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;(autoload 'jsx-mode "jsx-mode" "JSX mode" t)


;; Attempt to improve the performance of compilations 
(setq process-adaptive-read-buffering nil)

;; occur-x.el adds some extra functionality to occur-mode.  It allows
;; the user to refine any occur mode with extra regexp based filters.
(require 'occur-x)
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dockerfile mode
(require 'dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs (quote y-or-n-p))
 '(flycheck-disabled-checkers (quote (go-vet)))
 '(flycheck-gometalinter-fast t)
 '(magit-diff-refine-hunk t)
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (zones lsp-ui lsp-ivy lsp-mode buffer-move yaml-mode swiper ivy magit magithub projectile go-mode go-dlv dockerfile-mode exec-path-from-shell neotree sr-speedbar ghub)))
 '(speedbar-show-unknown-files t)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(require 'projectile)

(projectile-global-mode +1)


