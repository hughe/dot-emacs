;; TODO:
;;
;; * try removing exec path from shell



;; Packages:
;;
;; If this emacs does not seem to have an uptodate list of packages,
;; run `he-install-my-packages`.
;;
;; When we install a new package, add it to the he-package-list.



(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/") t)

(require 'package)
(add-to-list 'package-archives 
    '("melpa" .
      "http://melpa.org/packages/"))
(package-initialize)

(setq he-package-list
      '(
	;; Sort this
	auto-complete
	buffer-move
	cmake-mode
	dockerfile-mode
	exec-path-from-shell
	flycheck
;	ghub
	go-autocomplete
	go-dlv
	go-mode
;	lsp-mode
;	lsp-ui
	lua-mode
	magit
	neotree
	occur-x
	projectile
	protobuf-mode
	sr-speedbar
	swiper
	web-mode
	yaml-mode
	yasnippet
	zones
	ivy
;	lsp-ivy
	))

(defun he-install-my-packages ()
  (interactive)
  (mapc (lambda (p)
	  (message "Installing package: %s" p)
	  (package-install p))
       he-package-list))


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
  ;;
  ;; TODO: stop doing this someday and use splat-z (which we can remap
  ;; to C-_ in iTerm2)
  (global-set-key [(control ?z)] 'undo)
  ;; These will only make sense if there is a window system I think, I
  ;; don't think a terminal can produce C-.
  (global-set-key [(control ?.)] 'fixup-whitespace)
  (global-set-key [(control ?,)] 'join-line)

  (global-unset-key (kbd "C-<triple-wheel-down>"))
  (global-unset-key (kbd "C-<double-wheel-down>"))
  (global-unset-key (kbd "C-<wheel-down>"))

  (global-unset-key (kbd "C-<triple-wheel-up>"))
  (global-unset-key (kbd "C-<double-wheel-up>"))
  (global-unset-key (kbd "C-<wheel-up>"))

  )

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

;; Load midnight to that we can use clean-buffer-list.
(require 'midnight)

(add-hook 'desktop-save-mode
	  (lambda ()
	    ;; Clean the buffer list
	    ;; https://www.emacswiki.org/emacs/CleanBufferList before
	    ;; saving it.
	    (clean-buffer-list))) 

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

;; Copy go envrionment from shell

(defun he-copy-go-env-from-shell ()
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH" "GOROOT" "PKG_CONFIG_PATH" "GVM_ROOT")))

(he-copy-go-env-from-shell)

(defun he-gvm-change-env (ver pkgset)
  "Set the GVM environment for Go version VER and package set
PKGSET"
  (interactive "MVersion: \nMPkgset: \n")
  (message "Version: %s, Pkgset: %s" ver pkgset)
  (setenv "HUGH_GVM_DEFAULT_GO" ver)
  (setenv "HUGH_GVM_DEFAULT_PKG" pkgset)
  (he-copy-go-env-from-shell))


(setq gofmt-command "goimports") ;; goreturns
(require 'go-mode)

(require 'flycheck)

(add-hook 'go-mode-hook
	  (lambda ()
	    (flycheck-mode t)))

(eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

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

(defun he-go-grep (expression)
  "Grep the .go files in the current directory for regular EXPRESSION"
  (interactive "sExpression: ")
  (grep (concat grep-command "\'" expression "\' *.go")))

;; Load go-dlv for debugging go with gud
(require 'go-dlv)

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
;; YASnippet
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

;;(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ;; Use web mode for html.
(setq web-mode-enable-engine-detection t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete mode
;; M-tab
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'go-autocomplete)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Ivy mode

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; swiper doesn't seem to work with this ivy for some reason.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

;; (use-package helm)

;; (helm-mode 1)

;; ;; Pinched from ./helm-cfg.el which we don't load.
;; (with-eval-after-load 'tramp-cache (setq tramp-cache-read-persistent-data t))
;; (with-eval-after-load 'auth-source (setq auth-source-save-behavior nil))
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)
;; (define-key global-map [remap apropos-command] 'helm-apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit

(require 'magit)


(define-key global-map (kbd "C-x g") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

;; Attempt to improve the performance of compilations 
(setq process-adaptive-read-buffering nil)

;; occur-x.el adds some extra functionality to occur-mode.  It allows
;; the user to refine any occur mode with extra regexp based filters.
(require 'occur-x)
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(require 'projectile)

(projectile-mode) ;; enable projectile mode

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua mode
(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(setq lua-default-application (expand-file-name "~/src/lua/lua-docker"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cc-modes

(c-add-style "sqlite"
	     '((c-basic-offset . 2)	; Guessed value
	       (c-offsets-alist
		(arglist-cont . 0)	; Guessed value
		(arglist-intro . +)	; Guessed value
		(block-close . 0)	; Guessed value
		(brace-entry-open . 0)	; Guessed value
		(brace-list-close . 0)	; Guessed value
		(brace-list-entry . 0)	; Guessed value
		(brace-list-intro . +)	; Guessed value
		(case-label . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(cpp-macro-cont . 8)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(inclass . +)		; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(statement-case-intro . +) ; Guessed value
		(statement-cont . -)	; Guessed value
		(substatement . -)	; Guessed value
		(topmost-intro . 0)	; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-open . 0)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(catch-clause . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(defun-open . 0)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . 0)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . 0)
		(inline-close . 0)
		(inline-open . +)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 2)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-open . 0)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement-label . 2)
		(substatement-open . +)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))

(c-add-style "sldb-c++"
	     '("stroustrup"
	       (c-basic-offset . 2)
	       (c-offsets-alist
		(innamespace . [0]))
	       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colorize compilation buffer.
;;
;; From: https://emacs.stackexchange.com/a/38531

;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output () ;; (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'compilation-filter-hook ;;'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANTLR mode (is built in according to package-list-packages)

(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

(add-hook 'antlr-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (control c)] 'compile)
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile mode

(add-hook 'makefile-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (control c)] 'compile)
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode

(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (control c)] 'compile)
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'sql)

(defvar he-sql-sldb-program (expand-file-name "~/src/sldb/scripts/run_sldb_rs_shell_minio.sh"))


;; Add SLDB as a product (taken from the top of sql.el.gz).

;; 1) Add the product to the list of known products.

(sql-add-product 'sldb "SLDB"
     	         '(:free-software t))

;; 2) Define font lock settings.  Same as SQLite.

(sql-set-product-feature 'sldb
                         :font-lock
                         'sql-mode-sqlite-font-lock-keywords)

;; 3) Define any special syntax characters including comments and
;;    identifier characters.

;; None

;; 4) Define the interactive command interpreter for the database
;;    product.

(defcustom he-sql-sldb-program (expand-file-name "~/src/sldb/build/sldb_shell")
  "Command to start sldb_shell."
  :type 'file
  :group 'SQL)

(sql-set-product-feature 'sldb
                         :sqli-program 'he-sql-sldb-program)
(sql-set-product-feature 'sldb
                         :prompt-regexp "^sqlite> ")
(sql-set-product-feature 'sldb
                         :prompt-length 8)

;; 5) Define login parameters and command line formatting.

;;     (defcustom my-sql-xyz-login-params '(user password server database)
;;       "Login parameters to needed to connect to XyzDB."
;;       :type 'sql-login-params
;;       :group 'SQL)
;;
;;     (sql-set-product-feature 'sldb
;;                              :sqli-login 'my-sql-xyz-login-params)

;;     (defcustom my-sql-xyz-options '("-X" "-Y" "-Z")
;;       "List of additional options for `sql-xyz-program'."
;;       :type '(repeat string)
;;       :group 'SQL)
;;
;;     (sql-set-product-feature 'sldb
;;                              :sqli-options 'my-sql-xyz-options))

;;     (defun my-sql-comint-xyz (product options &optional buf-name)
;;       "Connect ti XyzDB in a comint buffer."
;;
;;         ;; Do something with `sql-user', `sql-password',
;;         ;; `sql-database', and `sql-server'.
;;         (let ((params
;;                (append
;;           (if (not (string= "" sql-user))
;;                     (list "-U" sql-user))
;;                 (if (not (string= "" sql-password))
;;                     (list "-P" sql-password))
;;                 (if (not (string= "" sql-database))
;;                     (list "-D" sql-database))
;;                 (if (not (string= "" sql-server))
;;                     (list "-S" sql-server))
;;                 options)))
;;           (sql-comint product params buf-name)))
;;
(sql-set-product-feature 'sldb
                         :sqli-comint-func 'sql-comint-sqlite)

;; 6) Define a convenience function to invoke the SQL interpreter.

(defun he-sql-sldb (&optional buffer)
  "Run sldb_shell as an inferior process."
  (interactive "P")
  (sql-product-interactive 'sldb buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cmake mode

(require 'cmake-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run gdb in docker.
;;
;; Important: the --annotate=1 flag must be on the command line.
;;

(defun sldb-gdb (progname)
  (interactive "fFile to debug: ")
  ;; Don't use -it it makes things worse.
  (let ((cmd (format "docker exec -i sldb-shell gdb --annotate=1 %s" progname)))
    (message "Running: %s" cmd)
    (gud-gdb cmd)))


(defun sldb-gdb-ex (command)
  (interactive "MCommand line (don't forget 'gdb --annotate=1'): ")
  ;; Don't use -it it makes things worse.
  (let ((cmd (format "docker exec -i sldb-shell %s" command)))
    (message "Running: %s" cmd)
    (gud-gdb cmd)))

(defun sldb-gdb-with-minio (progname)
  (interactive "fFile to debug: ")
  ;; Don't use -it it makes things worse.
  (let ((cmd (format "docker exec -i sldb-shell ./scripts/test_with_minio.sh gdb --annotate=1 %s" progname)))
    (message "Running: %s" cmd)
    (gud-gdb cmd)))

(defun sldb-gdb-test (progname)
  (interactive "fFile to debug: ")
  ;; Don't use -it it makes things worse.
  ;; TODO: change directory to SLDB root dir and run.  Think I might need a new buffer.
  ;; (projectile-project-root) returns the project root.
  (let ((cmd (format "./sldb_run -x ./scripts/test_with_minio.sh gdb --annotate=1 %s" progname))
	(gud-gdb-mode-hook (cons (lambda ()
				   (display "cd sldb")
				   (cd (expand-file-name "~/src/sldb")))
				 (if (boundp 'gud-gdb-mode-hook)
				     gud-gdb-mode-hook
				   '())
				 ))
	)
    (message "Running: %s" cmd)
    (gud-gdb cmd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for gdb

(add-hook 'gdb-mode-hook
	  (lambda ()
	    (gud-def he-kill-gdbserver "monitor exit" "y"
		     "Kill the gdbserver we are connected to.")
	    (gud-def he-connect-gdbserver "target extended-remote localhost:2222" "k"
		     "Connect to gdb server on localhost:2222.")
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust mode

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key [(control c) (\#)] 'comment-region)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    
	    ))

;; Add rust back traces to compilation-error-regexp-alist.
(add-to-list 'compilation-error-regexp-alist 'rust-backtrace)
(add-to-list 'compilation-error-regexp-alist-alist
             '(rust-backtrace
               "^ *at +\\(.+?\\):\\([0-9]+\\)" 
               1 2 nil nil))

;; TODO: move this to .dir-locals.el
;; TODO: moved it.  Leave it here until we know it works.
;; ;; Add these directories to the compilation-search-path.
;; (setq compilation-search-path '("/Users/hugh/src/sldb/"
;; 				"/Users/hugh/src/sldb/rs/"
;; 				"/Users/hugh/src/sldb/c/"))

(use-package rustic)

(require 'dap-gdb)

(dap-mode 1)

;; The modes below are optional

(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management. See
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager

(setq switch-to-buffer-in-dedicated-window 'pop)

(setq switch-to-buffer-obey-display-actions t)

(defun mp-split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

(defun mp-split-right (arg)
  "Split window right from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'right nil))

(defun mp-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
			  (not (window-dedicated-p (selected-window)))))

(add-to-list 'display-buffer-alist
   '("\\*Help\\*"
     (display-buffer-reuse-window display-buffer-pop-up-window)
     ))

;; Display the *compilation* buffer in a dedicated window at the
;; bottom of the frame.
(add-to-list 'display-buffer-alist
   '("\\*compilation\\*"
     (display-buffer-reuse-window display-buffer-at-bottom)
     (dedicated . t)
     (window-height . 0.25)
     ))

;; Display the main Magit buffer in the same window as it was invoked from.
(add-to-list 'display-buffer-alist
   '("^magit:.*"
     (display-buffer-reuse-window display-buffer-same-window)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel

(defun he-read-file-contents (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun he-anthropic-api-key ()
  (he-read-file-contents "~/.emacs.d/claude_key"))

(setq he-claude-standard (gptel-make-anthropic "Claude"
			   :stream t
			   :key #'he-anthropic-api-key))

(setq he-claude-reasoning (gptel-make-anthropic "Claude-Reasoning"
			    :key #'he-anthropic-api-key
			    :stream t
			    :models '(claude-3-7-sonnet-20250219)
			    :header (lambda () (when-let* ((key (gptel--get-api-key)))
						 `(("x-api-key" . ,key)
						   ("anthropic-version" . "2023-06-01")
						   ("anthropic-beta" . "pdfs-2024-09-25")
						   ("anthropic-beta" . "output-128k-2025-02-19")
						   ("anthropic-beta" . "prompt-caching-2024-07-31"))))
			    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
							:max_tokens 4096)))

(defun he-gemini-api-key ()
  (he-read-file-contents "~/.emacs.d/gemini_key"))


(setq he-gemini (gptel-make-gemini "Gemini"
		  :key #'he-gemini-api-key
		  :stream t
		  ))


(use-package gptel)
(setq gptel-model 'claude-sonnet-4-20250514)  ;; Was: claude-3-7-sonnet-20250219
(setq gptel-backend he-claude-standard)

(global-set-key [(control x) ?7 ?b] 'gptel)
(global-set-key [(control x) ?7 ?s] 'gptel-send)
(global-set-key [(control x) ?7 ?r] 'gptel-rewrite)
(global-set-key [(control x) ?7 ?a] 'gptel-add)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elysium (commented out because gptel-aibo is better right now).

;; (use-package elysium
;;   :custom
;;   ;; Below are the default values
;;   (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
;;   (elysium-window-style 'vertical)) ; Can be customized to horizontal

;; (use-package smerge-mode
;;   :ensure nil
;;   :hook
;;   (prog-mode . smerge-mode))

;; (global-set-key [(control x) ?7 ?e] 'elysium-query)
;; (global-set-key [(control x) ?7 ?k] 'elysium-keep-all-suggested-changes)
;; (global-set-key [(control x) ?7 ?d] 'elysium-discard-all-suggested-changes)
;; (global-set-key [(control x) ?7 ?c] 'elysium-clear-buffer)
;; (global-set-key [(control x) ?7 ?a] 'elysium-add-context)
;; (global-set-key [(control x) ?7 ?t] 'elysium-toggle-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel-aibo

(use-package gptel-aibo
  :ensure t)

;; Talk to the AI about a buffer, with context added by gptel-add.
;; Hit C-c ! to accept the AI's suggestions.
(global-set-key [(control x) ?7 ?i] 'gptel-aibo) ;; i for aI (or invoke)

;; Complete the thing you are currently working on.
(global-set-key [(control x) ?7 ?c] 'gptel-aibo-summon) ;; c for complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; claude-code.el

(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; for eat terminal backend:
(use-package eat :ensure t)

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

(add-to-list 'display-buffer-alist
                 '("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . 100)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to do relative paths in .dir-locals.el.
;;
;; he-project-path gets the path of the .dir-locals.el file.  We can
;; use (eval ...) forms to then manipulate the path.

;; ((nil . ((eval . (set (make-local-variable 'he-project-path)
;;                       (file-name-directory
;;                        (let ((d (dir-locals-find-file "./")))
;;                          (if (stringp d) d (car d))))))
;;          ;(eval . (message "Project directory set to `%s'." he-project-path))
;; 	 ))
;;  (rust-mode . ((eval . (set rust-rustfmt-bin
;; 			    (concat he-project-path "scripts/sldb_rustfmt.sh")))
;; 	       ))
;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-max-output-line-length 2000)
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cua-mode nil)
 '(custom-enabled-themes '(tango-dark))
 '(flycheck-disabled-checkers '(go-vet))
 '(flycheck-gometalinter-fast t)
 '(gdb-many-windows t)
 '(global-tab-line-mode t)
 '(large-file-warning-threshold 500000000)
 '(lsp-ui-doc-delay 0.5)
 '(magit-diff-refine-hunk t)
 '(markdown-command "markdown_py")
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(buffer-move claude-code cmake-mode dap-mode dockerfile-mode eat
		 elysium exec-path-from-shell flycheck-golangci-lint
		 ghub go-dlv go-mode gptel gptel-aibo helm ivy log4e
		 lsp-ivy lsp-mode lsp-ui lua-mode magit neotree
		 nix-mode nix-modeline projectile projectile-ripgrep
		 rg rust-mode rustic sr-speedbar swiper vterm
		 websocket yaml-mode zones))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(projectile-tags-backend 'etags-select)
 '(projectile-tags-command "uctags -Re -f \"%s\" %s \"%s\"")
 '(rust-format-on-save t)
 '(rust-rustfmt-bin "/Users/hugh/src/sldb/scripts/sldb_rustfmt.sh")
 '(safe-local-variable-directories '("/Users/hugh/src/sldb/"))
 '(safe-local-variable-values '((engine . django)))
 '(speedbar-show-unknown-files t)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)


