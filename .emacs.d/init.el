; LAYOUT
; - fix encoding issues
; - package management
;   - configuration of repos
;   - intial installation of packages
; - configuration and hooks for each installed package



; - - - - - -  OS X Keyboard encoding stuff - - - - - - 
;!!! in Terminal.app: Preferences->Settings->Keyboard->"Use Option as Meta key"
;This actually interprets the OS modified characters back into what is expected
(set-keyboard-coding-system nil)
;
;UTF-8 Encoding as recommended by: http://www.emacswiki.org/emacs/EmacsForMacOS#toc20
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)




; - - - - - - PACKAGE MANAGEMENT - - - - - -- -
; for ELPA repo managementy stuff as recommended at: http://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)



; MOVING THE FOLLOWING INTO my-packages PROCEDURE BELOW
;; (unless (package-installed-p 'scala-mode2)
;;   (package-refresh-contents) (package-install 'scala-mode2))

;; (unless (package-installed-p 'sbt-mode)
;;   (package-refresh-contents) (package-install 'sbt-mode))

;; remove from below
;; clojure-test-mode
(defvar my-packages 
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell
    clojure-mode
    cider
    rainbow-delimiters
    color-theme-solarized
    haskell-mode
    scala-mode2
    sbt-mode
    minimap)
  "A list of packages that I wish to have installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))







; - - - - - -  POST INSTALLATION CONFIGURATION OF PACKAGES - - - - - 


; - - - - - - Rainbow-Delimiters - - - - - 
(require 'rainbow-delimiters)
;(to enable in all programming-related modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
; to enable in all modes
 ; ERORR
 ;Symbol's function definition is void: global-rainbow-delimiters-mode
;(global-rainbow-delimiters-mode)


; - - - - - - CLOJURE & CIDER - - - - -- 
; Path fixes recommended by http://clojure-doc.org/articles/tutorials/emacs.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode_hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)





; - - - - - - Solarized ( color theme ) - - - - -
;(require 'color-theme-solarized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
   (quote
    (starter-kit-lisp starter-kit-eshell starter-kit-bindings solarized-theme sbt-mode rainbow-delimiters minimap intero gotest go-projectile company-go color-theme-solarized cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(load-theme 'color-theme-sanityinc-solarized-dark t)
(load-theme 'solarized-dark t)



; - - - - - - - ParEdit - - - - - - 
; To enable ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)




; - - - - - - ElDoc - - - - - - 
; elDoc hooks to work with ParEdit
(require 'eldoc) ; if not already loaded
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)

; Enable eldoc in clojure buffers: (as @
                                        ; https://github.com/clojure-emacs/cider )
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-wrap-history t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

                                        ; Enable ParEdit in clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)




; - - - - - - CIDER - - - - - - -
                                        ; attempted fix for Cider woes
(defun cider--library-version ()
  "Get the version in the nrepl library header."
  ;; (-when-Let (version (pkg-info-Library-version 'cider))
  ;;  (pkg-info-format-version version))
  "0.3.0-SNAPSHOT")




; - - - - - - - SBT & Scala-Mode2 - - - - - 
; Customizations for Scala-mode2 & sbt-mode as per:
                                        ; https://github.com/hvesalai/sbt-mode
(add-hook 'sbt-mode-hook '(lambda ()
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate) 
  ))

(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
   ))

(global-set-key (kbd "M-'") 'next-error)




; - - - - - - LINE NUMBERS - - - - - - 
;; Turn on line numbers
(global-linum-mode 1)




; - - - - - - MiniMap ( SublimeTex like sidebar ) - - - - - - 
;; Minimap - 'Sublime Text' like sidebar
(require 'minimap)

 ; - - - - - - Haskell - - - - - - - -
 ; This needed to be altered manually
; TODO: figure out how to discover this config file in a more
; 'generic' way
;(load "~/.emacs.d/elpa/haskell-mode-20141104.1247/haskell-mode.el")
;(load "~/.emacs.d/elpa/haskell-mode-20141119.1110/haskell-mode.el")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; indentation options are: 'turn-on-haskell-indentation || 'turn-on-haskell-indent || 'turn-on-haskell-simple-indent
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)




;;  - - - - - - Auto-fill - - - - - - -
;; ( a very annoying word wrap )

(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; - - - Kill backwards line - - -
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key "\C-u" '
               backward-kill-line) ;; 'C-u'





;; - - - - - - Scrolling fix - - - - - -
;; ( from https://www.emacswiki.org/emacs/SmoothScrolling )

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)




;; - - - - - - Go Lang - - - - - - - - -
;; ( from
;; https://github.com/cockroachdb/cockroach/wiki/Ben's-Go-Emacs-setup
;; )
; pkg go installation
(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))

; As-you-type error highlighting
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun my-go-mode-hook ()
      (setq tab-width 2 indent-tabs-mode 1)
      ; eldoc shows the signature of the function at point in the status bar.
      (go-eldoc-setup)
      (local-set-key (kbd "M-.") #'godef-jump)
      (add-hook 'before-save-hook 'gofmt-before-save)

      ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
      (let ((map go-mode-map))
        (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c m") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c b") 'go-run)))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; Use projectile-test-project in place of 'compile'; assign whatever key you want.
(global-set-key [f9] 'projectile-test-project)

; "projectile" recognizes git repos (etc) as "projects" and changes settings
; as you switch between them. 
(projectile-global-mode 1)
(require 'go-projectile)
(go-projectile-tools-add-path)
(setq gofmt-command (concat go-projectile-tools-path "/bin/goimports"))

; "company" is auto-completion
(require 'company)
(require 'go-mode)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-go))))

; gotest defines a better set of error regexps for go tests, but it only
; enables them when using its own functions. Add them globally for use in
(require 'compile)
(require 'gotest)
(dolist (elt go-test-compilation-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist elt))
(defun prepend-go-compilation-regexps ()
  (dolist (elt (reverse go-test-compilation-error-regexp-alist))
    (add-to-list 'compilation-error-regexp-alist elt t)))
(add-hook 'go-mode-hook 'prepend-go-compilation-regexps)

; end .emacs additions


;; - - - - - - - - - - - - - - - - Common Lisp - - - - - - - - - - - -
;; from:
;;   http://lisp-lang.org/learn/getting-started/
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/opt/local/bin/sbcl")





;; - - - - - - - - - - - - - - - - My Custom Notes format - - - - - - - - - - - -
;; from:
;; ~/Source/GIT/emacs-myNotes
(add-to-list 'load-path "~/.emacs.d/manually-installed-plugins/")
(load "mynotes-mode.el")

(require 'myNotes-mode)
(add-to-list 'auto-mode-alist '("\\.notes.txt\\'" . myNotes-mode))
(put 'downcase-region 'disabled nil)






;; - - - - - - - - - - - - - - - - My Intero for Haskell  - - - - - -
;; - - - -- - - - - - - - - - - - - - - - - -
;; from:
;; https://haskell-lang.org/intero
(require 'package)
(add-to-list
'package-archives
'("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; my fix
(add-to-list 'exec-path "/usr/local/bin/")
