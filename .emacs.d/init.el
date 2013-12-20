
;Try 1 - set the command key as meta
;This actually interprets the OS modified characters back into what is expected
(set-keyboard-coding-system nil)
;
;Try 2 - set command key to meta
;(setq mac-option-key-is-meta nil)
;(setq mac-command-key-is-meta t)
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier nil)
;
;Try 3 - set command key to meta
;(setq mac-option-modifier 'super)
;(setq mac-command-modifier 'meta)
;
;Try 4 - again, just tring to get a sane meta key ( alt or command )
;(setq mac-option-modifier 'super)
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier nil)
;
;SOLUTION - in Terminal.app: Preferences->Settings->Keyboard->"Use Option as Meta key"
;
;UTF-8 Encoding as recommended by: http://www.emacswiki.org/emacs/EmacsForMacOS#toc20
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;solarized
;(load-theme 'solarized-[light|dark] t)

; Path fixes recommended by http://clojure-doc.org/articles/tutorials/emacs.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

; for ELPA repo managementy stuff as recommended at: http://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(unless (package-installed-p 'sbt-mode)
  (package-refresh-contents) (package-install 'sbt-mode))


(defvar my-packages '(starter-kit
		       starter-kit-lisp
		       starter-kit-bindings
		       starter-kit-eshell
		       clojure-mode
		       clojure-test-mode
		       cider
                       rainbow-delimiters
                       color-theme-solarized))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'rainbow-delimiters)
                                        ;(to enable in all programming-related modes
                                        ; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-rainbow-delimiters-mode)
;(require 'color-theme-solarized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(load-theme 'color-theme-sanityinc-solarized-dark t)
(load-theme 'solarized-dark t)

; To enable ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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

                                        ; attempted fix for Cider woes
(defun cider--library-version ()
  "Get the version in the nrepl library header."
  ;; (-when-Let (version (pkg-info-Library-version 'cider))
  ;;  (pkg-info-format-version version))
  "0.3.0-SNAPSHOT")

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

;; Turn on line numbers
(global-linum-mode 1)
