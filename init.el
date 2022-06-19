;;; $HOME/dots/emacs/init.el --- Demonacs config

;;; Commentary:
;; let the torture begin
;; ¯\_(ツ)_/¯

;;; Code:

;; make it easy to edit this file
(defun find-config ()
  "Edit Emacs config."
  (interactive)
  (find-file "~/repos/personal/demonacs/init.el"))
(global-set-key (kbd "C-c i") 'find-config)

;; TODO: Try it out
;; easy copy cut paste
;; (cua-mode t)

;; autosave on lose focus
(add-function :after after-focus-change-function
	      (lambda () (save-some-buffers t)))

;; improve looks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)
(defalias 'yes-or-no-p 'y-or-n-p)

;; improve mac keyboard
(setq mac-command-modifier 'meta)
(setq mac-right-option-modifier 'control)

;; remove minor mode from mode-line
;; https://emacs.stackexchange.com/a/41135
(let ((my/minor-mode-alist '((flycheck-mode flycheck-mode-line))))
  (setq mode-line-modes
        (mapcar (lambda (elem)
                  (pcase elem
                    (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                     `(:propertize ("" ,my/minor-mode-alist)
			           mouse-face mode-line-highlight
			           local-map ,mode-line-minor-mode-keymap)
                     )
                    (_ elem)))
                mode-line-modes)
        ))

;; move focus to split window
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; wrap lines
(global-visual-line-mode t)

;; refresh buffer when files change on disk
(global-auto-revert-mode t)

;; stop creating ~ files
(setq make-backup-files nil)

;; font
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))
(set-frame-font "Monego Nerd Font Fix 14" nil t)

;; remember cursor position
(save-place-mode t)

;; org capture notes file
(setq org-default-notes-file "~/org/bcw-notes.org")

;; org capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

;; package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
(setq use-package-always-ensure t)

;; theme
(use-package noctilux-theme
  :defer t
  :init (load-theme 'noctilux t))

;; magit
(use-package magit :ensure t)

;; git-gutter
;; TODO: Trying it out
(use-package git-gutter+
  :bind
  (("C-x p" . git-gutter+-previous-hunk)
   ("C-x n" . git-gutter+-next-hunk)
   ("C-x v s" . git-gutter+-stage-hunks)
   ("C-x v =" . git-gutter+-show-hunk)
   ("C-x v r" . git-gutter+-revert-hunks)
   ("C-x v c" . git-gutter+-commit)
   ("C-x v C" . git-gutter+-stage-and-commit)
   )
  :config
  (setq git-gutter+-added-sign "|"
	git-gutter+-modified-sign "|")
  (global-git-gutter+-mode)
  :diminish (git-gutter+-mode . "gg"))
(use-package git-gutter-fringe+)

;; help with parens and delimiters
(use-package smartparens
  :hook ((prog-mode . smartparens-mode)))
(use-package rainbow-delimiters
  :hook((prog-mode . rainbow-delimiters-mode)))

;; vterm
(use-package vterm)
(use-package vterm-toggle
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-hide-method 'reset-window-configration)
  :bind (("C-c t" . #'vterm-toggle)))

;; eglot
(use-package eglot
  :ensure t
  :bind
  (("C-c r" . eglot-rename)
   ("C-c h" . eldoc))
  :hook ((python-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (bash-mode . eglot-ensure)))

;; load env vars
(use-package load-env-vars)

;; python
(use-package auto-virtualenv
  :hook
  (python-mode . auto-virtualenv-set-virtualenv))

;; go
(use-package go-mode)

;; vertico
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))
(savehist-mode t)

;; vertico directory
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; company
(use-package company
  :init
  (global-company-mode))

;; consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history))


  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; Might not need this
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; consult projectile
(use-package consult-projectile
  :ensure t
  :after consult
  )

;; formatter
(use-package apheleia
  :ensure t
  :config (apheleia-global-mode t))

;; tree-sitter
;; TODO: Testing this out
(use-package tree-sitter
  :ensure t
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  :config
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; typescript mode
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; tsi - not yet in melpa
;; Tree-sitter based indentation for TypeScript, JSON and (S)CSS.
;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))


;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(package-selected-packages
   '(centaur-tabs consult vterm-toggle vterm project-x apheleia git-gutter-fringe+ git-gutter+ eglot spacemacs-theme tree-sitter-langs tree-sitter load-env-vars go-mode orderless company vertico wakatime-mode auto-virtualenv rainbow-delimiters smartparens use-package))
 '(wakatime-cli-path "/usr/bin/wakatime-cli"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
