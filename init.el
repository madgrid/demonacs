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
(setq-default cursor-type 'box) ;;bar
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)

;; impove general functionaliy
;; disable *.~undo-tree~" pollution
(setq undo-tree-auto-save-history nil)


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

(require 'quelpa-use-package)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; theme
(use-package noctilux-theme
  :defer t
  :init (load-theme 'noctilux t))

;; treemacs
;; TODO: testing out
;; If not good, or adds complexity. Replace with
;; https://github.com/jojojames/ibuffer-sidebar
;; https://github.com/jojojames/dired-sidebar
(use-package treemacs
  :demand t
  :config
	(setq treemacs-follow-after-init t
				treemacs-is-never-other-window t
				treemacs-width 20)
        (treemacs-follow-mode t)
        (treemacs-filewatch-mode t)
        (treemacs-git-mode 'simple)
	(treemacs-fringe-indicator-mode t)
  :hook (after-init . treemacs)
  :bind
        (:map global-map
        ("C-M-t"   . treemacs)))

;; magit
(use-package magit :ensure t)

;; git-gutter
;; TODO: Trying it out
(use-package git-gutter+
  ;; :bind
  ;; (("C-x p" . git-gutter+-previous-hunk)
  ;;  ("C-x n" . git-gutter+-next-hunk)
  ;;  ("C-x v s" . git-gutter+-stage-hunks)
  ;;  ("C-x v =" . git-gutter+-show-hunk)
  ;;  ("C-x v r" . git-gutter+-revert-hunks)
  ;;  ("C-x v c" . git-gutter+-commit)
  ;;  ("C-x v C" . git-gutter+-stage-and-commit)
  ;;  )
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

;; Might be needed for the eglot function
(use-package js2-mode
  :ensure t)

;; This is required! for LSP and shitty eglot
(setq exec-path (append exec-path '("~/.nvm/versions/node/v16.13.2/bin")))


;; lsp emergency transplant
(use-package lsp-ui :ensure t)
(use-package yasnippet-snippets :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package tide :ensure t)

;; lsp shitty configs
(yas-global-mode)
(setq lsp-completion-provider :capf)
(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil)

;; Annoying stuff
(setq lsp-enable-links t)
(setq lsp-signature-render-documentation t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-completion-enable-additional-text-edit t)

(setq lsp-ui-sideline-show-diagnostics t)


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
(setq company-tooltip-align-annotations t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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
	 ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-line)
	 ("M-s l" . consult-goto-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
	 ;; M-p bindings (projectile & treemacs)
	 ("M-p s" . consult-projectile-switch-project)
 	 ("M-p f" . consult-projectile-find-file)
	 ("M-p b" . consult-projectile-switch-to-buffer)
	 ("M-p t t" . treemacs)
	 ("M-p t d" . treemacs-select-directory)
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
  ;; (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; consult projectile
;; This is a shitty package
(use-package consult-projectile
  :ensure t
  :after consult
  )

;; formatter
;; TODO: not working
;; Failed to run prettier: Searching for program: No such file or directory, prettier
(use-package apheleia
  :ensure t
  :config (apheleia-global-mode t))

;; tree-sitter
;; TODO: Testing this out
;; Add this command: treemacs-project-follow-mode
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

(use-package flycheck :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))

  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "ts" (file-name-extension buffer-file-name))
		(setup-tide-mode))))

  )

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; (typescript-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


;; typescript mode
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  ;; NOTE REMOVE ALL TYEPSCRIPTREACT BULLSHIT
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))


;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'typescript-tslint 'typescript-mode)

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
   '(quelpa-use-package centaur-tabs consult vterm-toggle vterm project-x apheleia git-gutter-fringe+ git-gutter+ eglot spacemacs-theme tree-sitter-langs tree-sitter load-env-vars go-mode orderless company vertico wakatime-mode auto-virtualenv rainbow-delimiters smartparens use-package))
 '(tsi-typescript-indent-offset 4)
 '(wakatime-cli-path "/usr/bin/wakatime-cli"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vertico-current ((t (:inherit highlight :extend t :background "#404040" :foreground "#aaffaa"))))
 '(vertico-group-title ((t (:foreground "#ccaaff" :slant italic)))))

;;; init.el ends here
