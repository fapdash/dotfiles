;; Initialize Garbage Collector every 20mb allocated.
(setq gc-cons-threshold 20000000)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" .
               "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
      (package-install package))))

(sacha/package-install 'use-package)
(require 'use-package)

(defun sacha/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

(setq backup-directory-alist      '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosave" t)))

;; from github.com/purcell
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(sacha/package-install 'color-theme-modern)
(sacha/package-install 'color-theme-sanityinc-solarized)
(sacha/package-install 'monokai-theme)
										;(color-theme-solarized 'dark)

										;(defun sacha/setup-color-theme ()
										;  (interactive)
										;  (color-theme-solarized 'dark)
										;  (set-face-foreground 'secondary-selection "darkblue")
										;  (set-face-background 'secondary-selection "lightblue")
										;  (set-face-background 'font-lock-doc-face "black")
										;  (set-face-foreground 'font-lock-doc-face "wheat")
										;  (set-face-background 'font-lock-string-face "black")
										;  (set-face-foreground 'org-todo "green")
										;  (set-face-background 'org-todo "black"))

										;(use-package color-theme
										;  :init
										;  (when window-system
										;    (sacha/setup-color-theme)))
										;(load-theme 'monokai t)
(load-theme 'monokai t)

										;(when window-system
										;  (custom-set-faces
										;   '(erc-input-face ((t (:foreground "antique white"))))
										;   '(ido-first-match ((t (:background "ForestGreen" :foreground "black"))))
										;   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black");)) t)
										;   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
										;   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
										;   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
										;   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:fo;reground "LightSalmon" :strike-through t))))
										;   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflow;er blue"))))))

(global-auto-revert-mode 1) ;; Always reload the file if it changed on disk
(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding

(set-fringe-mode '(10 . 0)) ;; Show a nice fringe

;; Remove the fringe indicators
(when (boundp 'fringe-indicator-alist)
  (setq-default fringe-indicator-alist
        '(
          (continuation . nil)
          (overlay-arrow . nil)
          (up . nil)
          (down . nil)
          (top . nil)
          (bottom . nil)
          (top-bottom . nil)
          (empty-line . nil)
          (unknown . nil))))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (yas-minor-mode -1)
              (setq term-buffer-maximum-size 10000))))

(use-package goto-chg
  :ensure t
  :config
  (global-set-key [(control ?.)] 'goto-last-change)
  (global-set-key [(control ?,)] 'goto-last-change-reverse))


(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode t))

;; disable all toolbars and menus
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(delete-selection-mode)

;; delete trailing whitespace before save
(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  :init
  (global-whitespace-mode +1))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "M-+") 'er/expand-region))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package ag
  :ensure t)

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode +1)
  :config
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (set-face-attribute 'anzu-mode-line nil
					  :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => ")))

(use-package smartscan
  :ensure t
  :init
  (global-smartscan-mode 1))

(use-package smart-mode-line
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :disabled t)

(use-package adoc-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))
										;(use-package edts
										;  :ensure t
										;  :init (require 'edts-start))


										;(setq load-path (cons  "C:/Program Files/erl6.2/lib/tools-2.7/emacs"
										;                       load-path))
(setq erlang-root-dir "C:/Program Files/erl6.2")
(setq exec-path (cons "C:/Program Files/erl6.2/bin" exec-path))

(use-package erlang
  :ensure t
  :init
  (require 'erlang-start)
  (defvar inferior-erlang-prompt-timeout t))


(setq desktop-auto-save-timeout 30
      desktop-save t)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
(desktop-save-mode 1)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; EasyMotion-like code navigation
;; jump to specific character
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-c SPC") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "s-,") 'avy-goto-char-2))

;; move to window by window number
;; swap windows with C-u + ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key (kbd "C-x o") 'ace-window))

;; move to different window with shift + arrow keys
(use-package windmove
  :ensure t
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

;; winner-mode lets you use C-c <left> and C-c <right> to switch between window configurations.
(use-package winner
  :ensure t
  :config (winner-mode 1))

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (global-set-key (kbd "C-c m") 'magit-status))

;; use ibuffer groups to garbage collect temp buffers
(use-package ibuffer
  :init
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-switch-to-saved-filter-groups "Default")))
  :config
  (setq-default ibuffer-saved-filter-groups
                `(("Default"
                   ;; I create a group call Dired, which contains all buffer in dired-mode
                   ("Dired" (mode . dired-mode))
                   ("yari" (mode . yari-mode))
                   ("Temporary" (name . "\*.*\*")))))
  ;; don't ask before closing a buffer
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  :bind
  ("C-x C-b" . ibuffer))

(use-package discover
  :ensure t
  :init
  (global-discover-mode 1))

(setq sentence-end-double-space nil)

(setq tab-width 2)
(setq indent-tabs-mode nil)

(use-package move-text
  :ensure t
  :config
  (global-set-key [(meta up)] 'move-text-up)
  (global-set-key [(meta down)] 'move-text-down))

(use-package crux
  :ensure t
  :config
  (global-set-key [(meta super up)] 'crux-duplicate-current-line-or-region)
  (global-set-key [(meta super down)] 'crux-duplicate-current-line-or-region))

(use-package aggressive-indent
  :ensure t
  :init
  ;; messes with open source contribution
  (global-aggressive-indent-mode 0))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p) ;; Ask for confirmation before closing emacs

(let ((repetitions "10"))
  (fset 'centered-next
        (concat "\C-u" repetitions "\C-n\C-l"))
  (global-set-key "\M-n" 'centered-next)

  (fset 'centered-previous
        (concat "\C-u" repetitions "\C-p\C-l"))
  (global-set-key "\M-p" 'centered-previous))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;; (global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o', currently overwritten by switch-window

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "<C-tab>") 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(sml/name-width 35)
 '(sml/shorten-directory t)
 '(sml/shorten-modes t)
 '(tab-width 4)
 '(undo-tree-visualizer-diff t)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))) t)
 '(ido-first-match ((t (:background "ForestGreen" :foreground "black"))))
 '(ido-selection ((t (:background "ForestGreen" :foreground "black"))) t)
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))) t)
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))) t)
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))) t)
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))) t)
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))) t)
 '(sp-show-pair-match-face ((t (:background "dark slate blue")))))


(setq debug-on-error t)

(transient-mark-mode 1)

;; step through symbols with Meta + left/right
(use-package auto-highlight-symbol
  :ensure t
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (ahs-set-idle-interval 0.1)
  (ahs-chrange-display))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay "0.075"))




(use-package fuzzy
  :ensure t)

(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20151112.2030/ac-dict")
  (global-auto-complete-mode +1)
  :config
  (setq ac-auto-show-menu t
		ac-quick-help-delay 0.2
        ac-auto-start 0
		ac-use-fuzzy t
        ac-show-menu-immediately-on-auto-complete t))


(set-frame-parameter nil 'fullscreen 'maximized)

(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(if (eq system-type 'windows-nt)
    (progn
      (add-hook 'window-setup-hook 'w32-maximize-frame t))
  (set-frame-parameter nil 'fullscreen 'maximized))


(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package projectile-rails
  :ensure t
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))




(dolist (exp '("Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"))
  (add-to-list 'auto-mode-alist
              (cons exp 'ruby-mode)))

;; Ruby-Refactor keybinds
;; Extract to Method (C-c C-r e)
;; Extract Local Variable (C-c C-r v)
;; Extract Constant (C-c C-r c)
;; Add Parameter (C-c C-r p)
;; Extract to Let (C-c C-r l)
;; Convert Post Conditional (C-c C-r o)
(use-package ruby-refactor
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

;; you need to have the rcodetools gem installed to use this
(use-package rcodetools
  :bind
  ;; inline code evaluation for ruby
  ("C-c C-e" . xmp))

(use-package quickrun
  :ensure t
  :bind
  ("C-c q" . quickrun)
  ("C-c Q" . quickrun-region))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
  :init
  (yas-global-mode 1)
  ;; show dropdown menu if more than one snippet available for keyword
  (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt)))

(use-package rspec-mode
  :ensure t
  :init
    (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(use-package minitest
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'minitest-mode)
  :config
  (eval-after-load 'minitest
  '(minitest-install-snippets)))



(use-package rubocop
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode))

(use-package yari
	:ensure t
	:config
	(defalias 'ri 'yari))


(use-package ruby-compilation
  :ensure t
  :config
  (after-load 'ruby-mode
	(let ((m ruby-mode-map))
	  (define-key m [S-f7] 'ruby-compilation-this-buffer)
	  (define-key m [f7] 'ruby-compilation-this-test))))


(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r r") 'inf-ruby-console-auto)

(global-set-key (kbd "C-c r s") 'inf-ruby-console-racksh)

(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

(add-hook 'ruby-mode-hook 'projectile-mode)

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c b i")
       (lambda () (interactive) (async-shell-command "bundle install" "**Bundler**")))
     (define-key ruby-mode-map (kbd "C-c b u")
       (lambda () (interactive) (async-shell-command "bundle update" "**Bundler**")))))

(use-package ido
  :ensure t)

(use-package flx-ido
  :ensure t
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :config
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys))


;; convenient interface to your recently and most frequently used commands
;; build on top of ido, so you get flx-ido behaviour
(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  ;; use robe for smarter auto-complete-mode
  (add-hook 'robe-mode-hook 'ac-robe-setup))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; activate web-mode for plain html
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :init
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package rvm
  :ensure t
  :init
  (rvm-activate-corresponding-ruby)
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

;; elixir integration
(use-package alchemist
  :ensure t)

(use-package ac-alchemist
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup))

(use-package elixir-yasnippets
  :ensure t)

;; smartparens hack for elixir
(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "->" "end"
                 :when '(("RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(use-package flycheck-credo
  :ensure t
  :config
  (setq flychck-elixir-credo-strict t)
  (eval-after-load 'flycheck
    '(flycheck-credo-setup))
  (add-hook 'elixir-mode-hook 'flycheck-mode))

(use-package elm-mode
  :ensure t
  :init
  ;; needs elm oracle installed, doesn't come with elm install
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

(use-package rust-mode
  :ensure t)

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :config
  (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
  (setq racer-rust-src-path (concat (getenv "HOME") "/src/rust/src")))

(use-package ac-racer
  :ensure t
  :init
  (defun my/racer-mode-hook ()
    (ac-racer-setup))
  (add-hook 'racer-mode-hook 'my/racer-mode-hook))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

                                        ;(global-company-mode t)
                                        ;(push 'company-robe company-backends)


(define-minor-mode my/pair-programming-mode
  "Toggle visualizations for pair programming.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Pairing"
  ;; The minor mode bindings.
  '()
  :group 'my/pairing
  (linum-mode (if my/pair-programming-mode 1 -1)))

(define-global-minor-mode my/global-pair-programming-mode
  my/pair-programming-mode
  (lambda () (my/pair-programming-mode 1)))

(global-set-key "\C-c\M-p" 'my/global-pair-programming-mode)

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
  (message "Renamed to %s." new-name)))

(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

(use-package discover
  :ensure t
  :init
  (global-discover-mode 1))

(use-package uniquify
  :config
  ;; Setup uniquify so that non-unique buffer names get the parent path included to make them unique.
  (setq
   uniquify-buffer-name-style 'forward
   uniquify-separator " : "))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;;Fullframe saves your window configuration before displaying the next command in the entire Emacs window. When the command finishes, it restores your previous window configuration.
(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window nil))

(use-package dictionary
  :ensure t)

(use-package google-this
  :ensure t
  :init
  (google-this-mode 1)
  :config
  (global-set-key (kbd "C-x g") 'google-this-mode-submap))

(use-package google-translate
  :ensure t
  :init
  (require 'google-translate-smooth-ui)
  :config
  (setq google-translate-translation-directions-alist
        '(("de" . "en") ("en" . "de")))
  (setq google-translate-enable-ido-completion 't)
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (global-set-key "\C-cT" 'google-translate-query-translate))

;; plantUML for UML generation from text
(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
  (setq puml-plantuml-jar-path (concat (getenv "HOME") "/plantUML/plantuml.jar"))
  :config
  ;; puml doesn't support auto-indent
  (add-hook 'puml-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; source for css: https://gist.github.com/Dashed/6714393
  :init (setq markdown-command (concat (concat "pandoc -c file:///" (getenv "HOME")) "/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")))

;; couldn't decide on a good keybind for this so far
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(use-package workgroups2
  :ensure t
  :config
  ;; Change prefix key (before activating WG)
  (setq wg-prefix-key (kbd "C-c w"))

  ;; Change workgroups session file
                                        ;(setq wg-session-file "~/.emacs.d/emacs_workgroups")
  (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
  (setq wg-workgroups-mode-exit-save-behavior 'save)

  (setq wg-session-load-on-start t)    ; default: (not (daemonp))

  ;; Set your own keyboard shortcuts to reload/save/switch WGs:
  ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
  (global-set-key (kbd "<pause>")     'wg-reload-session)
  (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
  (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
  (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup))


(workgroups-mode 1)        ; put this one at the bottom of .emacs

(and window-system (server-start))
