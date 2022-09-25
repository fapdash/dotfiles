;; Initialize Garbage Collector every 20mb allocated.
(setq gc-cons-threshold 20000000)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

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

(use-package use-package-ensure-system-package
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(defun sacha/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

(setq backup-directory-alist      '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosave" t)))
(setq vc-make-backup-files t)

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

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(sacha/package-install 'color-theme-modern)
(sacha/package-install 'color-theme-sanityinc-solarized)
(sacha/package-install 'monokai-theme)
(use-package tangotango-theme
  :ensure t)
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
;; (load-theme 'monokai t)
(load-theme 'tangotango t)

										;(when window-system
										;  (custom-set-faces
										;   '(erc-input-face ((t (:foreground "antique white"))))
										;   '(ido-first-match ((t (:background "ForestGreen" :foreground "black"))))
										;   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black");)) t)
										;   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
										;   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
										;   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
										;   '(org-headline-done ((((t color) (min-colors 16) (background dark)) (:fo;reground "LightSalmon" :strike-through t))))
										;   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflow;er blue"))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(erc-input-face ((t (:foreground "antique white"))) t)
 '(ido-first-match ((t (:background "ForestGreen" :foreground "black"))))
 '(ido-selection ((t (:background "ForestGreen" :foreground "black"))) t)
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 1.8))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.7))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.6))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :height 1.5))))
 '(markup-title-4-face ((t (:inherit markup-gen-face :height 1.4))))
 '(markup-title-5-face ((t (:inherit markup-gen-face :height 1.3))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))
 '(sp-show-pair-match-face ((t (:background "dark slate blue")))))

(global-auto-revert-mode 1) ;; Always reload the file if it changed on disk
(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding

(and window-system (set-fringe-mode '(10 . 0))) ;; Show a nice fringe

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; https://karthinks.com/software/batteries-included-with-emacs/#view-mode--m-x-view-mode
(setq view-read-only t)

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

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

;; invert electric-indent keybinds
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

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
  (setq multi-term-program "/bin/bash")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (yas-minor-mode -1)
              (setq term-buffer-maximum-size 10000))))

(use-package vterm
  :ensure t
  :init
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  :config
  ;; https://old.reddit.com/r/emacs/comments/op4fcm/send_command_to_vterm_and_execute_it/h63i4f3/
  (defun my/vterm-execute-current-line ()
    "Insert text of current line in vterm and execute."
    (interactive)
    (require 'vterm)
    (let ((command (buffer-substring
                    (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (end-of-line)
                      (point)))))
      (let ((buf (current-buffer)))
        (unless (get-buffer vterm-buffer-name)
          (vterm))
        (display-buffer vterm-buffer-name t)
        (switch-to-buffer-other-window vterm-buffer-name)
        (vterm--goto-line -1)
        (message command)
        (vterm-send-string command)
        (vterm-send-return)
        (switch-to-buffer-other-window buf)
        ))))

(use-package multi-vterm
  :ensure t)

(use-package goto-chg
  :ensure t
  :config
  (global-set-key [(control ?.)] 'goto-last-change)
  (global-set-key [(control ?,)] 'goto-last-change-reverse))

;; really important for discoverability,
;; pops up a window with all possible keystroke completions
;; alternative: https://github.com/kai2nenobu/guide-key
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package helpful
  :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

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

(use-package diminish
  :ensure t
  :config
  (diminish 'WS)
  (diminish 'which-key-mode)
  (diminish 'undo-tree-mode)
  (diminish 'highlight-indent-guides-mode)
  (diminish 'git-gutter-mode)
  (diminish 'yas/minor-mode)
  (diminish 'yas-minor-mode)
  (diminish 'google-this-mode)
  (diminish 'auto-complete-mode)
  (diminish 'workgroups-mode)
  (diminish 'smartparens-mode)
  (diminish 'auto-highlight-symbol-mode)
  (diminish 'global-whitespace-mode)
  (diminish 'global-activity-watch-mode)
  (diminish 'ivy-mode)
  (diminish 'org-roam-mode)
  (diminish 'git-gutter-mode)
  (diminish 'org-roam-bibtex-mode)
  (diminish 'ace-isearch-mode)
  (diminish 'company-box-mode)
  (diminish 'activity-watch-mode))

(delete-selection-mode)

;; delete trailing whitespace before save
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; smooth scrolling, tried sublimity but it felt "jumpy" when scrolling fast
;; more info under: http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1) ;; is said to introduce lag
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; three lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  ;; deactivate line limit for adoc-mode
  ;; can't be set to nil because it then uses fill-column
  ;; TODO: would be better to remove lines-tail and tail from whitespace-style
  (add-hook 'adoc-mode-hook #'fap/deactivate-line-max-highlight)
  (add-hook 'org-mode-hook #'fap/deactivate-line-max-highlight)
  (add-hook 'adoc-mode-hook #'fap/deactivate-line-max-highlight)
  (add-hook 'vterm-mode-hook #'fap/deactivate-line-max-highlight)
  :init
  (global-whitespace-mode +1))


(defun fap/deactivate-line-max-highlight ()
  (and (boundp 'whitespace-mode)
       (set (make-local-variable 'whitespace-line-column) 999)))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places")
  :init
  (save-place-mode 1))

(use-package expand-region
  :ensure t
  :after auto-highlight-symbol
  :config
  (global-set-key (kbd "M-+") 'er/expand-region)
  (global-set-key (kbd "M--") 'nil)
  (global-set-key (kbd "M--") 'er/contract-region))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ag
  :ensure t)

(use-package smartscan
  :ensure t
  :config
  (define-key smartscan-map (kbd "M-'") nil)
  (global-smartscan-mode 1))

(use-package anzu
  :ensure t
  :after smartscan
  :config
  (global-set-key (kbd "M-'") 'anzu-query-replace-at-cursor)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (set-face-attribute 'anzu-mode-line nil
					  :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => "))
  (global-anzu-mode +1))

(setq column-number-mode t)

(use-package dired
  :bind  (:map dired-mode-map ("°" . (lambda () (interactive) (find-alternate-file ".."))))
  :config
  ;; allow dired to delete or copy dir
  (setq dired-recursive-copies (quote always)) ; “always” means no asking
  (setq dired-recursive-deletes (quote top)) ; “top” means ask once
  (put 'dired-find-alternate-file 'disabled nil))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package all-the-icons
  :ensure t
  ;; have to run all-the-icons-install-fonts once
  )

(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons dired)
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode))))
  :diminish 'all-the-icons-dired-mode)

;; view large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;; tail a file, possible to tail a remote log file through TRAMP
(use-package itail
  :ensure t)

;; shows matching paranthesis with matching colors
(use-package rainbow-delimiters
  :ensure t)

;; provides color preview in buffer
(use-package rainbow-mode
  :ensure t)

(use-package adoc-mode
  :ensure t
  :bind (("M-<left>" . adoc-demote)
         ("M-<right>" . adoc-promote))
  :config
  ;; https://github.com/dakra/moe-theme.el/blob/85d3d84f22096cf181f1494a99864515e76a1047/moe-dark-theme.el#L177-L183
  (custom-set-faces
   `(markup-title-0-face ((t (:inherit markup-gen-face :height 1.8))))
   `(markup-title-1-face ((t (:inherit markup-gen-face :height 1.7))))
   `(markup-title-2-face ((t (:inherit markup-gen-face :height 1.6))))
   `(markup-title-3-face ((t (:inherit markup-gen-face :height 1.5))))
   `(markup-title-4-face ((t (:inherit markup-gen-face :height 1.4))))
   `(markup-title-5-face ((t (:inherit markup-gen-face :height 1.3)))))
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
  (global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "s-,") 'avy-goto-char-2))

(use-package copy-as-format
  :ensure t)

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

;; move buffers from window to window
;; methods are called buf-move-*
(use-package buffer-move
  :ensure t)

;; winner-mode lets you use C-c <left> and C-c <right> to switch between window configurations.
(use-package winner
  :ensure t
  :config (winner-mode 1))

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)

(use-package magit
  :ensure t
  :config
  (define-prefix-command 'magit-map)
  (global-set-key (kbd "C-c m") 'magit-map)
  (define-key magit-map (kbd "m") 'magit-status)
  (define-key magit-map (kbd "f") 'magit-find-file)
  (define-key magit-map (kbd "l") 'magit-log-buffer-file)
  (define-key magit-map (kbd "t") 'git-timemachine-toggle)
  ;; TODO: this fixes keybinding conflicts with smartscan, find a better solution
  ;;(global-set-key (kbd "C-M-p") 'git-rebase-move-line-up)
  ;;(global-set-key (kbd "C-M-n") 'git-rebase-move-line-down)
  (setq magit-completing-read-function 'ivy-completing-read)
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")
  (setq git-commit-major-mode 'markdown-mode))

(use-package magit-popup
  :ensure t)

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))

(autoload 'org-read-date "org")

(defun magit-org-read-date (prompt &optional _default)
  (org-read-date 'with-time nil nil prompt))

(magit-define-popup-option 'magit-log-popup
  ?s "Since date" "--since=" #'magit-org-read-date)

(magit-define-popup-option 'magit-log-popup
  ?u "Until date" "--until=" #'magit-org-read-date)


(use-package git-timemachine
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  :diminish 'git-gutter-mode)

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

(use-package ibuffer-vc
  :ensure t)

(use-package ibuffer-projectile
  :ensure t)

(use-package discover
  :ensure t
  :init
  (global-discover-mode 1))

;; don't look for double spaces at the end to identify sentences
(setq sentence-end-double-space nil)

(setq tab-width 2)
(setq indent-tabs-mode nil)

;; don't require escaped backslashes in re-builder
(setq reb-re-syntax 'string)

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :diminish highlight-indent-guides-mode)

(use-package highlight-indentation
:ensure t)

(use-package move-text
  :ensure t
  :config
  (global-set-key [(meta up)] 'move-text-up)
  (global-set-key [(meta down)] 'move-text-down))

(use-package smart-shift
  :ensure t
  :config
(global-set-key (kbd "<C-iso-lefttab>") 'smart-shift-left)
(global-set-key [(C tab)] 'smart-shift-right))

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
 '(ace-isearch-function 'avy-goto-word-1)
 '(ace-isearch-function-from-isearch 'my/ace-isearch-swiper-from-isearch)
 '(ace-isearch-input-length 2)
 '(ace-isearch-jump-delay 0.2)
 '(ace-isearch-use-jump 'printing-char)
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default))
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   '("/home/fap/repos/org/todo/calendar.org" "/home/fap/repos/org/todo/daily_habits.org" "/home/fap/repos/org/todo/dating.org" "/home/fap/repos/org/todo/habits.org" "/home/fap/repos/org/todo/kanban.org" "/home/fap/repos/org/todo/notes.org" "/home/fap/repos/org/todo/todo.org" "/home/fap/repos/org/todo/week_overview.org" "/home/fap/repos/org/todo/weekly_habits.org" "/home/fap/repos/org/journal/20201228"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m))
 '(package-selected-packages
   '(quickrun org-superstar org-preview-html w3m yaml-mode web-mode tide robe yasnippet-snippets smartparens org-tree-slide spacemacs-theme writeroom-mode org-ref embrace smart-semicolon ox-hugo ox-pandoc projectile-rails smart-mode-line org-contrib org-gcal terminal-here workgroups2 racer rubocop rspec-mode yasnippet org-present org-kanban org-download org-web-tools flycheck-ledger ledger-mode beginend keyfreq editorconfig org-pomodoro all-the-icons-dired mode-icons doom-modeline spaceline ace-isearch helpful emmet-mode paredit pdfgrep org-noter-pdftools org-pdftools markdown-mode company-box ruby-test-mode ivy-hydra ivy-rich company-quickhelp counsel-etags emacs-w3m ivy-xref prettier-js org-rich-yank diminish org-cliplink activity-watch-mode org-agenda-property org-ql multi-vterm org-noter ivy-bibtex company-org-roam org-roam-bibtex org-journal ansible magit-todos flutter-l10n-flycheck flutter use-package-ensure-system-package dart-mode calfw-ical calfw-org calfw hide-mode-line deft deadgrep alchemist mastodon exec-path-from-shell iy-go-to-char copy-as-format epresent esprent smart-shift engine-mode itail vlf vfl htmlize tangotango-theme org-mode discover-my-major ivy-historian ac-dabbrev iedit wgrep-ag imenu-list ruby-tools rbenv counsel-projectile fzf smex counsel ivy projectile-ripgrep ripgrep dumb-jump yari undo-tree switch-window smartscan rvm ruby-refactor ruby-compilation puml-mode pos-tip plantuml-mode nyan-mode neotree move-text minitest goto-chg google-translate google-this fuzzy fullframe flymake-ruby flycheck-rust flycheck-credo flx-ido fill-column-indicator expand-region erlang elm-mode elixir-yasnippets discover dictionary crux comment-dwim-2 color-theme-solarized color-theme-sanityinc-solarized color-theme-modern auto-highlight-symbol anzu aggressive-indent ag adoc-mode ace-window))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((magit-todos-exclude-globs "*.js.map")
     (magit-todos-exclude-globs . "*.js.map")
     (magit-todos-exclude-globs . *\.js\.map)
     (encoding . utf-8)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))
 '(send-mail-function 'mailclient-send-it)
 '(sml/name-width 35)
 '(sml/shorten-directory t)
 '(sml/shorten-modes t)
 '(tab-width 4)
 '(undo-tree-visualizer-diff t)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))



(setq debug-on-error t)

(transient-mark-mode 1)

(defun fap/copy-keep-highlight (beg end)
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(global-set-key (kbd "M-s-w") 'fap/copy-keep-highlight)

(defun newline-and-indent-anywhere ()
  "Insert a newline character, but from the end of the current line."
  (interactive)
  (progn
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "s-j") 'newline-and-indent-anywhere)

(setq
 org_top (concat (getenv "HOME") "/repos/org")
 org_todo (concat org_top "/todo")
 org_gtd (concat org_todo "/gtd")
 org_roam (concat org_top "/roam")
 org_bib (concat org_roam "/roam.bib") ;; https://github.com/JabRef/jabref
 bib_notes_subdir "/bib_notes"
 org_bib_notes (concat org_roam bib_notes_subdir)
 org_bib_library (concat org_bib_notes "/pdf")
 org_journal (concat org_top "/journal")
 org-directory org_top
 deft-directory org_top
 org-roam-directory org_roam
 )

(use-package org
  :ensure t
  :bind
  (:map org-mode-map
        ("C-j" . newline-and-indent)
        ("C-c SPC" . nil) ;; bound to ~org-table-blank-field~, block the avy keybind
        )
  :init
  (setq org-startup-indented t)
  :config
  (setq org-startup-with-inline-images t)
  (setq org-refile-targets `((nil :level . 3)
                             (,(concat org_gtd "/gtd.org") :maxlevel . 3)
                             (,(concat org_gtd "/someday.org") :maxlevel . 2)
                             (,(concat org_todo "/calendar.org") :maxlevel . 2)))
  ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w!)" "NEXT(n!)" "IDEA(i)" "GOAL(g)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-comment-string "BACKBURNER")
  (require 'ox-taskjuggler)
  (setq org-taskjuggler-target-version 3.7))


(use-package org-contrib
  :after org
  :ensure t)

(require 'org-columns-calc)

(use-package org-tempo
  ;; contains old template expansion syntax: <s
  ;; https://orgmode.org/manual/Structure-Templates.html
  :after org)

;; if you yank something coming from a programming major-mode: surround yank with source block, language is automatically specified
(use-package org-rich-yank
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-web-tools
  :ensure t)

;; https://colekillian.com/posts/org-pomodoro-and-polybar/
(use-package org-pomodoro
  :ensure t
  :config
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   org-pomodoro-clock-break t))

(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (outline-show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    )
  )

;; https://github.com/raamdev/dotfiles/blob/aff0a369fdc31de57914a7b8b030bda506f7a971/emacs.d/config/custom-functions.el#L86
;;-----------------------------------------------------------------
;; See http://punchagan.muse-amuse.in/posts/refile-to-date-tree.html
(defun my/org-refile-to-journal ()
  "Refile an entry to journal file's date-tree"
  (interactive)
  (require 'org-datetree)
  (let ((journal (expand-file-name org-journal-file org-directory))
	post-date)
    (setq post-date (or (org-entry-get (point) "TIMESTAMP_IA")
			(org-entry-get (point) "TIMESTAMP")))
    (setq post-date (nthcdr 3 (parse-time-string post-date)))
    (setq post-date (list (cadr post-date)
			  (car post-date)
			  (caddr post-date)))
    (org-cut-subtree)
    (with-current-buffer (or (find-buffer-visiting journal)
			     (find-file-noselect journal))
      (save-excursion
	(org-datetree-file-entry-under (current-kill 0) post-date)
	(bookmark-set "org-refile-last-stored")))
    (message "Refiled to %s" journal))
  (setq this-command 'my/org-refile-to-journal)) ;; See http://emacs.stackexchange.com/q/21322/8494

;; Insert org-mode links from clipboard
(use-package org-cliplink
  :ensure t
  :bind ("C-x p i" . org-cliplink)
  :config
  (defalias 'org-insert-link-from-clipboard 'org-cliplink))

(defun my/org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))

;; seems broken, reference: https://emacs.stackexchange.com/a/29413/11806
(defun org-read-datetree-date (d)
  "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))
(defun org-refile-to-archive-datetree ()
  "Refile an entry to a datetree under an archive."
  (interactive)
  (require 'org-datetree)
  (let ((datetree-date (org-read-datetree-date (org-read-date t nil))))
    (org-refile nil nil (list nil (buffer-file-name) nil
                              (save-excursion
                                (org-datetree-find-date-create datetree-date)))))
  (setq this-command 'org-refile-to-journal))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(setq flyspell-issue-message-flag nil)

(use-package org-preview-html
  :ensure t)

(use-package org-download
  :ensure t)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((ruby . t)
        (java . t)))

(use-package ox-pandoc
  :ensure t
  :config
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex"))))

(use-package ox-hugo
  :ensure t
  :after ox)

(setq org-agenda-files '("~/repos/org/todo" "~/repos/org/todo/gtd" "~/repos/org/journal"))

;; (use-package org-brain
;;   :ensure t)

(use-package org-superstar
  :ensure t)

;; https://github.com/Fuco1/org-pretty-table/blob/master/org-pretty-table.el

(use-package deft
  :ensure t
  :bind (("C-c n n" . deft))
  :commands (deft)
  :config
  (setq deft-extension "org")
  (setq deft-directory org-directory)
  (setq deft-text-mode 'org-mode)
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (add-to-list 'deft-extensions "tex")
  ;; converts the filter string into a readable file-name using kebab-case:
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

;; (defun org-journal-find-location ()
;;   ;; Open today's journal, but specify a non-nil prefix argument in order to
;;   ;; inhibit inserting the heading; org-capture will insert the heading.
;;   (org-journal-new-entry t)
;;   (org-narrow-to-subtree)
;;   (goto-char (point-max)))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir org_journal
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline (lambda () (concat org_gtd "/inbox.org")) "Tasks")
                               "* TODO %i%?")
                              ("C" "Calendar" entry
                               (file+headline (lambda () (concat org_todo "/calendar.org")) "Termine")
                               "* TODO %?\nSCHEDULED: %^t")
                                ("c" "Current file todo entry" entry
                                 (file+datetree buffer-file-name)
                                 "* TODO %? \n%t")
                                ("j" "Journal entry" entry
                                 (function org-journal-find-location)
                                 "* %(format-time-string org-journal-time-format)%^{Title}\n\
  %i%?")
                                ("s" "Scheduled journal entry" entry
                                 (function org-journal-find-location)
                                 "* TODO %^{Title}\n\
   SCHEDULED: %^t\n\
  %i%?")
                                ;; TODO(FAP): embed or link to image/pdf of feelings and needs for offline-use
                                ("n" "Daily NVC practice" entry
                                 (function org-journal-find-location)
                                 "* Daily NVC practice :daily_nvc:\n\
%i    | Thought / Judgment | Observation | Feeling | Need | Request |\n\
%i    |--------------------+-------------+---------+------+---------|\n\
%i    |%?                    |             |         |      |         |\n")
                                ("m" "Morning journal entry" entry
                                 (function org-journal-find-location)
                                 "* Morning entry\n\
** 5-minute journal :5min_journal:5min_journal_morning:\n\
*** I am grateful for...\n\
%i    1. %?\n\
%i    2. \n\
%i    3. \n\
*** What would make today great?\n\
%i    1. \n\
%i    2. \n\
%i    3. \n\
*** Daily affirmations, I am...\n\
%i    1. \n\
** Day plan")
                                ("e" "Evening journal entry" entry
                                 (function org-journal-find-location)
         "* Evening entry\n\
** 5-minute journal :5min_journal:5min_journal_evening:\n\
*** 3 amazing things that happened today...\n\
%i    1. %?\n\
%i    2. \n\
%i    3. \n\
*** How could I have made today better?
%i    1. \n\
")))

(use-package org-ql
  :ensure t)

(use-package org-agenda-property
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :init
  (setq org-agenda-custom-commands (list
                                    '("z" "Zuper agenda view" ;; from https://github.com/zaen323/.spacemacs.d/blob/de49a1882881198586f5e848d9281d48b030c598/config-org.el#L77
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-property-list '("LOCATION"))
                                                   (org-agenda-property-position 'where-it-fits)
                                                   (org-agenda-property-separator "|" )
                                                   (org-super-agenda-groups
                                                    '((:discard (:tag "private"))
                                                      (:name "Today"
                                                             :time-grid t
                                                             :date today
                                                             :todo "TODAY"
                                                             :scheduled today
                                                             :order 1)
                                                      (:name "Due Today"
                                                             :deadline today
                                                             :order 2)
                                                      (:name "Overdue"
                                                             :deadline past
                                                             :order 3)
                                                      (:name "Due Soon"
                                                             :deadline future
                                                             :order 4)
                                                      ))))
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-agenda-property-list '("LOCATION"))
                                                    (org-agenda-property-position 'where-it-fits)
                                                    (org-agenda-property-separator "|" )
                                                    (org-super-agenda-groups
                                                     '((:discard (:tag "private"))
                                                       (:name "WORKING ON"
                                                              :todo "WORKING"
                                                              :order 0)
                                                       (:name "NEXT TO DO"
                                                              :todo "NEXT"
                                                              :order 1)
                                                       (:name "GOALS"
                                                              :todo "GOAL"
                                                              :order 2)
                                                       (:name "IDEAS"
                                                              :todo "IDEA"
                                                              :order 3)
                                                       (:name "Important"
                                                              :tag "Important"
                                                              :priority "A"
                                                              :order 6)
                                                       (:name "Waiting"
                                                              :todo "WAITING"
                                                              :order 9)
                                                       (:name "Assignments"
                                                              :tag "Assignment"
                                                              :order 10)
                                                       (:name "Pending"
                                                              :todo "PENDING"
                                                              :order 11)
                                                       (:name "Issues"
                                                              :tag "Issue"
                                                              :order 12)
                                                       (:name "Emacs"
                                                              :tag "Emacs"
                                                              :order 13)
                                                       (:name "Linux"
                                                              :tag "Linux"
                                                              :order 14)
                                                       (:name "Projects"
                                                              :tag "Project"
                                                              :order 91)
                                                       (:name "Research"
                                                              :tag "Research"
                                                              :order 15)

                                                       (:name "Piano"
                                                              :tag "Piano"
                                                              :order 25)
                                                       (:name "Guitar"
                                                              :tag "Guitar"
                                                              :order 26)

                                                       (:name "Kerbal Space Program"
                                                              :tag "KSP"
                                                              :order 29)

                                                       (:name "To Remember"
                                                              :tag "Remember"
                                                              :order 30)
                                                       (:name "To read"
                                                              :and (:tag ("Read" "Book")
                                                                         :not (:todo "SOMEDAY"))
                                                              :order 35
                                                              )

                                                       (:name "Mathematics"
                                                              :tag "Maths"
                                                              :order 40)
                                                       (:name "Science"
                                                              :tag ("Science" "Physics")
                                                              :order 41)

                                                       (:name "trivial"
                                                              :priority<= "C"
                                                              :tag ("Trivial" "Unimportant")
                                                              :todo ("SOMEDAY" )
                                                              :order 90)
                                                       (:discard (:tag ("Chore" "Routine" "Daily")))
                                                       ))

                                                    ))
                                       )) ; Zuper agenda view
                                    '("h" "Habits" ((agenda "" ((org-super-agenda-groups
                                                                 '(
                                                                   (:name "Habits" :habit t)
                                                                   ))))))
                                    '("w" "Next tasks at work " tags-todo "@work"
                                      ((org-agenda-overriding-header "Work")
                                       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                                    '("W" "All tasks at work" tags-todo "@work"
                                      ((org-agenda-overriding-header "Work")
                                       ))
                                    ))
  :config
  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (org-super-agenda-mode t)
  )

(use-package org-roam
  :ensure t
  :after org
  :hook
  (org-load . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam-buffer-toggle)
               ("C-c n f" . org-roam-node-find)
               ("C-c n g" . org-roam-graph)
               ("C-c n d" . org-roam-dailies-goto-today))
              :map org-mode-map
              (("C-c n i" . org-roam-node-insert)))
  :config
  (setq org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
        org-roam-verbose nil ; make org-roam quiet
        ;; https://www.youtube.com/watch?v=1q9x2aZCJJ4
        org-roam-directory org_roam
        )
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-enable)
  ;; https://ag91.github.io/blog/2021/03/12/find-org-roam-notes-via-their-relations/
  (defun my/navigate-note (arg &optional node choices)
    "Navigate notes by link. With universal ARG tries to use only to navigate the tags of the current note. Optionally takes a selected NOTE and filepaths CHOICES."
    (interactive "P")
    (let* ((depth (if (numberp arg) arg 1))
           (choices
            (or choices
                (when arg
                  (-map #'org-roam-backlink-target-node (org-roam-backlinks-get (org-roam-node-from-id (or (ignore-errors (org-roam-node-id node))
                                                                                                           (org-id-get-create))))))))
           (all-notes (org-roam-node-read--completions))
           (completions
            (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
           (next-node
            ;; taken from org-roam-node-read
            (let* ((nodes completions)
                   (node (completing-read
                          "Node: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                '(metadata
                                  (annotation-function . (lambda (title)
                                                           (funcall org-roam-node-annotation-function
                                                                    (get-text-property 0 'node title))))
                                  (category . org-roam-node))
                              (complete-with-action action nodes string pred))))))
              (or (cdr (assoc node nodes))
                  (org-roam-node-create :title node)))
            )
           )
      (if (equal node next-node)
          (org-roam-node-visit node)
        (my/navigate-note nil next-node (cons next-node (-map #'org-roam-backlink-source-node (org-roam-backlinks-get next-node))))))))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package org-roam-protocol
  :after org-protocol)

(use-package org-pdftools
  :after org
  :ensure t
  :init
  (pdf-tools-install) ;; see https://github.com/politza/pdf-tools/issues/288
  :config
  ;; see https://github.com/stardiviner/emacs.d/blob/d149a4cb0f4520c92b4f3f9564db1e542d571d2c/init/Emacs/init-emacs-pdf.el#L58-L60
  (add-hook 'pdf-view-mode-hook #'pdf-annot-minor-mode)
  (add-hook 'pdf-view-mode-hook (lambda () (read-only-mode 0)))
  ;; save after adding annotation comment
  (advice-add 'pdf-annot-edit-contents-commit :after 'save-buffer)
  (org-pdftools-setup-link) ;; make pdf:/ links work for annotations
  )

(use-package pdfgrep
  :ensure t
  :defer t
  :after pdf-tools
  :commands (pdfgrep pdfgrep-mode)
  :hook (pdf-view-mode . pdfgrep-mode)
  :config
  (defun insert-current-buffer-file (cmd)
    "Add current buffer file name as PATTERN."
    (cond ((and (equal major-mode 'pdf-view-mode) (listp cmd) (numberp (cdr cmd))) ;; pdfgrep-ignore-errors is t
           (cons (concat (car cmd) " " buffer-file-name) (cdr cmd)))
          ((equal major-mode 'pdf-view-mode)
           (cons (concat cmd " " buffer-file-name) (1+ (length cmd))))
          (t cmd)))
  (advice-add 'pdfgrep-default-command :filter-return 'insert-current-buffer-file))

(use-package org-noter
  :ensure t
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_bib_notes)
   org-noter-auto-save-last-location t
   ))

(use-package org-noter-pdftools
  :ensure t
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-ref
  :ensure t
  :after org-roam
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   ;;   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex
   org-ref-default-bibliography (list org_bib)
   org-ref-pdf-directory org_bib_library
   org-ref-bibliography-notes (concat org_bib_notes "/bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory org_bib_notes)
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
	       (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
	      (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
  ;; (org-ref-ivy-cite-completion) void-function since v3 of org-ref
  )

(use-package ivy-bibtex
  :ensure t
  :after org-ref
  :bind ("C-c n b" . ivy-bibtex)
  :config
  (setq
   bibtex-completion-notes-path org_bib_notes
   bibtex-completion-bibliography org_bib
   bibtex-completion-library-path org_bib_library
;;   bibtex-completion-library-path (concat (getenv "HOME") "/Documents/ebooks")
   bibtex-completion-pdf-field "file"
   ivy-bibtex-default-action 'ivy-bibtex-edit-notes
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_REFS: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ;; this fails instead of leaving the property empty if no file is associated
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n" ;; always add "pdf:" prefix?
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam ivy-bibtex)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions)))
  :config
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  (setq org-roam-capture-templates
        `(("r" "bibliography reference" plain
           (file ,(concat org_bib_notes "/bib_ref_template.org"))
           :if-new
           (file+head ,(concat org_bib_notes "/${citekey}.org") "#+title: ${title}\n"))
          ("d" "default" plain "%?" :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
")
           :unnarrowed t)))
  (org-roam-bibtex-mode)
  :diminish org-roam-bibtex-mode)

(use-package org-kanban
  :ensure t)

(use-package writeroom-mode
  :ensure t)

;; Source: https://github.com/novoid/dot-emacs/blob/d189ee0ff415fc1fdca5b0f3f3519d755011c656/config.org#thunderbird
(setq thunderlink-program (concat (getenv "HOME") "/cb_thunderlink/cb_thunderlink"))

(defun my-open-message-id-in-thunderbird (message-id)
  "open an email with a given message-ID in Thunderbird"
  (interactive)
  (start-process
   (concat "cbthunderlink: " message-id)
   nil
   thunderlink-program
   (concat "cbthunderlink:" message-id)
   )
  )

(defun org-message-thunderlink-open (slash-message-id)
  "Handler for org-link-set-parameters that converts a standard message:// link into
   a thunderlink and then invokes thunderbird."
  ;; remove any / at the start of slash-message-id to create real message-id
  ;; FAP: this doesn't do anything? Input is "//<messageid>", nothing get's cut off
  (let ((message-id
         (replace-regexp-in-string (rx bos (* ":"))
                                   ""
                                   slash-message-id)))
    (my-open-message-id-in-thunderbird message-id)
    ))

;; on message://aoeu link, this will call handler with //aoeu
(org-link-set-parameters "cbthunkderlink" :follow #'org-message-thunderlink-open)

;; https://www.youtube.com/watch?v=vO_RF2dK7M0 <- still out of date but shows google setup
;; https://cestlaz.github.io/posts/using-emacs-26-gcal/
;; https://github.com/kidd/org-gcal.el#Installation
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-file-alist `(("fabian.pfaff@vogella.com" .  ,(concat org_todo "/vogella_gcal.org")))
        org-gcal-remove-api-cancelled-events t
        fap//org-gcal--warning-period "-1d")
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
  ;;  https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
  (load-library "~/repos/dotfiles/emacs/org-gcal-secrets.el.gpg")
  (defun fap//org-gcal-add-warning-period (_calendar-id event _update-mode)
    "Add a warning period to the plain timestamp in the gcal drawer. Warning periods for plain timestamps are supported by Orgzly."
    (when (not (org-gcal--event-cancelled-p event))
      (org-back-to-heading)
      (org-narrow-to-element)
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
             (point-max)
             'noerror)
        (forward-line 1)
        (when
            (re-search-forward org-element--timestamp-regexp (point-at-eol) 'noerror)
          (replace-match (concat "<" (match-string 1) " " fap//org-gcal--warning-period ">"))))))
  (add-hook 'org-gcal-after-update-entry-functions #'fap//org-gcal-add-warning-period))

;; (use-package oauth2
;;   :ensure t)

;; (use-package plstore
;;   :defer t
;;   :config
;;   (setq plstore-cache-passphrase-for-symmetric-encryption t))

;; (use-package org-caldav
;;   :ensure t
;;   :init
;;   (setq org-caldav-url 'google
;;           org-caldav-calendar-id "fabian.pfaff@vogella.com"
;;           org-icalendar-timezone "Europe/Berlin"
;;           org-caldav-sync-direction 'cal->org
;;           org-caldav-inbox (concat org_todo "/vogella_caldav_gcal.org")
;;           org-caldav-save-directory org_todo)
;;   (load-library "~/repos/dotfiles/emacs/org-gcal-secrets.el.gpg")
;;   :config
;;   (setq org-icalendar-alarm-time 60)
;;   (setq org-icalendar-include-todo t)
;;   (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;;   (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)))

;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :ensure t)
(use-package calfw-org
  :ensure t)
(use-package calfw-ical
  :ensure t)


(defun fap/my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
;;    (cfw:howm-create-source "Blue")  ; howm source
;;    (cfw:cal-create-source "Orange") ; diary source
;;    (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
;;    (cfw:ical-create-source "gcal" "~/repos/org/todo/fabian.pfaff@vogella.com.ics" "IndianRed") ; google calendar ICS
    (cfw:ical-create-source "gcal" "~/repos/org/todo/socialhackspace.ics" "Blue") ; google calendar ICS
   )))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode t)
  :diminish activity-watch-mode)

(use-package spacemacs-theme
  :ensure t
  :defer t
  :config
  (progn
    (defun customize-spacemacs-light ()
      "Customize spacemacs-light theme"
      (if (member 'spacemacs-light custom-enabled-themes)
          (let* ((bg-white           "#fbf8ef")
                 (bg-light           "#222425")
                 (bg-dark            "#1c1e1f")
                 (bg-darker          "#1c1c1c")
                 (fg-white           "#ffffff")
                 (shade-white        "#efeae9")
                 (fg-light           "#655370")
                 (dark-cyan          "#008b8b")
                 (region-dark        "#2d2e2e")
                 (region             "#39393d")
                 (slate              "#8FA1B3")
                 (keyword            "#f92672")
                 (comment            "#525254")
                 (builtin            "#fd971f")
                 (purple             "#9c91e4")
                 (doc                "#727280")
                 (type               "#66d9ef")
                 (string             "#b6e63e")
                 (gray-dark          "#999")
                 (gray               "#bbb")
                 (sans-font          "Source Sans Pro")
                 (serif-font         "Merriweather")
                 (et-font            "ETBembo")
                 (sans-mono-font     "Souce Code Pro")
                 (serif-mono-font    "Verily Serif Mono"))
            (custom-theme-set-faces
             'spacemacs-light
             `(variable-pitch
               ((t
                 (:family ,et-font
                          :background nil
                          :foreground ,bg-dark
                          :height 1.7))))))))
    (add-hook 'after-load-theme-hook 'customize-spacemacs-light)))


(defun fap/activate-presentation ()
  (interactive)
  (progn
    (writeroom-mode t)
    (rogue/org-tweaks)
    (org-display-inline-images)
    (load-theme 'spacemacs-light)))

(defun rogue/org-tweaks ()
    (setq org-startup-indented t
          org-superstar-headline-bullets-list '(" ") ;; no bullets, needs org-bullets package
          org-ellipsis "  " ;; folding symbol
          org-pretty-entities t
          org-hide-emphasis-markers t ;; show actually italicized text instead of /italicized text/
          org-agenda-block-separator ""
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t))














(use-package htmlize
  :ensure t)

;; source: https://emacs.stackexchange.com/a/14987/11806
(defun my/open-tree-view ()
  "Open a clone of the current buffer to the left, resize it to 30 columns
, and bind <mouse-1> to jump to the same position in the base buffer."
  (interactive)
  (let ((new-buffer-name (concat "<tree>" (buffer-name))))
    ;; Create tree buffer
    (split-window-right 30)
   ;; (setq org-last-indirect-buffer (get-buffer (buffer-name)))
    (if (get-buffer new-buffer-name)
        (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
      ;; Make new tree buffer
      (progn
        (clone-indirect-buffer new-buffer-name nil t)
              (switch-to-buffer new-buffer-name)
              (outline-show-all)
              (outline-hide-body)
              (read-only-mode)
              (toggle-truncate-lines)

              ;; Do this twice in case the point is in a hidden line
              (dotimes (_ 2 (forward-line 0)))

              ;; Map keys
              ;;(use-local-map (copy-keymap outline-mode-map))
              ;;(local-set-key (kbd "q") 'delete-window)
              (mapc (lambda (key) (local-set-key (kbd key) 'org-tree-open-in-right-frame))
                    '("RET"))
              (mapc (lambda (key) (local-set-key (kbd key) 'my/jump-to-point-and-show))
                    '("<C-M-m"))))))

(add-to-list 'special-display-regexps '("<tree>.*" my-display-buffers))

(defun my-display-buffers (buf)
  "put all buffers in a window other than the one in the bottom right"
  (let ((window-list (window-list nil nil (frame-first-window))))
  (print (length window-list))
    (if (<= 3 (length window-list))
        (progn
          (print "if block")
          (select-window (car window-list))
          (split-window-vertically)))
    (let ((pop-up-windows t))
      (print "else block")
      (set-window-buffer (nth 1 window-list) buf)
      (nth 1 window-list))))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun org-tree-open-in-right-frame ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(setq org-indirect-buffer-display 'other-window)

(defun my/jump-to-point-and-show ()
  "Switch to a cloned buffer's base buffer and move point to the cursor position in the clone."
  (interactive)
  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))
      (goto-char pos)
      (when (invisible-p (point))
        (show-branches)))))

(defun my/org-narrow-to-here ()
   (interactive)
   (org-narrow-to-subtree)
   (save-excursion
     (org-next-visible-heading 1)
     (narrow-to-region (point-min) (point))))

;; Original Source: https://github.com/chenfengyuan/elisp/blob/1e3d37b168f3ee95c0608a8c300bce79b6d0ef77/next-spec-day.el
(defvar next-spec-day-runingp)
(setq next-spec-day-runningp nil)
(defvar next-spec-day-alist
  '((last-workday-of-month
     .
     ((or
       (and (= (calendar-last-day-of-month m y) d) (/= (calendar-day-of-week date) 0) (/= (calendar-day-of-week date) 6))
       (and (< (- (calendar-last-day-of-month m y) d) 3) (string= (calendar-day-name date) "Friday")))))
    (last-day-of-month
     .
     ((= (calendar-extract-day date) (calendar-last-day-of-month (calendar-extract-month date) (calendar-extract-year date)))))
    (fathers-day
     .
     ((diary-float 6 0 3))))
  "Contains some useful sexp.")
(defun next-spec-day ()
  "Function to automagically generate the next date for a given diary sexp expression.
How to use:
1. add code to your dot Emacs file.
2. set `NEXT-SPEC-TIMESTAMP`, `NEXT-SPEC-DEADLINE` and/or `NEXT-SPEC-SCHEDULED` property of a TODO task,like this:
        * TODO test
          SCHEDULED: <2013-06-16 Sun> DEADLINE: <2012-12-31 Mon -3d>
          :PROPERTIES:
          :NEXT-SPEC-TIMESTAMP: (diary-float t 4 2)
          :NEXT-SPEC-DEADLINE: (= (calendar-extract-day date) (calendar-last-day-of-month (calendar-extract-month date) (calendar-extract-year date)))
          :NEXT-SPEC-SCHEDULED: (diary-float 6 0 3)
          :END:
          TIMESTAMP: <2022-09-25 Sun 13:07>
    The value of NEXT-SPEC-DEADLINE will return `non-nil` if `date` is last day of month,and the value of NEXT-SPEC-SCHEDULED will return `non-nil` if `date` is the fathers' day(the third Sunday of June).
3. Then, when you change the TODO state of that task, the timestamp will be changed automatically (including lead time of warnings settings).
Notes:
1. Execute `(setq next-spec-day-runningp nil)' after your sexp signal some erros,
2. You can also use some useful sexp from next-spec-day-alist, like:
* TODO test
  SCHEDULED: <2013-03-29 Fri>
  :PROPERTIES:
  :NEXT-SPEC-SCHEDULED: last-workday-of-month
  :END:"
  (unless next-spec-day-runningp
    (setq next-spec-day-runningp t)
    (catch 'exit
      (dolist (type '("NEXT-SPEC-TIMESTAMP" "NEXT-SPEC-DEADLINE" "NEXT-SPEC-SCHEDULED"))
	    (when (stringp (org-entry-get nil type))
	      (let* ((time (org-entry-get nil (substring type (length "NEXT-SPEC-"))))
                 (repeater (and time
			                    (string-match "\\([.+-]+[0-9]+[hdwmy] ?\\)+" time)
			                    (match-string 0 time)))
		         (pt (if time (org-parse-time-string time) (decode-time (current-time))))
		         (func (ignore-errors (read-from-whole-string (org-entry-get nil type)))))
	        (unless func (message "Sexp is wrong") (throw 'exit nil))
	        (when (symbolp func)
	          (setq func (cadr (assoc func next-spec-day-alist))))
	        (cl-incf (nth 3 pt))
	        (setf pt (decode-time (apply 'encode-time pt)))
	        (cl-do ((i 0 (1+ i)))
		        ((or
		          (> i 1000)
		          (let* ((d (nth 3 pt))
			             (m (nth 4 pt))
			             (y (nth 5 pt))
			             (date (list m d y))
			             entry)
		            (calendar-dlet ((date date)
                                    (entry entry))
                      (eval func))))
		         (if (> i 1000)
		             (message "No satisfied in 1000 days")
                   (if (or (string= "NEXT-SPEC-DEADLINE" type) (string= "NEXT-SPEC-SCHEDULED" type))
		               (funcall
		                (if (string= "NEXT-SPEC-DEADLINE" type)
			                'org-deadline
		                  'org-schedule)
		                nil
		                (format-time-string
		                 (if (and
			                  time
			                  (string-match
			                   "[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
			                   time))
			                 (cdr org-time-stamp-formats)
		                   (car org-time-stamp-formats))
		                 (apply 'encode-time pt)))
                     (and
                      (org-back-to-heading)
                      (when (re-search-forward
                             "^\s*TIMESTAMP:"
                             (point-max)
                             'noerror)
                        (when
                            (re-search-forward org-element--timestamp-regexp (point-at-eol) 'noerror)
                          (replace-match (save-match-data (format-time-string
		                                  (if (and
			                                   time
			                                   (string-match
			                                    "[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
			                                    time))
			                                  (cdr org-time-stamp-formats)
		                                    (car org-time-stamp-formats))
		                                  (apply 'encode-time pt))))
                          (when repeater
                            (backward-char)
                            (insert " " repeater))))))))
	          (cl-incf (nth 3 pt))
	          (setf pt (decode-time (apply 'encode-time pt)))))))
      (if (or
	       (org-entry-get nil "NEXT-SPEC-SCHEDULED")
	       (org-entry-get nil "NEXT-SPEC-DEADLINE"))
	      (org-entry-put nil "TODO" (car org-todo-heads))))
    (setq next-spec-day-runningp nil)))

(add-hook 'org-after-todo-state-change-hook 'next-spec-day)

;; TODO(FAP): Write function that works like org-clone-subtree-with-time-shift but for diary-float
;;            Interactively takes a number of copies, creates the copy and
;;            applies next-spec-day n-times to the nth copy


(use-package epresent
  :ensure t)

(use-package org-tree-slide
  :ensure t)

(use-package org-present
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (hide-mode-line-mode t)
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (hide-mode-line-mode 0)
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package hide-mode-line
  :ensure t)

;; step through symbols with Meta + left/right
(use-package auto-highlight-symbol
  :ensure t
  :config
  (define-key auto-highlight-symbol-mode-map (kbd "M--") 'nil)
  (global-auto-highlight-symbol-mode t)
  (ahs-set-idle-interval 0.1)
  (ahs-chrange-display)
  (add-hook 'web-mode-hook 'auto-highlight-symbol-mode)
  (add-hook 'elixir-mode-hook 'auto-highlight-symbol-mode)
  :diminish 'auto-highlight-symbol-mode)

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "s-o") #'imenu-list-smart-toggle)
  (setq imenu-list-focus-after-activation t))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay "0.075"))

(use-package smart-semicolon
  :ensure t
  :config
  (add-hook 'web-mode-hook  'smart-semicolon-mode)
  (add-hook 'c-mode-hook  'smart-semicolon-mode)
  (add-hook 'java-mode-hook 'smart-semicolon-mode))

(use-package embrace
  :ensure t)

;; edit results inside grep result buffer
;; activate with C-c C-p, save with C-x C-s
(use-package wgrep-ag
  :ensure t)

;; basically rename refactoring with C-;
(use-package iedit
  :ensure t
  :config
  (defalias 'rename-variable 'iedit-mode))


(use-package fuzzy
  :ensure t)

;; trigger completion with C-M-i (traditional completion-at-point binding)
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t
          ;; after typing of how many characters are we auto suggesting completions
          company-minimum-prefix-length 3)
    (setq company-dabbrev-downcase nil)
    (add-to-list 'company-backends 'company-capf))
  :diminish company-mode)

(use-package company-box
  :ensure t
  :custom-face
  (company-tooltip-selection ((t (:inherit highlight))))
  :config
  (setq company-box-doc-delay 0.1)
  :hook (company-mode . company-box-mode)
  :diminish company-box-mode)

;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (require 'auto-complete-config)
;;   (global-auto-complete-mode +1)
;;   :config
;;   (setq ac-auto-show-menu t
;; 		ac-quick-help-delay 0.2
;;         ac-auto-start 0
;; 		ac-use-fuzzy t
;;         ac-show-menu-immediately-on-auto-complete t)
;;   (define-key ac-complete-mode-map (kbd "M-x") 'execute-extended-command)
;;   (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
;;   (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
;;   (define-key ac-complete-mode-map (kbd "C-g") 'ac-stop)
;;   ;; trigger auto-complete on delete
;;   (define-key ac-complete-mode-map (kbd "DEL")
;;     (lambda ()
;;       (interactive)
;;       (backward-delete-char-untabify 1)
;;       (ac-start)))
;;   (provide 'auto-complete-config)
;;   ;; deactivate company-mode so it can't interfe with ac completion
;;   (setq company-backends nil))

;; (use-package ac-dabbrev
;;   :ensure t
;;   :config
;;   (global-set-key "\M-/" 'ac-dabbrev-expand)
;;   (defun ac-dabbrev-expand ()
;;     (interactive)
;;     (auto-complete '(ac-source-dabbrev)))
;;   (setq ac-dabbrev-sort t))

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
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

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

;; provides functions to convert string to symbol etc.
(use-package ruby-tools
  :ensure t
  :config
  (define-key ruby-tools-mode-map (kbd "C-;") nil))

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
  :after company
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
  :init
  (yas-global-mode 1)
  ;; show dropdown menu if more than one snippet available for keyword
  (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt)))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet company)
  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package rvm
  :ensure t
  :init
  (rvm-activate-corresponding-ruby)
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package rbenv
  :ensure t
  :init
  (global-rbenv-mode))

(use-package rspec-mode
  :ensure t
  :init
    (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(use-package ruby-test-mode
  :ensure t
  :after ruby-mode
  :diminish ruby-test-mode
  :config
  (defun amk-ruby-test-pretty-error-diffs (old-func &rest args)
    "Make error diffs prettier."
    (let ((exit-status (cadr args)))
      (apply old-func args)
      (when (> exit-status 0)
        (diff-mode)
        ;; Remove self
        (advice-remove #'compilation-handle-exit #'amk-ruby-test-pretty-error-diffs))))
  (defun amk-ruby-test-pretty-error-diffs-setup (old-func &rest args)
    "Set up advice to enable pretty diffs when tests fail."
    (advice-add #'compilation-handle-exit :around #'amk-ruby-test-pretty-error-diffs)
    (apply old-func args))
  (advice-add #'ruby-test-run-command :around #'amk-ruby-test-pretty-error-diffs-setup))

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

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :after helpful
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-S-s") 'swiper-all)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c G") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c K") 'counsel-projectile-rg)
  (global-set-key (kbd "C-c p") 'counsel-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c P") 'counsel-projectile-switch-project)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ;; source: http://oremacs.com/2016/01/06/ivy-flx/
  ;; https://github.com/abo-abo/swiper/issues/830#issuecomment-267330841
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (ivy-bibtex . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))
  ;; https://oremacs.com/2017/08/04/ripgrep/
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(use-package ivy-historian
  :ensure t)

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

(use-package flx
  :ensure t)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package fzf
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; special setup needed after Ruby 2.7: https://ict4g.net/adolfo/notes/admin/inf-ruby-in-emacs-tips-and-tricks.html
;; https://github.com/nonsequitur/inf-ruby/issues/129
;; need to add pry, pry-doc (optional) and webrick to the GEMFILE for robe to work
(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  ;; use robe for smarter company-mode
  (eval-after-load 'company
    '(push 'company-robe company-backends)))



(use-package tide
  :ensure t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2))


(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.slim\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; activate web-mode for plain html
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-pairing nil)
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  :init
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(setq js-indent-level 2)

;; powerful html and css snippet expand
;; https://github.com/smihica/emmet-mode#html-abbreviations
;; https://github.com/smihica/emmet-mode#css-abbreviations
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  ;; emmet-expand-line overwrites newline,
  ;; C-Return is also mapped to this
  (define-key emmet-mode-keymap "\C-j" nil))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

;; (use-package prettier-js
;;   :ensure t
;;   :config
;;   (add-hook 'web-mode-hook #'(lambda ()
;;                                (enable-minor-mode
;;                                 '("\\.jsx?\\'" . prettier-js-mode))))
;;   (add-hook 'js-mode-hook 'prettier-js-mode))

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(use-package dart-mode
  :ensure-system-package (dart_language_server . "pub global activate dart_language_server")
  :ensure t
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "/home/fap/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :ensure t
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/home/fap/flutter/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :ensure t
  :config
  (flutter-l10n-flycheck-setup))

(defun fap/recode-region (start end &optional coding-system)
  "Replace the region with a recoded text."
  (interactive "r\n\zCoding System (utf-8): ")
  (setq coding-system (or coding-system 'utf-8))
  (let ((buffer-read-only nil)
	    (text (buffer-substring start end)))
    (delete-region start end)
    (insert (decode-coding-string (string-make-unibyte text) coding-system))))

;; elixir integration
(use-package alchemist
  :ensure t)

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))


;; TODO: this should  probably break at cursor point and offer the existing
;;       behavior (jump to end of line, newline and then pipe) with S-return
(fset 'elixir-pipe-operator-on-newline
   "\C-e\C-j|> ")
(with-eval-after-load 'elixir-mode
     (define-key elixir-mode-map (kbd "<C-return>") 'elixir-pipe-operator-on-newline)
     (define-key elixir-mode-map (kbd "<M-return>") 'elixir-pipe-operator-on-newline))

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

;; (use-package flycheck-credo
;;   :ensure t
;;   :config
;;   (setq flycheck-elixir-credo-strict t)
;;   (eval-after-load 'flycheck
;;     '(flycheck-credo-setup))
;;   (add-hook 'elixir-mode-hook 'flycheck-mode))

;; (use-package elm-mode
;;   :ensure t
;;   :init
;;   ;; needs elm oracle installed, doesn't come with elm install
;; ;;  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
;; ;;  (add-hook 'elm-mode-hook #'elm-oracle-setup-ac)
;;   )

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

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

                                        ;(global-company-mode t)
                                        ;(push 'company-robe company-backends)


;; had to remove /usr/bin/ctags (which was actually etags bundled from Emacs
;; from the path so it would pick up the ctags from snap
(use-package counsel-etags
  :ensure t
  :bind
  ("C-c ." . counsel-etags-find-tag-at-point)
  ("C-c ," . pop-tag-mark)
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :diminish paredit-mode)

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  :diminish editorconfig-mode)

(use-package discover
  :ensure t
  :init
  ;;(global-discover-mode 1)
  )

(use-package discover-my-major
  :ensure t)

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

(use-package ripgrep
  :ensure)

(use-package projectile-ripgrep
  :ensure
  :config
  (defalias 'rg-project 'projectile-ripgrep))
;; also see https://github.com/Wilfred/deadgrep/blob/master/docs/ALTERNATIVES.md
(use-package deadgrep
  :ensure t)

(use-package dumb-jump
  :ensure

  :bind
  ;; the dumb-jump commands are deprecated
  ;; use xpath commands via  M-. and M-, instead
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g b" . dumb-jump-back))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;Fullframe saves your window configuration before displaying the next command in the entire Emacs window. When the command finishes, it restores your previous window configuration.
(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window nil)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package dictionary
  :ensure t)

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up.")

  (defengine project-gutenberg
    "http://www.gutenberg.org/ebooks/search/?query=%s")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine twitter
    "https://twitter.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"))

(use-package google-this
  :ensure t
  :init
  (google-this-mode 1)
  :config
  (global-set-key (kbd "C-x g") 'google-this-mode-submap)
  :diminish 'google-this-mode)

(use-package google-translate
  :ensure t
  :init
  (require 'google-translate-smooth-ui)
  :config
  (setq google-translate-translation-directions-alist
        '(("de" . "en") ("en" . "de")))
  (setq google-translate-enable-ido-completion 't)
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (global-set-key "\C-cT" 'google-translate-query-translate)
  ;; fix https://github.com/atykhonov/google-translate/issues/137, https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;; plantUML for UML generation from text
(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path (concat (getenv "HOME") "/plantuml/plantuml.jar"))
  :config
  ;; puml doesn't support auto-indent
  (add-hook 'plantuml-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; source for css: https://gist.github.com/Dashed/6714393
  :init (setq markdown-command (concat (concat "pandoc -c file:///" (getenv "HOME")) "/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")))

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

;; from better-defaults
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; auto revert mode
(global-auto-revert-mode 1)
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; automatically create parent directory when opening new file
;; http://iqbalansari.me/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(use-package terminal-here
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package ansible
  :ensure t)

(use-package mastodon
  :ensure t)

(use-package w3m
  :ensure t)

;; open links through firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; replaces M-< and M-> (or any key bound to beginning-of-buffer or end-of-buffer) with smarter functions in certain modes
(use-package beginend
  :ensure t
  :diminish beginend-global-mode
  :config
  (beginend-global-mode))

(use-package kbd-mode
  :load-path "~/.emacs.d/lisp/")

(use-package ledger-mode
  :ensure t
  :config
  (setq ledger-reports
        '(("bal budget"     "%(binary) --empty -S -T -f %(ledger-file) bal ^assets:budget")
          ("bal"            "%(binary) -f %(ledger-file) bal --real")
          ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount --real")
          ("bal this year"  "%(binary) -f %(ledger-file) bal -p 'this year' --real")
          ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities --real")
          ("account"        "%(binary) -f %(ledger-file) reg %(account) --real"))))

(use-package flycheck-ledger
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(require 'flycheck-ledger)))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(and window-system (server-start))

(provide '.emacs)
;;; .emacs ends here
