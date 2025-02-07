;; don't pop up warnings buffer
(setq native-comp-async-report-warnings-errors 'silent)

;; https://stackoverflow.com/questions/6026713/how-do-i-change-emacs-default-font-size-and-font-type
(when initial-window-system
  (add-to-list 'default-frame-alist '(font . "Hack 9"))

  ;; disable all toolbars and menus
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Show a nice fringe
  (set-fringe-mode '(10 . 0)))

;; Experimenting with fonts:

;; (set-frame-font "Ubuntu Sans Mono 10" nil t)

;; (set-frame-font "DejaVu Sans Mono 9" nil t)

;; (set-frame-font "Inconsolata 11" nil t)

;; (set-frame-font "Cascadia Mono 9" nil t)

;; (set-frame-font "Source Sans 3 11" nil t)

;; (set-frame-font "Red Hat Mono 9" nil t)

;; (set-frame-font "JetBrains Mono 8" nil t)

;; (set-frame-font "Hack 9" nil t)
