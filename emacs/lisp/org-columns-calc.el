;; source: https://emacs.stackexchange.com/questions/54027/org-column-view-with-date-calculations
(require 'org-colview)

(defcustom my-column-display-alist
  '(("Days since last repeat" . my-days-since)
    ("Days until clear" . my-days-until))
  "Alist of column titles vs value transformation functions."
  :group 'org-column
  :type '(repeat
      (cons
       (string :tag "Column Title")
       (function :tag "Value Transformation Function"))))

(defun my-days-diff (value &optional since)
  "Get time difference from today to VALUE in number of days.
VALUE is a time-stamp, e.g., given as DEADLINE property.
If SINCE is non-nil calculate the difference since VALUE to today."
  (condition-case err
      (let ((date (org-time-string-to-time value)))
    (when date
      (number-to-string (- (time-to-days (time-subtract (and (null since) date) (and since date)))
                   (time-to-days '(0 0 0 0))))))
    (error value)))

(defun my-days-since (value)
  "Get number of days since VALUE.
VALUE is a string that can be decoded by `org-time-string-to-time'."
  (my-days-diff value 'since))

(defun my-days-until (value)
  "Get number of days until VALUE.
VALUE is a string that can be decoded by `org-time-string-to-time'."
  (my-days-diff value))

(defun my-column-display-value-filter (column-title value)
  "Apply transformation function associated with COLUMN-TITLE on VALUE.
The association is definied through `my-column-display-alist'.
If the COLUMN-TITLE is not associated with any entry of `my-column-display-alist'
use the standard formatting for value."
  (let ((fun (cdr-safe (assoc-string column-title my-column-display-alist))))
    (when (functionp fun)
      (funcall fun value))))

(setq org-columns-modify-value-for-display-function #'my-column-display-value-filter)

(setq org-columns-default-format "%ITEM %TODO %LAST_REPEAT(Days since last repeat) %SCHEDULED(Days until clear)")

(defun txt-modtime-function (start end &rest _ignore)
  "Propertize region from START to END with modtime text property.
There are no property changes if the special variable txt-modtime-mode is nil.
Logging the modification time in the buffer without interpreting the log-change
as modification can be done by let-binding `txt-modtime-mode' to nil."
  (when (and
     txt-modtime-mode
     (null buffer-read-only)
     (> (buffer-size) 0))
    (if (eq start end) ;; Killed text.
      (cond ;; This could be improved! (Try to stay on line.)
       ((< (point-min) start)
        (put-text-property (1- start) end 'modtime (current-time)))
       ((< end (point-max))
        (put-text-property start (1+ end) 'modtime (current-time)))
       (t
        (save-restriction
          (widen)
          (txt-modtime-function start end))))
      (put-text-property start end 'modtime (current-time)))))

(define-minor-mode txt-modtime-mode
  "Remember modification time of text stretches."
  :lighter ""
  (if txt-modtime-mode
      (add-hook 'after-change-functions #'txt-modtime-function t t)
    (remove-hook 'after-change-functions #'txt-modtime-function t)
    (save-restriction
      (widen)
      (with-silent-modifications
    (remove-text-properties (point-min) (point-max)
                '(modtime nil))))))

(defun txt-modtime (&optional begin end interactive)
  "Get last modtime in region from BEGIN to END.
If INTERACTIVE is non-nil print modification time in message buffer."
  (interactive (list
        (or (and (use-region-p) (region-beginning))
            (point-min))
        (or (and (use-region-p) (region-end))
            (point-max))
        t))
  (let ((ret
     (cl-loop
      with modtime = (get-text-property begin 'modtime)
      with nextmodtime
      for int being the intervals from begin to end property 'modtime
      if (setq nextmodtime (get-text-property (car int) 'modtime))
      when (or (null modtime)
           (time-less-p modtime nextmodtime))
      do (setq modtime nextmodtime)
      finally return modtime)))
    (when interactive
      (message "Modification time: %s"
           (format-time-string "%F %H:%M %S''+%3Nms" ret)))
    ret))

(defvar org-columns-hook nil
  "Hook called at start of `org-columns'.")

(advice-add #'org-columns :before
        (lambda (&rest _ignore)
          (run-hooks 'org-columns-hook)))

(defun my-org-update-lastupdated ()
  "Update the LASTUPDATED property of all top-level headings."
  (unless buffer-read-only
    (let (pos
      org-modtime-mode)
      (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (or (null pos)
             (>
              (progn
            (org-forward-heading-same-level 1 t)
            (point))
              pos))
        (setq pos (point))
        (let ((modtime
           (txt-modtime (point) (save-excursion (org-end-of-subtree t) (point)))))
          (when modtime
        (org-entry-put (point) "LAST_REPEAT" (format-time-string "%F" modtime))))))))))

(defun my-org-configure-lastupdated ()
  "Configure Org mode to keep the LASTUPDATED up-to-date."
  (txt-modtime-mode)
  (add-hook 'write-contents-functions #'my-org-update-lastupdated nil t)
  (add-hook 'org-columns-hook #'my-org-update-lastupdated nil t))

;; don't update lastupdated automatically
;; (add-hook 'org-mode-hook #'my-org-configure-lastupdated)
(provide 'org-columns-calc)
