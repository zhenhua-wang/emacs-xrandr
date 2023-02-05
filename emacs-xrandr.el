;;; emacs-xrandr.el --- emacs run xrandr  -*- lexical-binding: t; -*-

(defcustom xrandr-position 'mirror
  "Xrandr screen postion."
  :type '(choice (const :tag "Mirror" mirror)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Left" left)
                 (const :tag "Right" right)))

(defvar xrandr-external-monitor nil
  "Previously enabled external device")

(defun parse-xrander ()
  "Parse xrander output."
  (let* ((xrandr-output (shell-command-to-string "xrandr"))
         (xrandr-lines (split-string xrandr-output "\n"))
         (rx (rx line-start (+ space)
                 (group-n 1 (+ digit))"x"(group-n 2 (+ digit))
                 (+ space)
                 (group-n 3 (+ digit)"."(+ digit))
                 (group-n 4 (? (any "*" " ")))
                 (group-n 5 (? (any "+" " ")))))
         (rxconn (rx (group-n 1 (+ anything))
                     (+ space)
                     "connected"
                     (+ space)))
         (parsed-lines '()))
    (let ((device nil)
          (primary-p nil)
          (width nil)
          (height nil)
          (freq nil)
          (current nil)
          (preferred nil))
      (dolist (line xrandr-lines)
        (cond
         ((string-match rxconn line)
          (progn
            (setq device (match-string 1 line))
            (setq primary-p (string-match "primary" line))))
         ((string-match rx line)
          (progn
            (setq width (match-string 1 line))
            (setq height (match-string 2 line))
            (setq freq (match-string 3 line))
            (setq current (string-trim (match-string 4 line)))
            (setq preferred (match-string 5 line))
            (when (and device width height)
              (if primary-p
                  (when (not (length= current 0)) (push (list device width height preferred "primary") parsed-lines))
                (push (list device width height preferred nil) parsed-lines))))))))
    (nreverse parsed-lines)))

(defun xrandr-get-available-primary-devices (parsed-lines)
  "Get available primary devices."
  (car (cl-remove-if-not (lambda (x) (car (last x))) parsed-lines)))

(defun xrandr-get-available-nonprimary-devices (parsed-lines)
  "Get unique available non-primary devices."
  (cl-remove-if (lambda (x) (car (last x))) parsed-lines))

(defun xrandr-get-device (parsed-lines device)
  "Get all configs for a given device"
  (cl-remove-if-not (lambda (x) (string= (car x) device)) parsed-lines))

(defun xrandr-get-device-available-resolutions (parsed-lines device)
  "Get available resolution for a given device."
  (let ((device-parsed-lines (xrandr-get-device parsed-lines device)))
    (mapcar (lambda (x) (list (format "%sx%s" (cadr x) (caddr x)) (cadddr x)))
            device-parsed-lines)))

(defun xrandr-get-device-preferred-resolution (parsed-lines device)
  "Get the preferred resolution for a given device."
  (let* ((device-parsed-lines (xrandr-get-device parsed-lines device))
         (device-preferred (car (cl-remove-if-not (lambda (x) (string= (cadddr x) "+")) device-parsed-lines))))
    (format "%sx%s"
            (cadr device-preferred)
            (caddr device-preferred))))

(defun xrandr--device-type-annotations (s)
  (let* ((item (assoc s minibuffer-completion-table))
         (ann-primary (car (last item))))
    (when item (concat (propertize " " 'display `(space :align-to center))
                       (when ann-primary (propertize ann-primary 'face 'font-lock-type-face))))))

(defun xrandr--resolution-annotations (s)
  (let* ((item (assoc s minibuffer-completion-table))
         (ann-res (cadr item)))
    (when item (concat (propertize " " 'display `(space :align-to center))
                       (when ann-res (propertize ann-res 'face 'font-lock-type-face))))))

(defun xrandr--device-config (primary-resolution-width
                              primary-resolution-height
                              resolution-width
                              resolution-height)
  (let* ((width-ratio (/ (float primary-resolution-width) resolution-width))
         (height-ratio (/ (float primary-resolution-height) resolution-height))
         (scale-ratio (if (> width-ratio height-ratio) width-ratio height-ratio))
         (scale (format "%fx%f" scale-ratio scale-ratio))
         (pos (pcase xrandr-position
                ('top (format "%dx-%d" 0 primary-resolution-height))
                ('bottom (format "%dx%d" 0 primary-resolution-height))
                ('left (format "-%dx%d" primary-resolution-width 0))
                ('right (format "%dx%d" primary-resolution-width 0))
                ('mirror (let* ((x-offset (if (< width-ratio height-ratio)
                                              (/ (- (* resolution-width scale-ratio)
                                                    primary-resolution-width)
                                                 2.0)
                                            0))
                                (y-offset (if (> width-ratio height-ratio)
                                              (/ (- (* resolution-height scale-ratio)
                                                    primary-resolution-height)
                                                 2.0)
                                            0)))
                           (format "-%dx-%d" x-offset y-offset))))))
    (list scale pos)))

(defun xrandr ()
  "Run xrandr with selected device and resolution."
  (interactive)
  (let* ((parsed-lines (parse-xrander))
         (primary-device (xrandr-get-available-primary-devices parsed-lines))
         (primary-name (car primary-device))
         (primary-resolution-width (string-to-number (cadr primary-device)))
         (primary-resolution-height (string-to-number (caddr primary-device)))
         (completion-extra-properties '(:annotation-function xrandr--device-type-annotations))
         (device-name (completing-read "Available devices: " parsed-lines nil t)))
    (if (string= device-name primary-name)
        (progn
          (start-process-shell-command
           "xrandr" nil (format (concat "xrandr --output %s --primary --mode %dx%d --pos 0x0 --rotate normal"
                                        (when xrandr-external-monitor (concat " --output " xrandr-external-monitor " --off")))
                                primary-name primary-resolution-width primary-resolution-height))
          (setq xrandr-external-monitor nil))
      (let* ((preferred-resolution (xrandr-get-device-preferred-resolution parsed-lines device-name))
             (completion-extra-properties '(:annotation-function xrandr--resolution-annotations))
             (resolution (string-split (completing-read (format-prompt "Available Resolutions"
                                                                       preferred-resolution)
                                                        (xrandr-get-device-available-resolutions parsed-lines device-name)
                                                        nil t nil nil preferred-resolution) "x"))
             (resolution-width (string-to-number (car resolution)))
             (resolution-height (string-to-number (cadr resolution)))
             (device-config (xrandr--device-config primary-resolution-width
                                                   primary-resolution-height
                                                   resolution-width
                                                   resolution-height))
             (scale (car device-config))
             (pos (cadr device-config)))
        (start-process-shell-command
         "xrandr" nil (format "xrandr --output %s --primary --mode %dx%d --pos 0x0 --rotate normal --output %s --mode %dx%d  --scale %s --pos %s --rotate normal"
                              primary-name primary-resolution-width primary-resolution-height
                              device-name resolution-width resolution-height
                              scale pos))
        (setq xrandr-external-monitor device-name)))))

(defun xrandr-set-position (position)
  "Set external screen position interactively."
  (interactive
   (list
    (completing-read
     "External screen postion: "
     '(mirror top bottom left right)
     nil t)))
  (setq xrandr-position (intern position)))

(provide 'emacs-xrandr)

;;; emacs-xrandr.el ends here
