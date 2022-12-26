;;; emacs-xrandr.el --- emacs run xrandr  -*- lexical-binding: t; -*-

(defcustom xrandr-position 'mirror
  "Xrandr screen postion."
  :type '(choice (const :tag "Mirror" mirror)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Left" left)
                 (const :tag "Right" right)))

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
                  (when (not (length= current 0)) (push (list device width height "primary") parsed-lines))
                (push (list device width height nil) parsed-lines))))))))
    (nreverse parsed-lines)))

(defun get-available-primary-devices (parsed-lines)
  "Get available primary devices."
  (car (cl-remove-if-not (lambda (x) (car (last x))) parsed-lines)))

(defun get-available-nonprimary-devices (parsed-lines)
  "Get unique available non-primary devices."
  (cl-remove-if (lambda (x) (car (last x))) parsed-lines))

(defun get-device-available-resolutions (parsed-lines device)
  "Get available resolution for a given device."
  (let ((device-parsed-lines (cl-remove-if-not (lambda (x) (string= (car x) device)) parsed-lines)))
    (mapcar (lambda (x) (format "%sx%s" (cadr x) (caddr x)))
            device-parsed-lines)))

(defun xrandr ()
  "Run xrandr with selected device and resolution."
  (interactive)
  (let* ((parsed-lines (parse-xrander))
         (primary-device (get-available-primary-devices parsed-lines))
         (primary-name (car primary-device))
         (primary-resolution-width (string-to-number (cadr primary-device)))
         (primary-resolution-height (string-to-number (caddr primary-device)))
         (device-name (completing-read "Available devices: " (get-available-nonprimary-devices parsed-lines)))
         (resolution (string-split (completing-read "Available resolutions: "
                                                    (get-device-available-resolutions parsed-lines device-name)) "x"))
         (resolution-width (string-to-number (car resolution)))
         (resolution-height (string-to-number (cadr resolution)))
         (width-ratio (/ (float primary-resolution-width) resolution-width))
         (height-ratio (/ (float primary-resolution-height) resolution-height))
         (scale-ratio (if (> width-ratio height-ratio) width-ratio height-ratio))
         (x-offset 0)
         (y-offset 0)
         (offset "0x0"))
    (pcase xrandr-position
      ('top (progn
              (setq x-offset 0
                    y-offset primary-resolution-height
                    offset (format "%dx-%d" x-offset y-offset))))
      ('bottom (progn
                 (setq x-offset 0
                       y-offset primary-resolution-height
                       offset (format "%dx%d" x-offset y-offset))))
      ('left (progn
               (setq x-offset primary-resolution-width
                     y-offset 0
                     offset (format "-%dx%d" x-offset y-offset))))
      ('right (progn
                (setq x-offset primary-resolution-width
                      y-offset 0
                      offset (format "%dx%d" x-offset y-offset))))
      ('mirror (progn
                 (setq x-offset (if (< width-ratio height-ratio)
                                    (/ (- resolution-width
                                          (/ (float primary-resolution-width) scale-ratio))
                                       2.0)
                                  0)
                       y-offset (if (> width-ratio height-ratio)
                                    (/ (- resolution-height
                                          (/ (float primary-resolution-height) scale-ratio))
                                       2.0)
                                  0)
                       offset (format "-%dx-%d" x-offset y-offset)))))
    (start-process-shell-command
     "xrandr" nil (format "xrandr --output %s --primary --mode %dx%d --pos 0x0 --rotate normal --output %s --mode %dx%d  --scale %fx%f --pos %s --rotate normal"
                          primary-name primary-resolution-width primary-resolution-height
                          device-name resolution-width resolution-height
                          scale-ratio scale-ratio offset))))

(defun xrandr-set-position (position)
  "Set external screen position interactively."
  (interactive
   (list
    (completing-read
     "External screen postion: "
     '(mirror top bottom left right))))
  (setq xrandr-position (intern position)))

(provide 'emacs-xrandr)

;;; emacs-xrandr.el ends here
