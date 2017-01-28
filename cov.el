(require 's)

(defgroup gcov nil
  "The group for everything in cov.el")

(defun gcov-l-max (list)
  (eval (cons 'max list)))

(defun gcov-second (list)
  (nth 1 list))

(defgroup gcov-faces nil
  "Faces for gcov."
  :group 'gcov
  :group 'faces)

(defface gcov-heavy-face
  '((((class color)) :foreground "red"))
  "Face used on the fringe indicator for successful evaluation."
  :group 'gcov-faces)

(defface gcov-med-face
  '((((class color)) :foreground "yellow"))
  ;;:group 'gcov
  "Face used on the fringe indicator for successful evaluation."
  :group 'gcov-faces)

(defface gcov-light-face
  '((((class color)) :foreground "green"))
  "Face used on the fringe indicator for successful evaluation."
  :group 'gcov-faces)

(defface gcov-none-face
  '((((class color)) :foreground "blue"))
  "Face used on the fringe indicator for no evaluation."
  :group 'gcov-faces)

(defvar gcov-high-threshold .85)
(defvar gcov-med-threshold .45)
(defvar gcov-overlays '())

(defun read-lines (file-path)
  "Return a list of lines"
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t nil)))

(defun gcov-read (file-path)
  "Read a gcov file, filter unused lines, and return a list of lines"
  (remove-if-not
   (lambda (str)
     (s-matches? "[0-9#]+:" (s-left 6 (s-trim-left str))))
   (read-lines (format "%s.gcov" file-path))))

;; (gcov-read "strings.c")

(defun gcov-parse (string)
  "Returns a list of (line-num, times-ran)"
  `(,(string-to-number (substring string 10 15))
    ,(string-to-number (substring string 0 10))))

(defun gcov-make-overlay (line fringe)
  "Create an overlay for the line"
  (let* ((ol-front-mark
          (save-excursion
            (goto-line line)
            (point-marker)))
         (ol-back-mark
          (save-excursion
            (goto-line line)
            (end-of-line)
            (point-marker)))
         (ol (make-overlay ol-front-mark ol-back-mark)))
    (overlay-put ol 'before-string fringe)
    ol))

(defun gcov-get-fringe (n max)
  (let ((face
         (cond ((< gcov-high-threshold (/ n (float max)))
                'gcov-heavy-face)
               ((< gcov-med-threshold (/ n (float max)))
                'gcov-med-face)
               ((< n 1)
                'gcov-none-face)
               (t 'gcov-light-face))))
    (propertize "f" 'display `(left-fringe empty-line ,face))))

(defun gcov-set-overlays ()
  (interactive)
  (gcov-clear-overlays)
  (let* ((lines (mapcar 'gcov-parse (gcov-read (buffer-file-name))))
         (max (gcov-l-max (mapcar 'gcov-second lines))))
    (while (< 0 (list-length lines))
      (let* ((line (pop lines))
             (overlay (gcov-make-overlay (first line) (gcov-get-fringe (gcov-second line) max))))
        (setq gcov-overlays (cons overlay gcov-overlays))))))

(defun gcov-clear-overlays ()
  (interactive)
  (while (< 0 (list-length gcov-overlays))
    (delete-overlay (pop gcov-overlays))))

;;(clear-overlays)
;;(set-overlays)
;;(gcov-make-overlay 60 (gcov-get-fringe 5 15))
;;gcov-overlays

(defun gcov-update ()
  "Turn on gcov-mode."
  (interactive)
  (gcov-clear-overlays)
  (gcov-set-overlays))

(defun gcov-turn-on ()
  "Turn on gcov-mode."
  (gcov-set-overlays))

(defun gcov-turn-off ()
  "Turn off gcov-mode."
  (gcov-clear-overlays))

;;;###autoload
(define-minor-mode gcov-mode
  "Minor mode for gcov."
  :lighter " gcov"
  (progn
    (if gcov-mode
        (gcov-turn-on)
      (gcov-turn-off))))

(provide 'cov)
