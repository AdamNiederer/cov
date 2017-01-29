;;; cov.el --- Show coverage gutter in  the fringe.

;; Copyright (C) 2016-2017 Adam Niederer


;; Author: Adam Niederer
;; Maintainer: Adam Niederer
;; Created: 12 Aug 2016

;; Keywords: coverage
;; Homepage: https://github.com/AdamNiederer/cov
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (s "1.11.0") (f "0.18.2"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'cl-extra)

(defgroup gcov nil
  "The group for everything in cov.el")

(defun gcov-l-max (list)
  (eval (cons 'max (cons 0 list))))

(defun gcov-second (list)
  (nth 1 list))()

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

(defvar gcov-coverage-alist '((".gcov" . gcov))
  "Alist of coverage tool and file postfix.

Each element looks like (FILE-POSTIX . COVERAGE-TOOL). If a file with
FILE-POSTIX appended to the buffer-file-name is found is is assumed
that the specified COVERAGE-TOOL has created the data.

Currently the only supported COVERAGE-TOOL is gcov.")

(defvar gcov-coverage-file-paths '(".")
  "List of file paths to use to search for coverage files as strings or function.

Relative paths:
 .      search in current directory
 cov    search in subdirectory cov
 ..     search in parent directory
 ../cov search in subdirectory cov of parent directory

Function:
 A function or lambda that should get the buffer file dir and name as arguments and return eiter nil or the truename path to the coverage file and the corresponding coverage tool in a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL).
 The following example sets a lambda that searches the coverage file in the current directory:
  (setq gcov-coverage-file-paths (list #'(lambda (file-dir file-name)
                                           (let ((try (format \"%s/%s%s\"
                                                              file-dir file-name
                                                              gcov-coverage-file-extension)))
                                             (and (file-exists-p try)
                                                  (cons (file-truename try) 'gcov))))))

Make the variable buffer-local, so it can be set per project, e.g. in a .dir-locals.el file, by adding
(make-variable-buffer-local 'gcov-coverage-file-paths) in your init.el.")
(defvar-local gcov-coverage-file nil
  "Last located cverage file and tool.")
(defvar gcov-high-threshold .85)
(defvar gcov-med-threshold .45)
(defvar gcov-overlays '())

(defun gcov--read-lines (file-path)
  "Return a list of lines"
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t nil)))

(defun gcov--locate-coverage-postfix (file-dir file-name path extension)
  (let ((try (format "%s/%s/%s%s" file-dir path file-name extension)))
    (and (file-exists-p try) (file-truename try))))

(defun gcov--locate-coverage-path (file-dir file-name path)
  "Locate coverage file .gcov of given FILE-PATH."
  (cl-some (lambda (extension-tool)
             (let* ((extension (car extension-tool))
                    (tool (cdr extension-tool))
                    (file (gcov--locate-coverage-postfix file-dir file-name path extension)))
               (and file (cons file tool))))
           gcov-coverage-alist))

(defun gcov--locate-coverage (file-path)
  "Locate coverage file of given source file FILE-PATH.

The function iterates over `gcov-coverage-file-path' for path candidates or locate functions. The first found file will be returned as a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL). If no file is found nil is returned."
  (let ((file-dir (f-dirname file-path))
        (file-name (f-filename file-path)))
    (cl-some (lambda (path-or-fun)
               (if (stringp path-or-fun)
                   (gcov--locate-coverage-path file-dir file-name path-or-fun)
                 (funcall path-or-fun file-dir file-name)))
             gcov-coverage-file-paths)))

(defun gcov--coverage ()
  "Return coverage file and tool as a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL) for current buffer.

If `gcov-coverage-file' is non nil, the value of that variable is returned. Otherwise `gcov--locate-coverage' is called."
  (or gcov-coverage-file
      (setq gcov-coverage-file (gcov--locate-coverage (f-this-file)))))

(defun gcov--keep-line? (line)
  (string-match "^\\s-+\\([0-9#]+\\):\\s-+\\([0-9#]+\\):" line))

(defun gcov--read (file-path)
  "Read a gcov file, filter unused lines, and return a list of lines"
  (remove-if-not
   'gcov--keep-line?
   (gcov--read-lines file-path)))

(defun gcov--parse (string)
  "Returns a list of (line-num, times-ran)"
  `(,(string-to-number (match-string 2 string))
    ,(string-to-number (match-string 1 string))))

(defun gcov-make-overlay (line fringe help)
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
    (overlay-put ol 'help-echo help)
    ol))

(defun gcov--get-fringe (n max percentage)
  (let ((face
         (cond ((< gcov-high-threshold percentage)
                'gcov-heavy-face)
               ((< gcov-med-threshold percentage)
                'gcov-med-face)
               ((< n 1)
                'gcov-none-face)
               (t 'gcov-light-face))))
    (propertize "f" 'display `(left-fringe empty-line ,face))))

(defun gcov--help (n max percentage)
  (format "gcov: executed %s times (~%s%%)" n (* percentage 100)))

(defun gcov--set-overlay (line max)
  (let* ((n (gcov-second line))
         (percentage (/ n (float max)))
         (overlay (gcov-make-overlay
                   (first line)
                   (gcov--get-fringe n max percentage)
                   (gcov--help n max percentage))))
    (setq gcov-overlays (cons overlay gcov-overlays))))

(defun gcov-set-overlays ()
  (interactive)
  (let ((gcov (gcov--coverage)))
    (if gcov
        (let (lines max)
          (save-match-data
            (setq lines (mapcar 'gcov--parse (gcov--read (car gcov)))))
          (setq max (gcov-l-max (mapcar 'gcov-second lines)))
          (while (< 0 (list-length lines))
            (let ((line (pop lines)))
              (gcov--set-overlay line max))))
      (message "No coverage data found."))))

(defun gcov-clear-overlays ()
  (interactive)
  (while (< 0 (list-length gcov-overlays))
    (delete-overlay (pop gcov-overlays))))

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
;;; cov.el ends here
