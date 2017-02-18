;;; cov.el --- Show coverage stats in the fringe. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Adam Niederer

;; Author: Adam Niederer
;; Maintainer: Adam Niederer
;; Created: 12 Aug 2016

;; Keywords: coverage gcov c
;; Homepage: https://github.com/AdamNiederer/cov
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (f "0.18.2") (s "1.11.0"))

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
(require 'cl-lib)

(defgroup cov nil
  "The group for everything in cov.el")

(defgroup cov-faces nil
  "Faces for cov.")

(defcustom cov-high-threshold .85
  "The threshold at which a line will be painted with the heavy-use face, as a
percentage of the most-run line."
  :tag "Cov heavy-use threshold"
  :group 'cov
  :type 'float)

(defcustom cov-med-threshold .45
  "The threshold at which a line will be painted with the medium-use face, as a
percentage of the most-run line."
  :tag "Cov medium-use threshold"
  :group 'cov
  :type 'float)

(defcustom cov-coverage-mode nil
  "Whether to decorate all covered lines with a green fringe, and all non-covered
lines with a red fringe"
  :tag "Cov coverage mode"
  :group 'cov
  :type 'boolean)

(defface cov-heavy-face
  '((((class color)) :foreground "red"))
  "Fringe indicator face used for heavily-run lines See `cov-high-threshold'."
  :tag "Cov heavy-use face"
  :group 'cov-faces)

(defface cov-med-face
  '((((class color)) :foreground "yellow"))
  "Fringe indicator face used for commonly-run lines See `cov-med-threshold'."
  :tag "Cov medium-use face"
  :group 'cov-faces)

(defface cov-light-face
  '((((class color)) :foreground "green"))
  "Fringe indicator face used for rarely-run lines. This face is applied if no
other face is applied."
  :tag "Cov light-use face"
  :group 'cov-faces)

(defface cov-none-face
  '((((class color)) :foreground "blue"))
  "Fringe indicator face used for lines which were not run."
  :tag "Cov never-used face"
  :group 'cov-faces)

(defface cov-coverage-run-face
  '((((class color)) :foreground "green"))
  "Fringe indicator face used in coverage mode for lines which were run. See
`cov-coverage-mode'"
  :tag "Cov coverage mode run face"
  :group 'cov-faces)

(defface cov-coverage-not-run-face
  '((((class color)) :foreground "red"))
  "Fringe indicator face used in coverage mode for lines which were not run. See
`cov-coverage-mode'"
  :tag "Cov coverage mode not-run face"
  :group 'cov-faces)

(defvar cov-coverage-alist '((".gcov" . gcov))
  "Alist of coverage tool and file postfix.

Each element looks like (FILE-POSTIX . COVERAGE-TOOL). If a file with
FILE-POSTIX appended to the buffer-file-name is found is is assumed
that the specified COVERAGE-TOOL has created the data.

Currently the only supported COVERAGE-TOOL is gcov.")

(defvar cov-coverage-file-paths '(".")
  "List of file paths to use to search for coverage files as strings or function.

Relative paths:
 .      search in current directory
 cov    search in subdirectory cov
 ..     search in parent directory
 ../cov search in subdirectory cov of parent directory

Function:
 A function or lambda that should get the buffer file dir and name as arguments and return eiter nil or the truename path to the coverage file and the corresponding coverage tool in a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL).
 The following example sets a lambda that searches the coverage file in the current directory:
  (setq cov-coverage-file-paths (list #'(lambda (file-dir file-name)
                                           (let ((try (format \"%s/%s%s\"
                                                              file-dir file-name
                                                              cov-coverage-file-extension)))
                                             (and (file-exists-p try)
                                                  (cons (file-truename try) 'gcov))))))

Make the variable buffer-local, so it can be set per project, e.g. in a .dir-locals.el file, by adding
(make-variable-buffer-local 'cov-coverage-file-paths) in your init.el.")
(defvar-local cov-coverage-file nil
  "Last located coverage file and tool.")

(defvar cov-overlays '())
(defconst cov-line-re "^ *\\(\\([0-9#]+\\): *\\([0-9]+\\)\\):")

(defun cov--locate-coverage-postfix (file-dir file-name path extension)
  (let ((try (format "%s/%s/%s%s" file-dir path file-name extension)))
    (and (file-exists-p try) (file-truename try))))

(defun cov--locate-coverage-path (file-dir file-name path)
  "Locate coverage file .gcov of given FILE-PATH."
  (cl-some (lambda (extension-tool)
             (let* ((extension (car extension-tool))
                    (tool (cdr extension-tool))
                    (file (cov--locate-coverage-postfix file-dir file-name path extension)))
               (and file (cons file tool))))
           cov-coverage-alist))

(defun cov--locate-coverage (file-path)
  "Locate coverage file of given source file FILE-PATH.

The function iterates over `cov-coverage-file-path' for path candidates or locate functions. The first found file will be returned as a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL). If no file is found nil is returned."
  (let ((file-dir (f-dirname file-path))
        (file-name (f-filename file-path)))
    (cl-some (lambda (path-or-fun)
               (if (stringp path-or-fun)
                   (cov--locate-coverage-path file-dir file-name path-or-fun)
                 (funcall path-or-fun file-dir file-name)))
             cov-coverage-file-paths)))

(defun cov--coverage ()
  "Return coverage file and tool as a cons cell of the form (COV-FILE-PATH . COVERAGE-TOOL) for current buffer.

If `cov-coverage-file' is non nil, the value of that variable is returned. Otherwise `cov--locate-coverage' is called."
  (or cov-coverage-file
      (setq cov-coverage-file (cov--locate-coverage (buffer-file-name)))))

(defun cov--parse (buffer)
  "Parse a buffer containing gcov file, filter unused lines, and return a list of (LINE-NUM TIMES-RAN)."
  (let ((more t)
        matches)
    (save-match-data
      (while more
        (when (looking-at cov-line-re)
          (push (list (string-to-number (match-string-no-properties 3))
                      (string-to-number (match-string-no-properties 2)))
                matches))
        (end-of-line)
        (setq more (= 0 (forward-line 1)))))
    matches))

(defun cov--read-and-parse (file-path)
  "Read coverage file FILE-PATH into temp buffer and parses it using `cov--parse'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (cov--parse (current-buffer))))

(defun cov--make-overlay (line fringe help)
  "Create an overlay for the line"
  (let (ol-front-mark ol-back-mark ol)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (setq ol-front-mark (point))
      (end-of-line)
      (setq ol-back-mark (point)))
    (setq ol (make-overlay ol-front-mark ol-back-mark))
    (overlay-put ol 'before-string fringe)
    (overlay-put ol 'help-echo help)
    ol))

(defun cov--get-face (percentage)
    "Return the appropriate face to color the line given use preferences and the
code's execution frequency"
    (cond
     ((and cov-coverage-mode (> percentage 0))
      'cov-coverage-run-face)
     ((and cov-coverage-mode (= percentage 0))
      'cov-coverage-not-run-face)
     ((< cov-high-threshold percentage)
      'cov-heavy-face)
     ((< cov-med-threshold percentage)
      'cov-med-face)
     ((> percentage 0)
      'cov-light-face)
     (t 'cov-none-face)))

(defun cov--get-fringe (percentage)
  "Returns the fringe with the correct face"
  (propertize "f" 'display `(left-fringe empty-line ,(cov--get-face percentage))))

(defun cov--help (n percentage)
  (format "cov: executed %d times (~%.2f%% of highest)" n (* percentage 100)))

(defun cov--set-overlay (line max displacement)
  (let* ((times-executed (nth 1 line))
         (percentage (/ times-executed (float max)))
         (overlay (cov--make-overlay
                   (- (cl-first line) displacement)
                   (cov--get-fringe percentage)
                   (cov--help times-executed percentage))))
    (push overlay cov-overlays)))

(defun cov--calc-line-displacement ()
  "Get line number displacement if buffer is narrowed."
  (let ((start (point-min)))
    (if (= start 1)
        0
      (save-excursion
        (save-restriction
          (widen)
          (1- (line-number-at-pos start)))))))

(defun cov-set-overlays ()
  (interactive)
  (let ((cov (cov--coverage)))
    (if cov
        (let* ((lines (cov--read-and-parse (car cov)))
               (max (cl-reduce 'max (cons 0 (mapcar 'cl-second lines))))
               (displacement (cov--calc-line-displacement))
               (max-line (+ (line-number-at-pos (point-max)) displacement)))
          (dolist (line-data lines)
            (when (and (> (car line-data) displacement)
                       (<= (car line-data) max-line))
              (cov--set-overlay line-data max displacement))))
      (message "No coverage data found for %s." (buffer-file-name)))))

(defun cov-clear-overlays ()
  (interactive)
  (while (< 0 (cl-list-length cov-overlays))
    (delete-overlay (pop cov-overlays))))

(defun cov-visit-coverage-file ()
  "Visit coverage file."
  (interactive)
  (let ((cov (cov--coverage)))
    (if cov
        (find-file (car cov))
      (message "No coverage data found."))))

(defun cov-update ()
  "Turn on cov-mode."
  (interactive)
  (cov-clear-overlays)
  (cov-set-overlays))

(defun cov-turn-on ()
  "Turn on cov-mode."
  (cov-clear-overlays)
  (cov-set-overlays))

(defun cov-turn-off ()
  "Turn off cov-mode."
  (cov-clear-overlays))

;;;###autoload
(define-minor-mode cov-mode
  "Minor mode for cov."
  :lighter " cov"
  (progn
    (if cov-mode
        (cov-turn-on)
      (cov-turn-off))))

(provide 'cov)
;;; cov.el ends here
