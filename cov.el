;;; cov.el --- Show coverage stats in the fringe.

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

(defgroup gcov nil
  "The group for everything in cov.el")

(defgroup cov-faces nil
  "Faces for gcov."
  :group 'gcov
  :group 'faces)

(defface cov-heavy-face
  '((((class color)) :foreground "red"))
  "Face used on the fringe indicator for successful evaluation."
  :group 'cov-faces)

(defface cov-med-face
  '((((class color)) :foreground "yellow"))
  ;;:group 'gcov
  "Face used on the fringe indicator for successful evaluation."
  :group 'cov-faces)

(defface cov-light-face
  '((((class color)) :foreground "green"))
  "Face used on the fringe indicator for successful evaluation."
  :group 'cov-faces)

(defface cov-none-face
  '((((class color)) :foreground "blue"))
  "Face used on the fringe indicator for no evaluation."
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
(defvar cov-high-threshold .85)
(defvar cov-med-threshold .45)
(defvar cov-overlays '())
(defconst cov-line-re "^ +\\([0-9#]+\\):\\s-+\\([0-9#]+\\):")

(defun cov--read-lines (file-path)
  "Return a list of lines"
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t nil)))

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
      (setq cov-coverage-file (cov--locate-coverage (f-this-file)))))

(defun cov--keep-line? (line)
  (s-matches? cov-line-re line))

(defun cov--read (file-path)
  "Read a gcov file, filter unused lines, and return a list of lines"
  (cl-remove-if-not
   'cov--keep-line?
   (cov--read-lines file-path)))

(defun cov--parse (string)
  "Returns a list of (line-num, times-ran)"
  `(,(string-to-number (nth 2 (s-match cov-line-re string)))
    ,(string-to-number (nth 1 (s-match cov-line-re string)))))

(defun cov-make-overlay (line fringe help)
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

(defun cov--get-fringe (n max percentage)
  "Returns the fringe with the correct face"
  (let ((face
         (cond ((< cov-high-threshold percentage)
                'cov-heavy-face)
               ((< cov-med-threshold percentage)
                'cov-med-face)
               ((< n 1)
                'cov-none-face)
               (t 'cov-light-face))))
    (propertize "f" 'display `(left-fringe empty-line ,face))))

(defun cov--help (n max percentage)
  (format "gcov: executed %d times (~%.2f%% of highest)" n (* percentage 100)))

(defun cov--set-overlay (line max)
  (let* ((times-executed (nth 1 line))
         (percentage (/ times-executed (float max)))
         (overlay (cov-make-overlay
                   (cl-first line)
                   (cov--get-fringe times-executed max percentage)
                   (cov--help times-executed max percentage))))
    (setq cov-overlays (cons overlay cov-overlays))))

(defun cov-set-overlays ()
  (interactive)
  (let ((gcov (cov--coverage)))
    (if gcov
        (let* ((lines (mapcar 'cov--parse (cov--read (car gcov))))
               (max (reduce 'max (mapcar 'cl-second lines))))
          (dolist (line-data lines)
            (cov--set-overlay line-data max)))
      (message "No coverage data found."))))

(defun cov-clear-overlays ()
  (interactive)
  (while (< 0 (cl-list-length cov-overlays))
    (delete-overlay (pop cov-overlays))))

(defun cov-visit-coverage-file ()
  "Visit coverage file."
  (interactive)
  (let ((gcov (cov--coverage)))
    (if gcov
        (find-file (car gcov))
      (message "No coverage data found."))))

(defun cov-update ()
  "Turn on cov-mode."
  (interactive)
  (cov-clear-overlays)
  (cov-set-overlays))

(defun cov-turn-on ()
  "Turn on cov-mode."
  (cov-set-overlays))

(defun cov-turn-off ()
  "Turn off cov-mode."
  (cov-clear-overlays))

;;;###autoload
(define-minor-mode cov-mode
  "Minor mode for gcov."
  :lighter " gcov"
  (progn
    (if cov-mode
        (cov-turn-on)
      (cov-turn-off))))

(provide 'cov)
;;; cov.el ends here
