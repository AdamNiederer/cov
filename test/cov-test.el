(load-file "cov.el")

(require 'cov)

;; cov--gcov-parse
(ert-deftest cov--gcov-parse--hyphen-test ()
  (with-temp-buffer
    (insert "        -:   22:        ")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test"))))))

(ert-deftest cov--gcov-parse--block-test ()
  (with-temp-buffer
    (insert "        1:   21-block  0")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test"))))))

(ert-deftest cov--gcov-parse--executed-test ()
  (with-temp-buffer
    (insert "        6:   24:        ")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test" (24 6)))))))

(ert-deftest cov--gcov-parse--big-value-test ()
  (with-temp-buffer
    (insert "999999999:99999:        ")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test" (99999 999999999)))))))

(ert-deftest cov--gcov-parse--multiline-test ()
  (with-temp-buffer
    (insert "        6:    1:\n       16:    2:\n       66:    3:")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test" (3 66) (2 16) (1 6)))))))

(ert-deftest cov--gcov-parse--not-executed-test ()
  (with-temp-buffer
    (insert "    #####:   24:        ")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--gcov-parse)
             '(("test" (24 0)))))))

;; cov--coveralls-parse
(ert-deftest cov--coveralls-parse--basic-test ()
  (with-temp-buffer
    (insert "{\"source_files\":[{\"coverage\":[0,null,3,1,2,0,null],\"source\":\"not covered\\nignored\\ncovered thee times\\ncovered once\\ncovered twice\\nnot covered either\\nignored too\\n\",\"name\":\"test\"}]}")
    (goto-char 1)
    (setq-local cov-coverage-file "test")
    (should (equal
             (cov--coveralls-parse)
             ;; The coverage is actually returned in reverse order.
             '(("test" (6 0) (5 2) (4 1) (3 3) (1 0)))))))

;; cov--clover-parse
(ert-deftest cov--clover-parse--basic-test ()
  (with-temp-buffer
    (insert-file-contents (format "%s/clover/clover.xml" test-path))
    (goto-char 1)
    (setq-local cov-coverage-file "clover.xml")
    (should (equal
             (cov--clover-parse)
             '(("test2" (3 100) (4 86) (5 85) (6 84) (7 46) (8 45) (9 44)
                (10 1) (11 0)))))))

;; cov--locate-coverage-postfix
(ert-deftest cov--locate-coverage-postfix-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "." ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix---subdir-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "../test" ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix--wrong-subdir-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "wrong-subdir" ".gcov")))
    (should (equal
             actual
             nil))))

(ert-deftest cov--locate-coverage-postfix--wrong-postfix-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "." ".wrong-postfix")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage-path
(ert-deftest cov--locate-coverage-path-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "."))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path---subdir-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "../test"))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path--wrong-subdir-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "wrong-subdir")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage
(ert-deftest cov--locate-coverage-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage (format "%s/%s" path "test")))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-wrong-file-test ()
  (let* ((path test-path)
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage (format "%s/%s" path "wrong-file"))))
    (should (equal
             actual
             nil))))

;; cov--get-face
(ert-deftest cov--get-face-test ()
  (let ((cov-coverage-mode nil)
        (cov-high-threshold 0.85)
        (cov-med-threshold 0.45))
    (should (equal (cov--get-face 0.86) 'cov-heavy-face))
    (should (equal (cov--get-face 0.46) 'cov-med-face))
    (should (equal (cov--get-face 0.44) 'cov-light-face))
    (should (equal (cov--get-face 0.0) 'cov-none-face))))

(ert-deftest cov--get-coverage-mode-face-test ()
  (let ((cov-coverage-mode t)
        (cov-high-threshold 0.85)
        (cov-med-threshold 0.45))
    (should (equal (cov--get-face 0.86) 'cov-coverage-run-face))
    (should (equal (cov--get-face 0.46) 'cov-coverage-run-face))
    (should (equal (cov--get-face 0.44) 'cov-coverage-run-face))
    (should (equal (cov--get-face 0.0) 'cov-coverage-not-run-face))))

;; cov-mode
(ert-deftest cov-mode--enable-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 1)
    (should (equal (length (cov--overlays)) 9))
	(kill-buffer)))

(ert-deftest cov-mode--disable-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 1)
    (cov-mode 0)
    (should (equal (cov--overlays) '()))
	(kill-buffer)))

(ert-deftest cov-mode--re-enable-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 1)
    (cov-mode 1)
    (should (equal (length (cov--overlays)) 9))
	(kill-buffer)))

(ert-deftest cov-mode--overlay-start-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (let ((overlay-starts (mapcar #'overlay-start (cov--overlays)))
          (expected '(15 36 57 78 99 120 141 162 183)))
      (should (equal (length overlay-starts) (length expected)))
      (ert-info ("Unexpected overlay ends")
        (should-not (cl-set-difference overlay-starts expected)))
      (ert-info ("Missing overlay starts")
        (should-not (cl-set-difference expected overlay-starts))))
	(kill-buffer)))

(ert-deftest cov-mode--overlay-end-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (let ((overlay-ends (mapcar #'overlay-end (cov--overlays)))
          (expected '(35 56 77 98 119 140 161 182 204)))
      (should (equal (length overlay-ends) (length expected)))
      (ert-info ("Unexpected overlay ends")
        (should-not (cl-set-difference overlay-ends expected)))
      (ert-info ("Missing overlay ends")
        (should-not (cl-set-difference expected overlay-ends))))
	(kill-buffer)))

(defun cov-mode--equal-including-properties (s1 s2)
  "Return t if strings S1 and S2 including complicated text properties.
This is like ‘equal-including-properties’ except that it handles
text properties with list values."
  (when (equal s1 s2) ; check that the actual characters are the same
    (let* ((pos1 0) (pos2 0)
           (props1 (text-properties-at pos1 s1))
           (props2 (text-properties-at pos2 s2)))
      (while (and pos1 pos2 ; posX will be nil at the end of string
                  (= pos1 pos2)
                  (equal props1 props2))
        (setq pos1 (next-property-change pos1 s1))
        (setq pos2 (next-property-change pos2 s2))
        (setq props1 (if pos1 (text-properties-at pos1 s1)))
        (setq props2 (if pos2 (text-properties-at pos2 s2))))
      (not (or pos1 pos2))))) ; if both are nil the strings are equal

(ert-deftest cov-mode--overlay-face-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    ;; overlay-bfs and expected are lists of list (LINENO BEFORE-STRING)
    (let ((overlay-bfs (mapcar
                        (lambda (ov)
                          (list (line-number-at-pos (overlay-start ov))
                                (overlay-get ov 'before-string)))
                        (cov--overlays)))
          (expected `((3  ,(propertize "f" 'display '(left-fringe empty-line cov-heavy-face)))
                      (4  ,(propertize "f" 'display '(left-fringe empty-line cov-heavy-face)))
                      (5  ,(propertize "f" 'display '(left-fringe empty-line cov-med-face)))
                      (6  ,(propertize "f" 'display '(left-fringe empty-line cov-med-face)))
                      (7  ,(propertize "f" 'display '(left-fringe empty-line cov-med-face)))
                      (8  ,(propertize "f" 'display '(left-fringe empty-line cov-light-face)))
                      (9  ,(propertize "f" 'display '(left-fringe empty-line cov-light-face)))
                      (10 ,(propertize "f" 'display '(left-fringe empty-line cov-light-face)))
                      (11 ,(propertize "f" 'display '(left-fringe empty-line cov-none-face)))))
          (cmp (lambda (a b)
                 (and (eq (car a) (car b))
                      (cov-mode--equal-including-properties (cadr a) (cadr b))))))
      (should (equal (length overlay-bfs) (length expected)))
      (ert-info ("Unexpected overlay before-strings")
        (should-not (cl-set-difference overlay-bfs expected :test cmp)))
      (ert-info ("Missing overlay before-strings")
        (should-not (cl-set-difference expected overlay-bfs :test cmp))))
	(kill-buffer)))

(ert-deftest cov-mode--overlay-help-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (let ((cov-overlays (sort (cov--overlays)
                              (lambda (a b)
                                (< (overlay-end a) (overlay-end b)))))
          (expected '("cov: executed 100 times (~100.00% of highest)"
                      "cov: executed 86 times (~86.00% of highest)"
                      "cov: executed 85 times (~85.00% of highest)"
                      "cov: executed 84 times (~84.00% of highest)"
                      "cov: executed 46 times (~46.00% of highest)"
                      "cov: executed 45 times (~45.00% of highest)"
                      "cov: executed 44 times (~44.00% of highest)"
                      "cov: executed 1 times (~1.00% of highest)"
                      "cov: executed 0 times (~0.00% of highest)")))
      (should (equal (length cov-overlays) (length expected)))
      (dolist (overlay cov-overlays)
        ;;(print (overlay-get overlay 'help-echo))
        (should (equal (overlay-get overlay 'help-echo) (pop expected)))))
	(kill-buffer)))
