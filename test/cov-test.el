(load-file "cov.el")

(require 'cov)

;; cov--keep-line?
(ert-deftest cov--keep-line--test ()
  (should-not (cov--keep-line? "        -:   22:        ")))

(ert-deftest cov--keep-line--block-test ()
  (should-not (cov--keep-line? "        1:   21-block  0")))

;; cov--parse & cov--keep-line?
(ert-deftest cov--parse--executed-test ()
  (let ((line "        6:   24:        "))
    (should (cov--keep-line? line))
    (should (equal (cov--parse line) '(24 6)))))

(ert-deftest cov--parse--not-executed-test ()
  (let ((line "    #####:   24:        "))
    (should (cov--keep-line? line))
    (should (equal (cov--parse line) '(24 0)))))

;; cov--locate-coverage-postfix
(ert-deftest cov--locate-coverage-postfix-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-postfix path "test" "." ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix---subdir-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-postfix path "test" "../test" ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix--wrong-subdir-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-postfix path "test" "wrong-subdir" ".gcov")))
    (should (equal
             actual
             nil))))

(ert-deftest cov--locate-coverage-postfix--wrong-postfix-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-postfix path "test" "." ".wrong-postfix")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage-path
(ert-deftest cov--locate-coverage-path-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-path path "test" "."))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path---subdir-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-path path "test" "../test"))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path--wrong-subdir-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage-path path "test" "wrong-subdir")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage
(ert-deftest cov--locate-coverage-test ()
  (let* ((path test-path)
         (actual (cov--locate-coverage (format "%s/%s" path "test")))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-wrong-file-test ()
  (let* ((path test-path)
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
    (should (equal (length cov-overlays) 9)))
  (kill-buffer "test"))

(ert-deftest cov-mode--disable-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 1)
    (cov-mode 0)
    (should (equal cov-overlays '())))
  (kill-buffer "test"))

(ert-deftest cov-mode--re-enable-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 1)
    (cov-mode 1)
    (should (equal (length cov-overlays) 9)))
  (kill-buffer "test"))

(ert-deftest cov-mode--overlay-start-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (should (equal (length cov-overlays) 9))
    (let ((expected (reverse '(15 36 57 78 99 120 141 162 183))))
      (dolist (overlay cov-overlays)
        (should (equal (overlay-start overlay) (pop expected))))))
  (kill-buffer "test"))

(ert-deftest cov-mode--overlay-end-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (should (equal (length cov-overlays) 9))
    (let ((expected (reverse '(35 56 77 98 119 140 161 182 204))))
      (dolist (overlay cov-overlays)
        (should (equal (overlay-end overlay) (pop expected))))))
  (kill-buffer "test"))

(ert-deftest cov-mode--overlay-face-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (should (equal (length cov-overlays) 9))
    (let ((expected (reverse '(#("f" 0 1 (display (left-fringe empty-line cov-heavy-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-heavy-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-med-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-med-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-med-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-light-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-light-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-light-face)))
                               #("f" 0 1 (display (left-fringe empty-line cov-none-face)))))))
      (dolist (overlay cov-overlays)
        ;;(print (overlay-get overlay 'before-string))
        (should (equal (overlay-get overlay 'before-string) (pop expected))))))
  (kill-buffer "test"))

(ert-deftest cov-mode--overlay-help-test ()
  (with-current-buffer (find-file-noselect (format "%s/test" test-path))
    (cov-mode 0)
    (cov-mode 1)
    (should (equal (length cov-overlays) 9))
    (let ((expected (reverse '("cov: executed 100 times (~100.00% of highest)"
                               "cov: executed 86 times (~86.00% of highest)"
                               "cov: executed 85 times (~85.00% of highest)"
                               "cov: executed 84 times (~84.00% of highest)"
                               "cov: executed 46 times (~46.00% of highest)"
                               "cov: executed 45 times (~45.00% of highest)"
                               "cov: executed 44 times (~44.00% of highest)"
                               "cov: executed 1 times (~1.00% of highest)"
                               "cov: executed 0 times (~0.00% of highest)"))))
      (dolist (overlay cov-overlays)
        ;;(print (overlay-get overlay 'help-echo))
        (should (equal (overlay-get overlay 'help-echo) (pop expected))))))
  (kill-buffer "test"))
