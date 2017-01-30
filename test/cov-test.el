(load-file "cov.el")

(require 'cov)

;; gcov--keep-line?
(ert-deftest gcov--keep-line--test ()
  (should-not (gcov--keep-line? "        -:   22:        ")))

(ert-deftest gcov--keep-line--block-test ()
  (should-not (gcov--keep-line? "        1:   21-block  0")))

(ert-deftest gcov--keep-line--executed-test ()
  (let ((line "        6:   24:        "))
    (should (gcov--keep-line? line))
    (should (equal (match-string 1 line) "6"))
    (should (equal (match-string 2 line) "24"))))

(ert-deftest gcov--keep-line--not-executed-test ()
  (let ((line "    #####:   24:        "))
    (should (gcov--keep-line? line))
    (should (equal (match-string 1 line) "#####"))
    (should (equal (match-string 2 line) "24"))))

;; gcov--locate-coverage-postfix
(ert-deftest gcov--locate-coverage-postfix-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-postfix path "test" "." ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest gcov--locate-coverage-postfix---subdir-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-postfix path "test" "../test" ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest gcov--locate-coverage-postfix--wrong-subdir-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-postfix path "test" "wrong-subdir" ".gcov")))
    (should (equal
             actual
             nil))))

(ert-deftest gcov--locate-coverage-postfix--wrong-postfix-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-postfix path "test" "." ".wrong-postfix")))
    (should (equal
             actual
             nil))))

;; gcov--locate-coverage-path
(ert-deftest gcov--locate-coverage-path-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-path path "test" "."))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest gcov--locate-coverage-path---subdir-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-path path "test" "../test"))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest gcov--locate-coverage-path--wrong-subdir-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage-path path "test" "wrong-subdir")))
    (should (equal
             actual
             nil))))

;; gcov--locate-coverage
(ert-deftest gcov--locate-coverage-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage (format "%s/%s" path "test")))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest gcov--locate-coverage---wrong-file-test ()
  (let* ((path test-path)
         (actual (gcov--locate-coverage (format "%s/%s" path "wrong-file"))))
    (should (equal
             actual
             nil))))
