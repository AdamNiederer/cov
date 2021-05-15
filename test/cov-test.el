(load-file "cov.el")

(require 'cov)
(require 'mocker)
(require 'type-break) ; for timep

(defmacro cov--with-test-buffer (testfile &rest body)
  "Open TESTFILE in a buffer, execute BODY in it, and kill the buffer.
TESTFILE can be an absolute path, a path relative to `test-path',
or a symbol to be resolved at runtime."
  (declare (indent 1) (debug t))
  (let ((testfilepath
         (cond ((and (stringp testfile) (file-name-absolute-p testfile)) testfile)
               ((and (stringp testfile) `(format "%s/%s" test-path ,testfile)))
               ((symbolp testfile) testfile)
               (t (error "Unexpected argument type for `testfile'")))))
    `(with-current-buffer (find-file-noselect ,testfilepath)
       ,@body
       (kill-buffer))))

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
	(let ((expected '(("test" (3 66) (2 16) (1 6))))
		  (result (cov--gcov-parse)))
	  (should (equal (caar result) (caar expected)))
	  (ert-info ("Unexpected matches")
		(should-not (cl-set-difference (cdar result) (cdar expected) :test 'equal)))
	  (ert-info ("Missing matches")
		(should-not (cl-set-difference (cdar expected) (cdar result)  :test 'equal))))))

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
  "Verify that `cov--locate-coverage-postfix-test' finds a file in the same directory."
  :tags '(cov--locate-coverage-postfix)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "." ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix---subdir-test ()
  "Verify that `cov--locate-coverage-postfix-test' finds a file in a subdir."
  :tags '(cov--locate-coverage-postfix)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "../same-dir" ".gcov"))
         (expected (file-truename (format "%s/test.gcov" path))))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-postfix--wrong-subdir-test ()
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "wrong-subdir" ".gcov")))
    (should (equal
             actual
             nil))))

(ert-deftest cov--locate-coverage-postfix--wrong-postfix-test ()
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-postfix path "test" "." ".wrong-postfix")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage-path
(ert-deftest cov--locate-coverage-path-test ()
  "Verify that `cov--locate-coverage-path' finds a file in the same dir."
  :tags '(cov--locate-coverage-path)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "."))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path---subdir-test ()
  "Verify that `cov--locate-coverage-path' finds a file on a path."
  :tags '(cov--locate-coverage-path)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "../same-dir"))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-path--wrong-subdir-test ()
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage-path path "test" "wrong-subdir")))
    (should (equal
             actual
             nil))))

;; cov--locate-coverage
(ert-deftest cov--locate-coverage-test ()
  "Verify that `cov--locate-coverage' finds a gcov file in the same dir."
  :tags '(cov--locate-coverage)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage (format "%s/%s" path "test")))
         (expected (cons (file-truename (format "%s/test.gcov" path)) 'gcov)))
    (should (equal
             actual
             expected))))

(ert-deftest cov--locate-coverage-wrong-file-test ()
  :tags '(cov--locate-coverage)
  (let* ((path (format "%s/gcov/same-dir" test-path))
         (cov-coverage-file-paths '("."))
         (actual (cov--locate-coverage (format "%s/%s" path "wrong-file"))))
    (should (equal
             actual
             nil))))

;; cov-data--remove-buffer
(ert-deftest cov-data--remove-buffer-test ()
  "Test `cov-data-remove-buffer'."
  (let ((covdata (make-cov-data)))
    ;; starts with an empty list
    (should-not (cov-data-buffers covdata))
    (with-temp-buffer
      (ert-info ((progn "Removing a buffer from empty list is ok"))
        (should-not (cov-data--remove-buffer covdata (current-buffer)))
        (should-not (cov-data-buffers covdata)))
      (ert-info ((progn "Adding and removing a single buffer"))
        (cl-pushnew (current-buffer) (cov-data-buffers covdata))
        (should (equal (list (current-buffer)) (cov-data-buffers covdata)))
        (should-not (cov-data--remove-buffer covdata (current-buffer)))
        (should-not (cov-data-buffers covdata)))
      (ert-info ((progn "Trying to remove a non-listed buffer is a no-op"))
        (cl-pushnew (current-buffer) (cov-data-buffers covdata))
        (with-temp-buffer
          (let (remaining)
            (should (setq remaining (cov-data--remove-buffer covdata (current-buffer))))
            (should (equal remaining (cov-data-buffers covdata))))))
      (ert-info ((progn "Remove a buffer in the middle of the list"))
        (let ((other-buffer (get-buffer-create (symbol-name (cl-gensym)))))
          (cl-pushnew other-buffer (cov-data-buffers covdata))
          (with-temp-buffer
            (cl-pushnew (current-buffer) (cov-data-buffers covdata))
            (should (equal 2 (length (cov-data--remove-buffer covdata other-buffer))))
            (should-not (memq other-buffer (cov-data-buffers covdata)))
            (should (memq (current-buffer) (cov-data-buffers covdata))))
          (should (memq (current-buffer) (cov-data-buffers covdata))))))))

;; cov-data--unregister-watcher
(ert-deftest cov-data--unregister-watcher ()
  "Test `cov-data--unregister-watcher'."
  (let ((covdata (make-cov-data)))
    (ert-info ((progn "Unregister nonexistent watch"))
      (mocker-let ((file-notify-rm-watch (descriptor)))
        (cov-data--unregister-watcher covdata)
        (should-not (cov-data-watcher covdata))))
    (ert-info ((progn "Unregister a watch"))
      (setf (cov-data-watcher covdata) 'watch-desc)
      (mocker-let ((file-notify-rm-watch (descriptor)
                                         ((:input '(watch-desc)))))
        (cov-data--unregister-watcher covdata)
        (should-not (cov-data-watcher covdata))))))

;; cov--stored-data
(ert-deftest cov--stored-data-empty-test ()
  "Test that a single new cov-data struct is added to `cov-coverages'."
  (let ((cov-coverges (make-hash-table :test 'equal)))
    (cov--stored-data "filename" 'gcov)
    (should (equal (hash-table-keys cov-coverages) '("filename")))
    (should (cov-data-p (car (hash-table-values cov-coverages))))
    (should (eq 'gcov (cov-data-type (car (hash-table-values cov-coverages)))))))

(ert-deftest cov--stored-data-has-data-test ()
  "Test that any matching cov-data struct is returned."
  (let ((cov-coverages (make-hash-table :test 'equal)))
    (puthash "filename" 'foo cov-coverages)
    (should (eq 'foo (cov--stored-data "filename" 'gcov)))))

(ert-deftest cov--stored-data-same-data-test ()
  "Test that the created data is returned later."
  (let ((cov-coverges (make-hash-table :test 'equal))
        data1 data2)
    (cov--stored-data "filename" 'gcov)
    (should (equal (hash-table-keys cov-coverages) '("filename")))
    (setq data1 (car (hash-table-values cov-coverages)))
    (should (cov-data-p data1))
    (should (eq 'gcov (cov-data-type data1)))
    (cov--stored-data "filename" 'gcov)
    (should (equal (hash-table-keys cov-coverages) '("filename")))
    (setq data2 (car (hash-table-values cov-coverages)))
    (should (eq data1 data2))
    (should (equal data1 data2))))

;; cov--get-buffer-coverage
(ert-deftest cov--get-buffer-coverage-no-coverage-test ()
  "No coverage data file found."
  :tags '(cov--get-buffer-coverage)
  (mocker-let ((cov--coverage ()
                              ((:input '() :output nil)))
               (cov--load-coverage (datastore file &optional ignore-current)))
    (should-not (cov--get-buffer-coverage))))

(ert-deftest cov--get-buffer-coverage-no-stored-data-test ()
  "No stored coverage data found."
  :tags '(cov--get-buffer-coverage)
  (let ((cov-coverages (make-hash-table :test 'equal)))
    (mocker-let ((file-notify-add-watch (file flags callback)
                                        ((:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/gcov/same-dir" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0)))))
      (cov--with-test-buffer "gcov/same-dir/test"
        (should (cov--get-buffer-coverage))
        ;; Check that one data entry has been stored in cov-coverages
        (should (equal (hash-table-count cov-coverages) 1))
        (let ((key (car (hash-table-keys cov-coverages)))
              (val (car (hash-table-values cov-coverages))))
          (should (string= (file-name-nondirectory key) "test.gcov"))
          (should (cov-data-p val))
          (should (eq (cov-data-type val) 'gcov))
          (should (eq (cov-data-watcher val)
                      ;; Has not been set if file-notify is not supported.
                      (if file-notify--library 'watch-descriptor nil)))
          (should (memq (current-buffer) (cov-data-buffers val)))
          (should (memq 'cov-kill-buffer-hook kill-buffer-hook))
          (should (equal '(cov-kill-buffer-hook t) kill-buffer-hook)))))))

(ert-deftest cov--get-buffer-coverage-have-data-test ()
  "No stored coverage data found, coverage file not updated."
  :tags '(cov--get-buffer-coverage)
  (let ((cov-coverages (make-hash-table :test 'equal))
        kill-buffer-hook
        stored-data)
    (mocker-let ((file-notify-add-watch (file flags callback)
                                        ((:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/gcov/same-dir" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0)
                                          ))))
      (cov--with-test-buffer "gcov/same-dir/test"
        ;; load data for the first time
        (should (setq stored-data (cov--get-buffer-coverage)))
        (should (eq stored-data (cov--get-buffer-coverage)))))))

;; cov--load-coverage
(ert-deftest cov--load-coverage-test-mtime-check ()
  "Verify that the mtime-check in cov--load-coverage works."
  :tags '(cov--load-coverage)
  (cov--with-test-buffer "gcov/same-dir/test"
    (let ((coverage (make-cov-data :type 'gcov :buffers (list (current-buffer))))
          (cov-file (buffer-file-name)))
      (mocker-let ((cov-update () ((:occur 1))))
        (cov--load-coverage coverage cov-file))
      (ert-info ("Some coverage data should have been set")
        (should (cov-data-coverage coverage)))
      (ert-info ("mtime field of cov-data struct should be a valid time")
        (should (timep (cov-data-mtime coverage))))
      (ert-info ("change cov-data-mtime to force reload")
        (setf (cov-data-mtime coverage)
              (time-add (cov-data-mtime coverage) 1))
        (mocker-let ((cov-update () ((:occur 1))))
          (cov--load-coverage coverage cov-file ))
        (ert-info ("Some coverage data should have been set")
          (should (cov-data-coverage coverage)))
        (ert-info ("mtime field of cov-data struct should be a valid time")
          (should (timep (cov-data-mtime coverage)))))
      (ert-info ("Running again with the same cov-data-mtime should not reload")
        (mocker-let ((cov--read-and-parse (&rest _) ((:occur 0)))
                     (cov-update () ((:occur 0))))
          (cov--load-coverage coverage cov-file )))
        )))

;; cov-kill-buffer-hook
(ert-deftest cov-kill-buffer-hook-test-1-buffer ()
  "Test kill hook when a single cov file and buffer is in `cov-coverages'."
  :tags '(cov-kill-buffer-hook)
  (let* ((testfile (format "%s/gcov/same-dir/test" test-path))
         (covfile (concat testfile ".gcov")))
    (mocker-let ((file-notify-add-watch (file flags callback)
                                        ((:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/gcov/same-dir" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0))))
                 (file-notify-rm-watch (descriptor)
                                       ((:input
                                         '(watch-descriptor)
                                         :output nil
                                         ;; Not called if file-notify is not supported
                                         :occur (if file-notify--library 1 0)))))
      (cov--with-test-buffer testfile
        (let ((cov-coverages (make-hash-table :test 'equal))
              covdata)
          (cov-mode)
          (should (local-variable-p 'kill-buffer-hook))
          (should (memq 'cov-kill-buffer-hook kill-buffer-hook))
          (should (member covfile (hash-table-keys cov-coverages)))
          (setq covdata (gethash covfile cov-coverages))
          (should (memq (current-buffer) (cov-data-buffers covdata)))
          (cov-kill-buffer-hook)
          (should-not (cov-data-watcher covdata))
          (should-not (memq (current-buffer) (cov-data-buffers covdata)))
          (should (hash-table-empty-p cov-coverages)))))))

(ert-deftest cov-kill-buffer-hook-test-2-buffers ()
  "Test that only the buffer being killed is cleared out."
  :tags '(cov-kill-buffer-hook)
  (let* ((testfile (format "%s/gcov/same-dir/test" test-path))
         (covfile (concat testfile ".gcov")))
    (mocker-let ((file-notify-add-watch (file flags callback)
                                        ((:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/gcov/same-dir" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0))))
                 (file-notify-rm-watch (descriptor)))
      (cov--with-test-buffer testfile
        (let ((cov-coverages (make-hash-table :test 'equal))
              covdata)
          (cov-mode)
          ;; do not call cov-kill-buffer-hook from the hook
          (remove-hook 'kill-buffer-hook 'cov-kill-buffer-hook t)
          (setq covdata (gethash covfile cov-coverages))
          ;; add another buffer to the cov-data buffer list
          (cl-pushnew (get-buffer-create (symbol-name (cl-gensym)))
                      (cov-data-buffers covdata))
          (cov-kill-buffer-hook)
          (should-not (hash-table-empty-p cov-coverages))
          (should (= 1 (length (hash-table-keys cov-coverages))))
          (should (eq (car (hash-table-values cov-coverages)) covdata))
          (should (= 1 (length (cov-data-buffers covdata))))
          (should-not (memq (current-buffer) (cov-data-buffers covdata)))
          (should (eq (cov-data-watcher covdata) 'watch-descriptor)))))))

(ert-deftest cov-kill-buffer-hook-test-multiple-files ()
  "Test that `cov-kill-buffer-hook' can handle multiple files and buffers."
  :tags '(cov-kill-buffer-hook)
  (let* ((testfile1 (format "%s/gcov/same-dir/test" test-path))
         (testfile2 (format "%s/clover/test2" test-path))
         (covfile (concat testfile1 ".gcov"))
         (cloverfile (format "%s/clover/clover.xml" test-path))
         (cov-coverages (make-hash-table :test 'equal)))
    (mocker-let ((file-notify-add-watch (file flags callback)
                                        ((:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/gcov/same-dir" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor-gcov
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0))
                                         (:input-matcher
                                          (lambda (file flags callback)
                                            (and
                                             (string= (format "%s/clover" test-path) file)
                                             (equal flags '(change))
                                             (functionp callback)))
                                          ;; dummy watch descriptor
                                          :output 'watch-descriptor-clover
                                          ;; Will not be called if file-notify is not supported
                                          ;; by Emacs.
                                          :occur (if file-notify--library 1 0))))
                 (file-notify-rm-watch (descriptor)
                                       ((:input
                                         '(watch-descriptor-clover)
                                         :output nil
                                         :occur (if file-notify--library 1 0)))))
      (cov--with-test-buffer testfile1
        (let (covdata1)
          (cov-mode)
          ;; do not call cov-kill-buffer-hook from the hook
          (remove-hook 'kill-buffer-hook 'cov-kill-buffer-hook t)
          (setq covdata1 (gethash covfile cov-coverages))
          ;; add another buffer to the cov-data buffer list
          (cl-pushnew (get-buffer-create (symbol-name (cl-gensym)))
                      (cov-data-buffers covdata1))
          (cov--with-test-buffer testfile2
            (cov-mode)
            ;; do not call cov-kill-buffer-hook from the hook
            (remove-hook 'kill-buffer-hook 'cov-kill-buffer-hook t)
            (should (= 2 (length (hash-table-keys cov-coverages))))
            (cov-kill-buffer-hook)
            (should (= 1 (length (hash-table-keys cov-coverages))))
            (should (eq (car (hash-table-values cov-coverages)) covdata1))
            (should (= 2 (length (cov-data-buffers covdata1))))
            (should-not (memq (current-buffer) (cov-data-buffers covdata1)))))))))

;; cov-set-overlays
(ert-deftest cov-set-overlays-test-no-coverage ()
  (cov--with-test-buffer "gcov/same-dir/test"
    (mocker-let ((cov--get-buffer-coverage () ((:output nil)))
                 (message (format-sting &rest args)
                          ((:input `("No coverage data found for %s."
                                     ,(format "%s/gcov/same-dir/test" test-path))))))
      (cov-set-overlays))))

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

;; cov--set-overlay

;; cov-mode
(ert-deftest cov-mode--enable-test ()
  "Verify that `cov-mode' adds overlays."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 1)
    (should (equal (length (cov--overlays)) 9))))

(ert-deftest cov-mode--disable-test ()
  "Verify that `cov-mode' removes overlays when disabled."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 1)
    (cov-mode 0)
    (should (equal (cov--overlays) '()))))

(ert-deftest cov-mode--re-enable-test ()
  "Verify that calling `cov-mode' several times does not add more overlays."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 1)
    (cov-mode 1)
    (should (equal (length (cov--overlays)) 9))))

(ert-deftest cov-mode--overlay-start-test ()
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (cov-mode 1)
    (let ((overlay-starts (mapcar #'overlay-start (cov--overlays)))
          (expected '(15 36 57 78 99 120 141 162 183)))
      (should (equal (length overlay-starts) (length expected)))
      (ert-info ("Unexpected overlay starts")
        (should-not (cl-set-difference overlay-starts expected)))
      (ert-info ("Missing overlay starts")
        (should-not (cl-set-difference expected overlay-starts))))))

(ert-deftest cov-mode--overlay-end-test ()
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (cov-mode 1)
    (let ((overlay-ends (mapcar #'overlay-end (cov--overlays)))
          (expected '(35 56 77 98 119 140 161 182 204)))
      (should (equal (length overlay-ends) (length expected)))
      (ert-info ("Unexpected overlay ends")
        (should-not (cl-set-difference overlay-ends expected)))
      (ert-info ("Missing overlay ends")
        (should-not (cl-set-difference expected overlay-ends))))))

(ert-deftest cov-mode--overlays-narrowed-begin ()
  "Check narrowing to whole lines in the beginnig of the buffer."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (goto-char (point-min))
    ;; Narrow from start to the 85% line (1st to 5th inclusive)
    ;; including the terminating newline of line 5
    (narrow-to-region (point-min)
                      (progn (forward-line 5) (point)))
    (cov-mode 1)
    (widen)
    (let ((ov-regions (cl-loop for ov in (cov--overlays)
                               collect (list (overlay-start ov) (overlay-end ov))))
          (expected '((15 35)
                      (36 56)
                      (57 77))))
      (ert-info ("Unexpected overlay region")
        (should-not (cl-set-difference ov-regions expected :test #'equal)))
      (ert-info ("Missing overlay region")
        (should-not (cl-set-difference expected ov-regions :test #'equal))))))

(ert-deftest cov-mode--overlays-narrowed-end ()
  "Check narrowing to whole lines at the end of the buffer."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (goto-char (point-min))
    ;; Narrow from the 84% line to the last line (6st to 11th inclusive)
    ;; including the terminating newline of line 11
    (narrow-to-region (progn (forward-line 5) (point))
                      (point-max))
    (cov-mode 1)
    (widen)
    (let ((ov-regions (cl-loop for ov in (cov--overlays)
                               collect (list (overlay-start ov) (overlay-end ov))))
          (expected '((78 98)
                      (99 119)
                      (120 140)
                      (141 161)
                      (162 182)
                      (183 204))))
      (ert-info ("Unexpected overlay region")
        (should-not (cl-set-difference ov-regions expected :test #'equal)))
      (ert-info ("Missing overlay region")
        (should-not (cl-set-difference expected ov-regions :test #'equal))))))

(ert-deftest cov-mode--overlays-narrowed-middle ()
  "Check narrowing to whole lines in the middle of the buffer."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (goto-char (point-min))
    ;; Narrow to the 86% - 45% lines (4th to 8th inclusive) including
    ;; the terminating newline of line 8
    (narrow-to-region (progn (forward-line 3) (point))
                      (progn (forward-line 5) (point)))
    (cov-mode 1)
    (widen)
    (let ((ov-regions (cl-loop for ov in (cov--overlays)
                               collect (list (overlay-start ov) (overlay-end ov))))
          (expected '((36 56)
                      (57 77)
                      (78 98)
                      (99 119)
                      (120 140))))
      (ert-info ("Unexpected overlay region")
        (should-not (cl-set-difference ov-regions expected :test #'equal)))
      (ert-info ("Missing overlay region")
        (should-not (cl-set-difference expected ov-regions :test #'equal))))))

(ert-deftest cov-mode--overlays-narrowed-middle-broken-lines ()
  "Check narrowing to mid-lines in the middle of the buffer."
  (cov--with-test-buffer "gcov/same-dir/test"
    (cov-mode 0)
    (goto-char (point-min))
    (narrow-to-region (progn (forward-line 3) (+ 5 (point)))
                      (progn (forward-line 5) (+ 5 (point))))
    (cov-mode 1)
    (widen)
    (let ((ov-regions (cl-loop for ov in (cov--overlays)
                               collect (list (overlay-start ov) (overlay-end ov))))
          (expected '((36 56)
                      (57 77)
                      (78 98)
                      (99 119)
                      (120 140)
                      (141 161))))
      (ert-info ("Unexpected overlay region")
        (should-not (cl-set-difference ov-regions expected :test #'equal)))
      (ert-info ("Missing overlay region")
        (should-not (cl-set-difference expected ov-regions :test #'equal))))))

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
  (cov--with-test-buffer "gcov/same-dir/test"
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
        (should-not (cl-set-difference expected overlay-bfs :test cmp))))))

(ert-deftest cov-mode--overlay-help-test ()
  (cov--with-test-buffer "gcov/same-dir/test"
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
        (should (equal (overlay-get overlay 'help-echo) (pop expected)))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
