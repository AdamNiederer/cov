(require 'f)

(defvar test-path
  (f-dirname (f-this-file)))

(when (require 'undercover nil t)
  ;; Track coverage, but don't send to coveralls (Travis will send it
  ;; to Codecov).
  (undercover "*.el" (:report-type :codecov)))
