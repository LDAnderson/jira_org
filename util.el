

(defun replace-all-regex (rexp replaces str)
  (let ((r str))
    (while (string-match rexp r)
      (setq r (replace-match replaces nil nil r)))
    r))
