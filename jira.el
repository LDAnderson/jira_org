
;; TODO
(defun jira-mode ()
  nil)

;; DONE

(defvar jira-email nil
  "Email address to use for JIRA authentication")
(defvar jira-token nil
  "API token for use in JIRA authentication")

(defun get-kanban (id)
  "Gets all kanban entries for the project and inserts them into a new buffer"
  (let* ((b (get-buffer-create "*jira*"))
         (url (format "https://la-dev.atlassian.net/rest/agile/1.0/board/%s/issue" id))
         (jira (gethash "issues" (get-jira url))))
    (switch-to-buffer b)
    (insert (map-workflow-statuses-to-org-notes))
    (insert "\n\n")
    (org-mode)
    (mapc 'parse-issue jira)
    (read-only-mode t)
    ))

(defun auth-and-get-buffer (url)
  (let* ((headers url-request-extra-headers)
         (auth_str (format "%s:%s" jira-email jira-token))
         (encoded_auth_str (base64-encode-string auth_str t)))
         (setq url-request-extra-headers `(("Authorization" . ,(format "Basic %s" encoded_auth_str)) ("Content-Type" . "application/json")))
         (let ((r (url-retrieve-synchronously url)))
           (setq url-request-extra-headers headers)
           r)))

(defun get-issue-field (f j)
  (gethash f (gethash "fields" j)))

(defun write-line (level txt)
  (insert (format "%s %s\n" (make-string level ?*) txt)))

(defun parse-issue (j)
  (write-line 2
              (concat
               (upcase-and-remove-spaces (gethash "name" (get-issue-field "status" j)))
               " "
               (get-issue-field "summary" j)))
  (write-line 3 (get-issue-field "created" j))
  (write-line 3 (get-issue-field "description" j))
  )

(defun get-jira (url)
  (json-parse-string (buffer-to-string-clear-header (auth-and-get-buffer url))))

(defun buffer-to-string (b)
  (save-excursion
    (set-buffer b)
    (buffer-string)))

(defun buffer-to-string-clear-header (b)
  (save-excursion
    (set-buffer b)
    (re-search-forward "^")
    (buffer-substring (point) (point-max))))

(defun get-issue-props (rq prop)
  (seq-map (lambda (x) (gethash prop (gethash "fields" x))) (gethash "issues" rq)))

(defun get-issue-names (rq)
  (get-issue-props rq "summary"))

(defun deprecated-get-issue-names (rq)
  (seq-map (lambda (x) (gethash "summary" (gethash "fields" x))) (gethash "issues" rq)))

(defun get-first-issue-name ()
  (gethash "summary" (gethash "fields" (aref (gethash "issues" j) 0))))

(defun lh (ht)
  (end-of-line -1)
  (insert "\n")
  (maphash (lambda (x y) (insert (format "%s\n" x))) ht))

(defun upcase-and-remove-spaces (s)
  (upcase (replace-all-regex " " "" s)))

(defun map-workflow-statuses-to-org-notes ()

  (let ((r (delete "Done" (delete "Backlog" (get-workflow-statuses)))))
    (concat "#+TODO: "
          (mapconcat
                      (lambda (x) (upcase (replace-all-regex " " "" x)))
                      r " ")
          " | BACKLOG DONE")))

(defun get-workflow-statuses ()
  (seq-map (lambda (x) (gethash "name" x))
           (json-parse-string
            (buffer-to-string-clear-header
                      (auth-and-get-buffer "https://la-dev.atlassian.net/rest/api/3/status")))))

(defun replace-all-regex (rexp replaces str)
  (let ((r str))
    (while (string-match rexp r)
      (setq r (replace-match replaces nil nil r)))
    r))


