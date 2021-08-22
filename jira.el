(defun jira-create-buffer ()
  (when (get-buffer "*jira*") (kill-buffer "*jira*"))
  (get-buffer-create "*jira*")
)

;; REFACTOR

(defun get-kanban (id)
  "Gets all kanban entries for the project and inserts them into a new buffer"
  (let* ((b (jira-create-buffer))
         (url (format "https://la-dev.atlassian.net/rest/agile/1.0/board/%s/issue" id))
         (jira (gethash "issues" (get-jira url))))
    (switch-to-buffer b)
    (insert (map-workflow-statuses-to-org-notes))
    (insert "\n\n")
    (org-mode)
    (mapc 'parse-issue jira)
    (read-only-mode t)
    ))
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


