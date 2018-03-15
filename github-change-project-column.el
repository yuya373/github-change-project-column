;;; github-change-project-column.el ---              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@yuya373>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'request)

(defcustom github-change-project-column-github-token nil
  "Use to GithubAPI."
  :group 'github-change-project-column
  :type 'string)

(defcustom github-change-project-column-issue-number-parser nil
  "Parser to read issue number from buffer."
  :group 'github-change-project-column
  :type 'function)

(defcustom github-change-project-column-repository-owner-parser nil
  "Parser to read repository owner from buffer."
  :group 'github-change-project-column
  :type 'function)

(defcustom github-change-project-column-repository-name-parser nil
  "Parser to read repository name from buffer."
  :group 'github-change-project-column
  :type 'function)

(defcustom github-change-project-column-repository-owner nil
  "Variable for repository owner."
  :group 'github-change-project-column
  :type 'string)

(defcustom github-change-project-column-repository-name nil
  "Variable for repository name."
  :group 'github-change-project-column
  :type 'string)

(defun github-change-project-column-issue-number ()
  "Return Issue number."
  (let ((number (if (functionp github-change-project-column-issue-number-parser)
                    (funcall github-change-project-column-issue-number-parser)
                  (read-from-minibuffer "Issue Number: #"))))
    (if (< 0 (length number))
        number
      (error "Specify Issue Number"))))


(defun github-change-project-column-repository-owner ()
  "Return repository owner name."
  (let ((owner (if (functionp github-change-project-column-repository-owner-parser)
                   (funcall github-change-project-column-repository-owner-parser)
                 (read-from-minibuffer "Repository Owner: "
                                       github-change-project-column-repository-owner))))
    (if (< 0 (length owner))
        owner
      (error "Specify Repository Owner"))))

(defun github-change-project-column-repository-name ()
  "Return repository name."
  (let ((name (if (functionp github-change-project-column-repository-name-parser)
                  (funcall github-change-project-column-repository-name-parser)
                (read-from-minibuffer "Repository Name: "
                                      github-change-project-column-repository-name))))
    (if (< 0 (length name))
        name
      (error "Spacify Repository Name"))))

(defvar github-change-project-column-project-columns-query
  (mapconcat #'identity
             (list
              "query projectColumns($owner: String!, $name: String!, $issueNumber: Int!, $projectCardsFirst: Int!, $projectColumnsFirst: Int!) {"
              "repository(owner: $owner, name: $name) {"
              "issue(number: $issueNumber) {"
              "projectCards(first: $projectCardsFirst) {"
              "nodes {"
              "id"
              "projectColumn {"
              "name"
              "id"
              "}"
              "project {"
              "columns(first: $projectColumnsFirst) {"
              "nodes {"
              "id"
              "name"
              "}"
              "}"
              "}"
              "}"
              "}"
              "}"
              "}"
              "}"
              )
             "\n"))

(defun github-change-project-column-choose-next-column (on-success)
  (interactive)
  (cl-labels
      ((on-success (data)
                   (let* ((project-card (car (plist-get
                                              (plist-get
                                               (plist-get
                                                (plist-get
                                                 (plist-get data :data)
                                                 :repository)
                                                :issue)
                                               :projectCards)
                                              :nodes)))
                          (project (plist-get project-card :project))
                          (columns (plist-get (plist-get project :columns)
                                              :nodes))
                          (current-column (plist-get project-card :projectColumn))
                          (next-column-name (completing-read
                                             (format "Which Column? (Current: %s) "
                                                     (plist-get current-column :name))
                                             (mapcar #'(lambda (column)
                                                         (cons (plist-get column :name)
                                                               column))
                                                     (cl-remove-if #'(lambda (e)
                                                                       (string= (plist-get current-column :name)
                                                                                (plist-get e :name)))
                                                                   columns))
                                             nil t))
                          (next-column (cl-find-if #'(lambda (e) (string= next-column-name
                                                                          (plist-get e :name)))
                                                   columns)))
                     (funcall on-success project-card next-column)))
       (on-error (data)
                 (message "Error in projectColumns: %s" (plist-get data :errors))))
    (github-change-project-column-request
     github-change-project-column-project-columns-query
     github-change-project-column-github-token
     :variables
     (list (cons "owner" (github-change-project-column-repository-owner))
           (cons "name" (github-change-project-column-repository-name))
           (cons "issueNumber" (string-to-number
                                (github-change-project-column-issue-number)))
           (cons "projectCardsFirst" 1)
           (cons "projectColumnsFirst" 100))
     :success #'on-success
     :error #'on-error)))

(defvar github-change-project-column-move-card-mutation
  (mapconcat #'identity
             (list
              "mutation moveCard($cardID: ID!, $columnID:ID!) {"
              "moveProjectCard(input: {cardId: $cardID, columnId: $columnID}) {"
              "cardEdge {"
              "node {"
              "column {"
              "name"
              "}"
              "}"
              "}"
              "}"
              "}"
              )
             "\n"))

(defun github-change-project-column-move-card (project-card column)
  (cl-labels
      ((on-success (data)
                   (let ((moved-column (plist-get
                                        (plist-get
                                         (plist-get (plist-get (plist-get data :data)
                                                               :moveProjectCard)
                                                    :cardEdge)
                                         :node)
                                        :column)))
                     (message "Card moved from to [%s]"
                              (plist-get moved-column :name))))
       (on-error (data)
                 (message "Error in moveProjectCard: %s" (plist-get data :errors))))
    (github-change-project-column-request
     nil
     github-change-project-column-github-token
     :mutation github-change-project-column-move-card-mutation
     :variables (list (cons "cardID" (plist-get project-card :id))
                      (cons "columnID" (plist-get column :id)))
     :success #'on-success
     :error #'on-error)))

(defun github-change-project-column-move ()
  (interactive)
  (github-change-project-column-choose-next-column
   #'github-change-project-column-move-card))


(cl-defun github-change-project-column-request (query token &key (variables nil) success error http-error (mutation nil))
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (if (plist-get data :errors)
                       (funcall error data)
                     (funcall success data)))
       (on-error (&key error-thrown symbol-status response data &allow-other-keys)
                 (if (functionp http-error)
                     (funcall http-error
                              :error-thrown error-thrown
                              :symbol-status symbol-status
                              :response response
                              :data data)
                   (message "ERROR-THROWN: %s, SYMBOL-STATUS: %s"
                            error-thrown
                            symbol-status))))
    (let ((url "https://api.github.com/graphql")
          (headers (list (cons "Authorization" (format "bearer %s" token))
                         (cons "User-Agent" "Emacs")
                         (cons "Content-Type" "application/json"))))
      (request url
               :type "POST"
               :data (json-encode (list
                                   (cons "query" (or mutation query))
                                   (cons "variables" variables)))
               :headers headers
               :parser #'(lambda () (let ((json-object-type 'plist)
                                          (json-array-type 'list))
                                      (json-read)))
               :error #'on-error
               :success #'on-success))))

(provide 'github-change-project-column)
;;; github-change-project-column.el ends here
