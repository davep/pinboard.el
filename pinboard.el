;;; pinboard.el --- pinboard.in client -*- lexical-binding: t -*-
;; Copyright 2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.1
;; Keywords: {{keywords}}
;; URL: https://github.com/davep/pinboard.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; pinboard.el provides a simple client for pinboard.in.

;;; Code:

(require 'json)
(require 'url-vars)

(defconst pinboard-api-url "https://api.pinboard.in/v1/%s?auth_token=%s&format=json"
  "Base URL of the Pinboard API.")

(defconst pinboard-agent "pinboard.el (https://github/davep/pinboard.el)"
  "User agent to send to the Pinboard server.")

(defvar pinboard-api-token ""
  "Your Pinboard account's API token.

Visit https://pinboard.in/settings/password to get the token for
your account.

DO NOT EVER SET THIS IN A WAY THAT IT CAN BE SEEN IN SOME PUBLIC
REPOSITORY!")

(defvar pinboard-pins nil
  "Cache of pins to display.")

(defun pinboard-api-url (&rest params)
  "Build the API call from PARAMS."
  (format pinboard-api-url (string-join params "/") pinboard-api-token))

(defun pinboard-call (url)
  "Call on URL and return the data."
  (let* ((url-request-extra-headers `(("User-Agent" . ,pinboard-agent)))
         (url-show-status nil))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read-from-string (buffer-string)))))

(defun pinboard-recent-posts ()
  "Return the user's recent posts on Pinboard."
  ;; TODO: Rate limit to once every 60 seconds.
  (pinboard-call (pinboard-api-url "posts" "recent")))

(defun pinboard-all-posts ()
  "Return all of the user's posts on Pinboard."
  ;; TODO: Rate limit to once every 5 minutes.
  (or
   pinboard-pins
   (setq pinboard-pins (pinboard-call (pinboard-api-url "posts" "all")))))

(define-derived-mode pinboard-mode tabulated-list-mode "Pinboard Mode"
  "Major mode for handling a list of Pinboard pins."
  (setq tabulated-list-format [("Description" 40 t) ("URL" 40 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun pinboard ()
  "Browse your pinboard pins."
  (interactive)
  (pop-to-buffer "*Pinboard*")
  (pinboard-mode)
  (setq tabulated-list-entries
        (mapcar (lambda (pin)
                  (list
                   (alist-get 'hash pin)
                   (vector
                    (alist-get 'description pin)
                    (alist-get 'href pin))))
                (pinboard-all-posts)))
  (tabulated-list-print t))

(provide 'pinboard)

;;; pinboard.el ends here
