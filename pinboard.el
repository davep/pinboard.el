;;; pinboard.el --- pinboard.in client -*- lexical-binding: t -*-
;; Copyright 2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.1
;; Keywords: hypermedia, bookmarking, reading, pinboard
;; URL: https://github.com/davep/pinboard.el
;; Package-Requires: ((emacs "25"))

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

(require 'seq)
(require 'json)
(require 'url-vars)
(require 'browse-url)

(defconst pinboard-api-url "https://api.pinboard.in/v1/%s?auth_token=%s&format=json"
  "Base URL of the Pinboard API.")

(defconst pinboard-agent "pinboard.el (https://github/davep/pinboard.el)"
  "User agent to send to the Pinboard server.")

(defvar pinboard-api-token nil
  "Your Pinboard account's API token.

Visit https://pinboard.in/settings/password to get the token for
your account.

DO NOT EVER SET THIS IN A WAY THAT IT CAN BE SEEN IN SOME PUBLIC
REPOSITORY!")

(defvar pinboard-pins nil
  "Cache of pins to display.")

(defvar pinboard-last-5-min-call nil
  "The last time a call was made to a 5-minute-limited API.")

(defun pinboard-too-soon (last-symbol length)
  "Are we hitting on Pinboard too soon?

LAST-SYMBOL is the name of the `defvar' that will have recorded
the previous call. LENGTH is how long we're supposed to wait
between calls."
  (let ((last (symbol-value last-symbol)))
    (when last
      (<= (- (float-time) last) length))))

(defun pinboard-api-url (&rest params)
  "Build the API call from PARAMS."
  (format pinboard-api-url (string-join params "/") pinboard-api-token))

(defun pinboard-call (url limit-record)
  "Call on URL and return the data.

LIMIT-RECORD is a symbol that is the name of the variable that
records the time the call was made. This helps with managing rate
limiting."
  (let ((url-request-extra-headers `(("User-Agent" . ,pinboard-agent)))
        (url-show-status nil))
    (set limit-record (float-time))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read-from-string (buffer-string)))))

(defun pinboard-all-posts ()
  "Return all of the user's posts on Pinboard."
  (if (pinboard-too-soon 'pinboard-last-5-min-call 300)
      pinboard-pins
    (setq pinboard-pins
          (pinboard-call
           (pinboard-api-url "posts" "all")
           'pinboard-last-5-min-call))))

(defun pinboard-find-pin (hash)
  "Find and return the pin identified by HASH."
  (seq-find
   (lambda (pin)
     (string= (alist-get 'hash pin) hash))
   pinboard-pins))

(defun pinboard-redraw (&optional filter)
  "Redraw the pin list.

Optionally filter the list of pins to draw using the function
FILTER."
  (setq tabulated-list-entries
        (mapcar (lambda (pin)
                  (list
                   (alist-get 'hash pin)
                   (vector
                    (alist-get 'description pin)
                    (alist-get 'href pin))))
                (seq-filter (or filter #'identity) pinboard-pins)))
  (tabulated-list-print t))

(defun pinboard-open ()
  "Open the currently-highlighted pin in a web browser."
  (interactive)
  (let ((pin (pinboard-find-pin (tabulated-list-get-id))))
    (if pin
        (browse-url (alist-get 'href pin))
      (error "Could not find pin %s" (tabulated-list-get-id)))))

(defun pinboard-view ()
  "View the details of the currently-highlighted pin."
  (interactive)
  (let ((pin (pinboard-find-pin (tabulated-list-get-id))))
    (unless pin
      (error "Could not find pin %s" (tabulated-list-get-id)))
    ;; TODO: For now, this is just a dump of pin data to a help window. This
    ;; will be done a lot better if it's a nicely-formatted display.
    (with-help-window "*Pinboard pin*"
      (mapc (lambda (info)
              (princ (format "%s:\n%s\n\n" (car info) (cdr info))))
            pin))))

(defun pinboard-unread ()
  "Only show unread pins."
  (interactive)
  (pinboard-redraw (lambda (pin) (string= (alist-get 'toread pin) "yes"))))

(defun pinboard-refresh ()
  "Refresh the list."
  (interactive)
  (pinboard-all-posts)
  (pinboard-redraw))

(defvar pinboard-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g"         #'pinboard-refresh)
    (define-key map "u"         #'pinboard-unread)
    (define-key map "v"         #'pinboard-view)
    (define-key map (kbd "RET") #'pinboard-open)
    map)
  "Local keymap for `pinboard'.")

(define-derived-mode pinboard-mode tabulated-list-mode "Pinboard Mode"
  "Major mode for handling a list of Pinboard pins."
  (setq tabulated-list-format [("Description" 40 t) ("URL" 40 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun pinboard ()
  "Browse your pinboard pins."
  (interactive)
  (if (not pinboard-api-token)
      (error "Please set your Pinboard API token")
    (pop-to-buffer "*Pinboard*")
    (pinboard-mode)
    (pinboard-all-posts)
    (pinboard-redraw)))

(provide 'pinboard)

;;; pinboard.el ends here
