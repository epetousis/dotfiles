;;; simple-html.el --- Simple HTML backend for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evangelos Petousis
;;
;; Author: Evangelos Petousis <evan@petousis.net>
;; Keywords: outlines, org
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A derived backend for Org Mode's Export Engine that simplifies the output HTML, so that WYSIWYG applications (like Slack's text editor) don't trip up.

;;; Code:

(require 'ox)
(require 'ox-html)

(defun org-simple-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Simple HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information.

The difference between this and the normal HTML headline export
function is that we avoid exporting a section div."
  (let* ((numberedp (org-export-numbered-headline-p headline info))
        (numbers (org-export-get-headline-number headline info))
        (level (+ (org-export-get-relative-level headline info)
                  (1- (plist-get info :html-toplevel-hlevel))))
        (todo (and (plist-get info :with-todo-keywords)
                   (let ((todo (org-element-property :todo-keyword headline)))
                     (and todo (org-export-data todo info)))))
        (todo-type (and todo (org-element-property :todo-type headline)))
        (priority (and (plist-get info :with-priority)
                       (org-element-property :priority headline)))
        (text (org-export-data (org-element-property :title headline) info))
        (tags (and (plist-get info :with-tags)
                   (org-export-get-tags headline info)))
        (full-text (funcall (plist-get info :html-format-headline-function)
                            todo todo-type priority text tags info))
        (contents (or contents "")))
    (concat
     (format "<strong>%s</strong>\n" full-text)
     contents)))

(defun org-simple-html-section (section contents info)
  "Transcode a SECTION element from Org to simple HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information.

Don't do anything fancy - just export the contents."
   (format "%s\n" contents))

(org-export-define-derived-backend 'simple-html 'html
  :menu-entry
  '(?s "Export to Simple HTML"
       ((?S "As HTML buffer" org-simple-html-export-as-html)
        (?s "As HTML file" org-simple-html-export-to-file)))
  :translate-alist '((headline . org-simple-html-headline)
		     (section . org-simple-html-section)))

;;;###autoload
(defun org-simple-html-export-as-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer."
  (interactive)
  (org-export-to-buffer 'simple-html "*Org Simple HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

;;;###autoload
(defun org-simple-html-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file."
  (interactive)
  (let ((file (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'simple-html file
      async subtreep visible-only body-only ext-plist)))

(provide 'simple-html)
