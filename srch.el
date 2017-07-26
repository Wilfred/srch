;;; srch.el --- fast, beautiful text search          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>

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

;; This package provides Emacs integration with dumb text search tools
;; like git-grep, rg and ag.

;;; Code:

;; other names: sniffer? sherlock? finder?

(require 'lv)

(defvar srch--tool "ag")
(defvar srch--tools (list "ag" "rg" "git-grep"))

(defun srch--prompt-header ()
  (lv-message "Tool: %s [C-c t]
Directory: %s
" (srch--format-list srch--tool srch--tools)
"foo"))

(defun srch--format-list (current options)
  (let ((fontified-options
         (--map
          (if (equal current it)
              (s-upcase it)
            it)
          options)))
    (format "[%s]" (s-join "|" fontified-options))))

(defun srch--cycle-tool ()
  (interactive)
  (setq srch--tool
        (nth (mod (1+ (-elem-index srch--tool srch--tools))
                  (length srch--tools))
             srch--tools))
  (srch--prompt-header))

(defvar srch-minibuffer-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap minibuffer-local-map)
    (define-key keymap (kbd "C-c t") #'srch--cycle-tool)
    keymap))

(srch-prompt)

(defun srch-prompt ()
  (interactive)
  (srch--prompt-header)
  (condition-case err
      (let ((search-term (read-from-minibuffer
                          "Search for: "
                          "foo"
                          srch-minibuffer-map)))
        (lv-delete-window)
        search-term)
    (quit
     (lv-delete-window))))

(provide 'srch)
;;; srch.el ends here
