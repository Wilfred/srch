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

;; other names: sniffer? sherlock? finder? rummage? frisk? seek?
;; scour? forage?

(require 'lv)
(require 'f)
(require 'dash)
(autoload 'projectile-project-root "projectile")

(defvar srch--settings
  '(:tools
    (:options ("ag" "rg" "git-grep") :value "ag")
    :case
    (:options ("auto" "match-case" "ignore-case") :value "auto")
    :directory nil))

(defun srch--propertize-option (pretty-name settings-key key)
  (format "%s %s %s"
          (propertize (concat pretty-name ":") 'face 'minibuffer-prompt)
          (plist-get srch--settings settings-key)
          (propertize (format "C-c %s" key) 'face 'font-lock-string-face)))

(defun srch--prompt-header ()
  (let* ((tool
          (format "%s %s [C-c t]"
                  (propertize "Tool:" 'face 'minibuffer-prompt)
                  (-> srch--settings
                      (plist-get :tools)
                      (plist-get :value))))
         (dir
          (srch--propertize-option "Folder" :directory "f"))
         (file-types (srch--propertize-option "File types" :types "t"))
         (hidden (srch--propertize-option "Hidden" :hidden "h"))
         (ignores (srch--propertize-option "VCS-Ignores" :ignores "i"))
         
         (summary (format "%s
%s
%s %s %s
Case: [sensitive|insensitive|auto] Syntax: [literal string] \n---"
                          tool dir file-types hidden ignores))
         )
    (setq wh/s summary)
    (lv-message summary)))

(lv-message "Tool:
  git-grep [C-c t]

Files:
  ~/projects/srch/ [C-c f]
  all file types [C-c t]
  skipping files in .gitignore, skipping dotfiles

Search syntax:
  literal string [C-c s]
  auto case [C-c c]
---")

(setq srch--opts
      '("Tool:"
        ((:func srch--cycle-tool))))

(defun srch--choose-tool ()
  (interactive)
  (message "interactive: %S" (called-interactively-p 'any)))

(lv-message "")
(lv-delete-window)

(defun srch--set-directory ()
  (interactive)
  (lv-delete-window)
  (let* ((current (plist-get srch--settings :directory))
         (new (read-directory-name "Base directory: "
                                   current)))
    (plist-put srch--settings :directory
               (f-abbrev (f-slash new))))
  (srch--prompt-header))

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
  (let* ((tool-settings (plist-get srch--settings :tools))
         (tools (plist-get tool-settings :options))
         (current (plist-get tool-settings :value)))
    (plist-put
     tool-settings :value
     (nth (mod (1+ (-elem-index current tools))
               (length tools))
          tools)))
  (srch--prompt-header))

(defvar srch-minibuffer-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap minibuffer-local-map)
    (define-key keymap (kbd "C-c t") #'srch--cycle-tool)
    (define-key keymap (kbd "C-c f") #'srch--set-directory)
    keymap))

(defun srch-prompt ()
  (interactive)
  (plist-put srch--settings :directory
             (f-abbrev (projectile-project-root)))
  (srch--prompt-header)
  (condition-case err
      (let ((search-term (read-from-minibuffer
                          "Search term: "
                          nil
                          srch-minibuffer-map)))
        (lv-delete-window)
        search-term)
    (quit
     (lv-delete-window))))

(provide 'srch)
;;; srch.el ends here
