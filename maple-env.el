;;; maple-env.el --- Isolate emacs dependency environment.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Isolate Emacs dependency environment.
;;

;;; Code:

(defgroup maple-env nil
  "Isolate emacs dependency environment."
  :group 'maple)

(defcustom maple-env:buffer-name "*maple-env*"
  "Display buffer action."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:path (expand-file-name "cache/env" user-emacs-directory)
  "Root env path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:python-path (expand-file-name "python" maple-env:path)
  "Python env path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:golang-path (expand-file-name "golang" maple-env:path)
  "Golang env path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:npm-path (expand-file-name "npm" maple-env:path)
  "NPM env path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:python-command "/usr/bin/pip"
  "Python pip execute path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:golang-command "go"
  "Golang execute path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:npm-command "npm"
  "NPM execute path."
  :type 'string
  :group 'maple-env)

(defcustom maple-env:python-packages
  '("yapf" "flake8" "isort" "python-language-server")
  "Python dependency packages."
  :type 'list
  :group 'maple-env)

(defcustom maple-env:golang-packages
  '("github.com/nsf/gocode"
    "github.com/rogpeppe/godef"
    "github.com/golang/lint/golint"
    "github.com/haya14busa/gopkgs/cmd/gopkgs"
    "golang.org/x/tools/cmd/gopls")
  "Golang dependency packages."
  :type 'list
  :group 'maple-env)

(defcustom maple-env:npm-packages
  '("js-beautify")
  "NPM dependency packages."
  :type 'list
  :group 'maple-env)

(defun maple-env:process(name program &rest program-args)
  "Start process with NAME PROGRAM &REST PROGRAM-ARGS."
  (let* ((args (append (list name maple-env:buffer-name program) program-args))
         (process (apply 'start-process args)))
    (run-with-timer 3 nil 'maple-env:display process)
    (set-process-sentinel process 'maple-env:sentinel)))

(defun maple-env:sentinel(process msg)
  "Start process sentinel with PROCESS MSG."
  (when (memq (process-status process) '(exit signal))
    (message (concat (process-name process) " - " msg))))

(defun maple-env:display(process)
  "Display PROCESS buffer."
  (when (and (process-live-p process)
             (eq (process-status process) 'run))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (if (minibufferp)
            (switch-to-buffer-other-window buf)
          (pop-to-buffer buf))))))

(defmacro maple-env:pip (&rest body)
  "Execute the pip command in BODY."
  (declare (indent 0) (debug t))
  `(let ((process-environment process-environment)
         (name "maple-env:pip"))
     (push (format "%s=%s" "PYTHONUSERBASE" maple-env:python-path) process-environment) ,@body))

(defmacro maple-env:go (&rest body)
  "Execute the go command in BODY."
  (declare (indent 0) (debug t))
  `(let ((process-environment process-environment)
         (name "maple-env:go"))
     (push (format "%s=%s" "GOPATH" maple-env:golang-path) process-environment) ,@body))

(defmacro maple-env:npm (&rest body)
  "Execute the npm command in BODY."
  (declare (indent 0) (debug t))
  `(let ((process-environment process-environment)
         (name "maple-env:npm"))
     (push (format "%s=%s" "NODE_PATH" maple-env:npm-path) process-environment) ,@body))

(defun maple-env:pip-upgrade(package)
  "Python pip install PACKAGE."
  (interactive "P")
  (maple-env:pip
    (maple-env:process
     name maple-env:python-command "install" "--user" "--upgrade"
     (or package (completing-read
                  "Select python package: "
                  (split-string (shell-command-to-string "/usr/bin/pip list --user --format=freeze") "\n"))))))

(defun maple-env:pip-install(package)
  "Python pip install PACKAGE."
  (interactive "sPython package name: ")
  (maple-env:pip
    (maple-env:process
     name maple-env:python-command "install" "--user"
     package)))

(defun maple-env:go-install(package)
  "Python pip install PACKAGE."
  (interactive "sGolang package name: ")
  (maple-env:go
    (maple-env:process
     name maple-env:golang-command "get" "-u"
     package)))

(defun maple-env:npm-install(package)
  "Python pip install PACKAGE."
  (interactive "sNPM package name: ")
  (maple-env:npm
    (maple-env:process
     name maple-env:npm-command "install" "-g" "--prefix" maple-env:npm-path
     package)))

(defun maple-env:init()
  "Init all env."
  (interactive)
  (dolist (package maple-env:python-packages)
    (maple-env:pip-install package))
  (dolist (package maple-env:golang-packages)
    (maple-env:go-install package))
  (dolist (package maple-env:npm-packages)
    (maple-env:npm-install package)))

(defun maple-env:set(key value)
  "Set environment variable with KEY VALUE."
  (let* ((value (if (listp value) value (list value)))
         (path (split-string (or (getenv key) "") ":"))
         (path (delete-dups (append path value)))
         (path (delete "" path)))
    (setenv key (mapconcat 'identity path ":"))))

(defun maple-env:unset(key value)
  "UnSet environment variable with KEY VALUE."
  (let* ((value (if (listp value) value (list value)))
         (path (split-string (or (getenv key) "") ":"))
         (path (delete-if (lambda(x) (member x value)) path))
         (path (delete "" path)))
    (setenv key (mapconcat 'identity path ":"))))

(defun maple-env-mode-on()
  "Turn on maple-env-mode."
  (interactive)
  (dolist (path (list maple-env:python-path maple-env:golang-path maple-env:npm-path))
    (unless (file-exists-p path)
      (make-directory path t))
    (add-to-list 'exec-path (format "%s/bin" path)))

  (let* ((path (format "%s/lib" maple-env:python-path))
         (path (when (file-directory-p path) (car (directory-files path t "^python")))))
    (when path (maple-env:set "PYTHONPATH" (format "%s/site-packages" path))))
  (maple-env:set "NODE_PATH" (format "%s/lib/node_modules" maple-env:npm-path))
  (maple-env:set "GOPATH" maple-env:golang-path))

(defun maple-env-mode-off()
  "Turn off maple-env-mode."
  (interactive)
  (dolist (path (list maple-env:python-path maple-env:golang-path maple-env:npm-path))
    (setq exec-path (remove (format "%s/bin" path) exec-path)))

  (let* ((path (format "%s/lib" maple-env:python-path))
         (path (when (file-directory-p path) (car (directory-files path t "^python")))))
    (when path (maple-env:unset "PYTHONPATH" (format "%s/site-packages" path))))
  (maple-env:unset "NODE_PATH" (format "%s/lib/node_modules" maple-env:npm-path))
  (maple-env:unset "GOPATH" maple-env:golang-path))

;;;###autoload
(define-minor-mode maple-env-mode
  "Maple env mode"
  :group      'maple-env
  :global     t
  (if maple-env-mode (maple-env-mode-on) (maple-env-mode-off)))

(provide 'maple-env)
;;; maple-env.el ends here
