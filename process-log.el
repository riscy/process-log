;;; process-log.el --- Log subprocess calls -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: May 23 2011
;; Keywords: processes, logging
;; URL: https://github.com/riscy/process-log
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

;;; Commentary:

;; A minor mode to keep a log of processes invoked by Emacs and Emacs packages.
;; This can be useful for benchmarking, debugging, auditing, or just for
;; curiosity's sake.  Note this may affect the performance of some packages.

;;; Code:

(require 'subr-x)

(defgroup process-log nil "Keep a log of subprocesses."
  :prefix "process-log"
  :group 'processes
  :link '(url-link :tag "GitHub" "https://github.com/riscy/process-log"))
(defvar process-log-buffer "*process-log*")

(defun process-log (func &rest args)
  "Advice around FUNC, which takes ARGS."
  (let* ((blame (process-log--blame))
         (text (process-log--cmd args)))
    (with-current-buffer (get-buffer-create process-log-buffer)
      (or font-lock-mode (font-lock-mode 1))
      (goto-char (point-max))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S") "\t" blame "\t" text "\n")))
  (apply func args))

(defun process-log--blame ()
  "Get a string blaming which package probably invoked the process.
It will be propertized with the backtrace that led to the call."
  (let ((frames "")
        (offset 10)
        (blame nil))
    (while (and
            (backtrace-frame offset)
            (not (eq (cadr (backtrace-frame offset)) #'call-process))
            (not (eq (cadr (backtrace-frame offset)) #'make-process)))
      (setq offset (+ offset 1)))  ; frames starting here are relevant!
    (dotimes (backtrace-depth 25)
      (when-let ((frame (cadr (backtrace-frame (+ offset 2 backtrace-depth)))))
        (setq frames (concat frames (process-log--frame-str frame)))
        (unless blame
          (setq blame
                (cond
                 ((eq frame 'dumb-jump-run-command)             " dumb-jump") ; nofmt
                 ((eq frame 'eshell-external-command)           "    eshell") ; nofmt
                 ((eq frame 'flycheck-syntax-check-start)       "  flycheck") ; nofmt
                 ((eq frame 'git-commit-setup-font-lock)        "     magit") ; nofmt
                 ((eq frame 'git-gutter:start-git-diff-process) "git-gutter") ; nofmt
                 ((eq frame 'git-gutter:in-repository-p)        "git-gutter") ; nofmt
                 ((eq frame 'magit-diff-highlight)              "     magit") ; nofmt
                 ((eq frame 'magit-diff-update-hunk-refinement) "     magit") ; nofmt
                 ((eq frame 'magit-process-file)                "     magit") ; nofmt
                 ((eq frame 'magit-refresh-buffer)              "     magit") ; nofmt
                 ((eq frame 'magit-start-process)               "     magit") ; nofmt
                 ((eq frame 'shell)                             "     shell") ; nofmt
                 ((eq frame 'term)                              "      term") ; nofmt
                 ((eq frame 'vc-call-backend)                   "        vc") ; nofmt
                 )))))
    (propertize
     (if blame
         (propertize blame 'font-lock-face 'font-lock-constant-face)
       (propertize "   unknown" 'font-lock-face 'warning))
     ;; NOTE: newlines turn into ", "  help-echo appears in minibuffer
     'help-echo
     (string-trim-right frames))))

(defun process-log--cmd (args)
  "Return a string identifying the shell command in ARGS.
ARGS is assumed to be the argument list from either
`make-process' (which can take a set of keyword arguments) or
`call-process' (which takes a normal list of arguments)."
  (let (bin arglist)
    (if-let ((command (plist-get args :command)))
        (setq bin (car command) arglist (cdr command))
      (setq bin (car args) arglist (cddddr args)))
    (setq arglist (format "%s" (mapconcat #'identity arglist " ")))
    (concat (propertize (file-name-base bin) 'help-echo bin) " " arglist)))

(defun process-log--frame-str (frame)
  "Return a string representation of FRAME.
FRAME might be a function symbol, or something byte-compiled."
  (cond
   ((eq frame #'apply) "") ; uninteresting
   ((eq frame #'cond) "") ; uninteresting
   ((eq frame #'if) "") ; uninteresting
   ((eq frame #'if*) "") ; uninteresting
   ((eq frame #'let) "") ; uninteresting
   ((eq frame #'let*) "") ; uninteresting
   ((eq frame #'progn) "") ; uninteresting
   ((eq frame #'save-excursion) "") ; uninteresting
   ((symbolp frame)
    (format "%S\n" frame))
   (t "#[compiled]\n")))

;;;###autoload
(define-minor-mode process-log-mode
  "Process log mode."
  :global t
  (if (not process-log-mode)
      (process-log-unload-function)
    (process-log--reset)
    ;; NOTE start-process just calls make-process:
    (advice-add #'make-process :around #'process-log)
    (advice-add #'call-process :around #'process-log)
    (message "Logging processes to %s" process-log-buffer)))

(defun process-log-unload-function ()
  "Pre-cleanup when `unload-feature' is called."
  (advice-remove #'make-process #'process-log)
  (advice-remove #'call-process #'process-log)
  nil)

(defun process-log--reset ()
  "Reset the process log."
  (with-current-buffer (get-buffer-create process-log-buffer) (erase-buffer)))

(provide 'process-log)
;;; process-log.el ends here
