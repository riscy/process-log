;;; process-log.el --- Keep a log of subprocesses. -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: May 23 2011
;; Keywords: processes, logging
;; URL: https://github.com/riscy/process-log
;; Package-Requires: ((emacs "25"))
;; Version: 0

;; This file is free software; you can redistribute or modify it under the terms
;; of the GNU General Public License <https://www.gnu.org/licenses/>, version 3
;; or any later version.  This software comes with with NO WARRANTY, not even
;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; Keep a log of subprocesses.

;;; Code:

(require 'subr-x)

(defgroup process-log nil "Keep a log of subprocesses."
  :prefix "process-log"
  :group 'processes
  :link '(url-link :tag "GitHub" "https://github.com/riscy/process-log"))

(defcustom process-log-duplicates-p nil
  "Whether to show calls we've already seen."
  :type 'boolean)

(defvar process-log--duplicates nil "MD5-hashed calls we've already seen.")
(defvar process-log--trim        11 "Amount of call-stack to pre-trim.")
(defvar process-log--call-stack  20 "Amount of call-stack to recall.")

(defun process-log (func &rest args)
  "Around advice for FUNC which takes ARGS."
  (with-current-buffer (get-buffer-create "*process-log*")
    (goto-char (point-max))
    (let ((tmpstr ""))
      (dotimes (ii process-log--call-stack)
        (when-let ((bframe (cadr (backtrace-frame (+ ii process-log--trim)))))
          (setq tmpstr (format "%s  ...from %S\n" tmpstr bframe))))
      (when (or process-log-duplicates-p
                (not (member (md5 tmpstr) process-log--duplicates)))
        (insert (format "%S:\n%s\n" args tmpstr))
        (unless process-log-duplicates-p
          (add-to-list 'process-log--duplicates (md5 tmpstr))))))
  (apply func args))

;; TODO: global minor mode
(advice-add #'call-process :around #'process-log)
(advice-add #'make-process :around #'process-log)

(provide 'process-log)
;;; process-log.el ends here
