;;; ~/.doom.d/bitlbee.el -*- lexical-binding: t; -*-
;; https://www.emacswiki.org/emacs/bitlbee.el
;; ended up using systemd instead
(defvar bitlbee-user-directory "~/.bitlbee"
  "The directory where user configuration goes")

(defvar bitlbee-options "-n -F -v "
  "The options passed to Bitlbee on the command line.")

(defvar bitlbee-executable "bitlbee"
  "The full path to the Bitlbee executable")

(defvar bitlbee-buffer-name " *bitlbee*"
  "The name of the bitlbee process buffer")

(defun bitlbee-running-p ()
  "Returns non-nil if bitlbee is running"
  (if (get-buffer-process bitlbee-buffer-name) t nil))

(defun bitlbee-start ()
  "Start the bitlbee server"
  (interactive)
  (if (bitlbee-running-p) (message "bitlbee is already running")
    (make-directory (expand-file-name bitlbee-user-directory) t)
    (let ((proc (start-process-shell-command "bitlbee" bitlbee-buffer-name bitlbee-executable (bitlbee-command-line))))
      (set-process-sentinel proc 'bitlbee-sentinel-proc))
      (message "started bitlbee")))

(defun bitlbee-stop ()
  "Stop the bitlbee server"
  (interactive)
  (let ((proc (get-buffer-process bitlbee-buffer-name)))
    (when proc (kill-process proc t))))

(defun bitlbee-sentinel-proc (proc msg)
  (when (memq (process-status proc) '(exit signal))
    (setq msg (replace-regexp-in-string "\n" "" (format "stopped bitlbee (%s)" msg)))
  (message msg)))

(defun bitlbee-command-line ()
  "Create the full command line necessary to run bitlbee"
  (concat bitlbee-options " -d " bitlbee-user-directory " -c " bitlbee-user-directory "/bitlbee.conf"))

(provide 'bitlbee)
