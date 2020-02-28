;;; ~/.doom.d/+exwm.el -*- lexical-binding: t; -*-
;; mostly ripped from https://github.com/alaq/dotfiles/blob/master/.doom.d/config.el
(display-time-mode)
(defun exwm-config-custom ()
  "Default configuration of EXWM. But customized slightly."
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 2))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Multiple monitor setup
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(1 "DP-4" 2 "HDMI-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DP-4 --right-of HDMI-0 --auto")))
  (message "Enabling multiple monitors!")
  (exwm-randr-enable)

  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ([?\M-i] . evil-move-divider-to-up)
            ([?\M-u] . evil-move-divider-to-down)
            ([?\M-y] . evil-move-divider-to-left)
            ([?\M-o] . evil-move-divider-to-right)
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ;; ,@(mapcar (lambda (i)
            ;;             `(,(kbd (format "s-%d" (+ 1 i))) .
            ;;               (lambda ()
            ;;                 (interactive)
            ;;                 (+workspace/switch-to ,i))))
            ;;           (number-sequence 0 8))
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" (+ 1 i))) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch ,i))))
                      (number-sequence -1 8)))))
  ;; Line-editing shortcuts
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete]))))
  ;; Enable EXWM
  (exwm-enable)
  (exwm-init)
  ;; Other configurations
  (exwm-config-misc))

(require 'exwm)
(after! exwm
  (require 'exwm-config)

  (exwm-config-custom)
  (exwm-input-set-key (kbd "M-h") #'evil-window-left)
  (exwm-input-set-key (kbd "M-j") #'evil-window-down)
  (exwm-input-set-key (kbd "M-k") #'evil-window-up )
  (exwm-input-set-key (kbd "M-l") #'evil-window-right)
  (exwm-input-set-key (kbd "M-u") #'evil-window-decrease-height)
  (exwm-input-set-key (kbd "M-i") #'evil-window-increase-height)
  (exwm-input-set-key (kbd "M-SPC") #'counsel-linux-app)
  (exwm-input-set-key (kbd "M-f") #'doom/window-maximize-buffer)
  (exwm-input-set-key (kbd "M-RET") #'eshell-toggle) ; Currently not working
  (exwm-input-set-key (kbd "M-b") #'exwm-workspace-switch-to-buffer)
  (evil-set-initial-state 'exwm-mode 'normal)

  ;; in normal state/line mode, use the familiar i key to switch to input state
  ;; from https://github.com/timor/spacemacsOS/blob/master/packages.el#L152
  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
  (push ?\i exwm-input-prefix-keys)
  (push ?\  exwm-input-prefix-keys))

(defun spacemacs/exwm-switch-to-buffer-or-run (window-class command)
  "Switch to first buffer with window-class, and if not present, run command."
  (let ((buffer
         (find window-class (buffer-list) :key (lambda(b) (cdr (assoc 'exwm-class-name (buffer-local-variables b)))) :test 'string-equal)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

(defun spacemacs/exwm-bind-switch-to-or-run-command (key window-class command)
  (exwm-input-set-key (kbd key)
                      `(lambda ()
                         (interactive)
                         (spacemacs/exwm-switch-to-buffer-or-run ,window-class ,command))))

(spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")


;; For Persp + EXWM compatibility
;; Source: https://www.reddit.com/r/emacs/comments/d8cd1h/a_simple_hack_for_persp_exwm_compatibility/
(defun exwm--update-utf8-title-advice (oldfun id &optional force)
"Only update the exwm-title when the buffer is visible."
  (when (get-buffer-window (exwm--id->buffer id))
    (funcall oldfun id force)))
(advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice)


(defun exwm-swap-monitors ()
  "Swaps your workspaces, between two monitors."
  (interactive)
  (exwm-workspace-swap 1 2))

(defun exwm-switch-to-monitors ()
  "Swaps your workspaces, between two monitors."
  (interactive)
  (exwm-workspace-switch 1))

(defun exwm-workspace-next (&optional reverse)
  (interactive "P")
  (let ((fn (if reverse #'- #'+)))
    (exwm-workspace-switch (mod (apply fn (list 1 exwm-workspace-current-index))
                                (- (length (frame-list)) 1)))))
(defun exwm-workspace-prev (&optional reverse)
  (interactive "P")
  (exwm-workspace-next t))
(exwm-input-set-key (kbd "s-j") 'exwm-workspace-next)
(exwm-input-set-key (kbd "s-k") 'exwm-workspace-prev)
(exwm-input-set-key (kbd "s-x") 'kill-this-buffer)

(add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
