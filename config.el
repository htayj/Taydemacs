;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Taylor"
      user-mail-address "nobody@nogroup.group")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hack" :size 14))

;; IRC
(load! "+chat")
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-outrun-electric)
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes/"
      org-agenda-files (directory-files-recursively "~/Dropbox/notes/" "\.org$") ;;look for .org files here for the agenda.
      org-archive-location (concat org-directory "archive/%s::"))
(setq org-journal-dir "~/Dropbox/notes/journal/"
      org-journal-file-format "%Y-%m-%d"
      org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%Y-%m-%d"
      org-journal-time-prefix "* "
      org-journal-time-format "%H:%M:%S")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; spacemacs escape sequence
(setq-default evil-escape-key-sequence "fd")
;; prettier for formatted code
(use-package! prettier-js)
(after! prettier-js
  (add-hook! 'js2-mode-hook 'prettier-js-mode)
  (add-hook! 'js-mode-hook 'prettier-js-mode)
  (add-hook! 'web-mode-hook 'prettier-js-mode))
;; eslint
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
;; eww
(setq eww-search-prefix "https://duckduckgo.com/lite?q=")

(use-package! string-inflection)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; emojify modeline mode doesnt work correctly, makes focus wierd
;; (use-package! emojify)                 ;
;; (after! emojify
;;   (global-emojify-mode t)
;;   (global-emojify-mode-line-mode nil))
;; w3m for web browsing


(setq-default explicit-shell-file-name "/bin/zsh")

;; todo: phase out w3m
(use-package! w3m)
(after! w3m
  ;; (require 'w3m-load)
  ;; (require 'mime-w3m)
  (setq w3m-search-default-engine "duckduckgo"))

;; Bindings
(load! "+bindings")
;; exwm
(load! "+exwm")
