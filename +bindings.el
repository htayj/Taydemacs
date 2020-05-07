;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; eww and erc bindings
(map! :leader
      :prefix ("e" . "eww")
      :desc "search w3m duck"    "e" 'w3m-search
      :desc "search w3m duck new"    "w" 'w3m-search-new-session
      :desc "search dwim"    "d" 'eval-last-sexp
      :desc "search dwim"    "r" 'erc-switch-to-buffer
      :desc "search eww"    "t" 'eww)

;; media bindings
(map! :leader
      :prefix ("D" . "media"))

;; EXWM related bindings
(map! :leader
      :prefix ("d" . "EXWM")
      )
;; ivy-run-external-command

;; org mode
(map! :leader
      :prefix ("a" . "org-global")
      :desc "capture"    "s" 'org-capture
      :desc "search dwim"    "d" 'eval-last-sexp
      )
;; layout
(map! :leader
      :prefix ("l" . "layout")
      :desc "ivy switch view"    "s" 'ivy-switch-view
      :desc "ivy pop view"    "d" 'ivy-pop-view
      :desc "ivy push view"    "t" 'ivy-push-view)

;; text manipulations
(map! :leader
      "x" nil
      (:prefix ("x" . "text")
        (:prefix ("i" . "inflection")
         :desc "camelCase"    "c" 'string-inflection-lower-camelcase
         :desc "PascalCase"    "C" 'string-inflection-camelcase
         :desc "snake_case"    "s" 'string-inflection-underscore
         :desc "kebab-case"    "k" 'string-inflection-kebab-case)))

;; exwm prefix
(define-prefix-command 'taymacs/metaspace-map)

(general-def taymacs/metaspace-map
  "w" 'winner-ring)
