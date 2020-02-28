;;; ~/.doom.d/+chat.el -*- lexical-binding: t; -*-


(load! "+cred")
(load! "+signald")
;; (load! "bitlbee")
(after! erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))
(setq erc-join-buffer 'bury)
  )
