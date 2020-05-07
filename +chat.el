;;; ~/.doom.d/+chat.el -*- lexical-binding: t; -*-


(load! "+chatcred")
(load! "+signald")
;; (load! "bitlbee")
(after! erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))
(setq erc-join-buffer 'bury))
