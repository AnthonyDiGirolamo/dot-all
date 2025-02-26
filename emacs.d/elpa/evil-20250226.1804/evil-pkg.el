;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250226.1804"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "5cc78bb22533482fdd5905f10950408156eb40a1"
  :revdesc "5cc78bb22533"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
