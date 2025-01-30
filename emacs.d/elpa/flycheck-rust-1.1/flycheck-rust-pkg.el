;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-rust" "1.1"
  "Flycheck: Rust additions and Cargo support."
  '((emacs     "24.1")
    (flycheck  "28")
    (dash      "2.13.0")
    (seq       "2.3")
    (let-alist "1.0.4"))
  :url "https://github.com/flycheck/flycheck-rust"
  :commit "a139cd53c5062697e9ed94ad80b803c37d999600"
  :revdesc "1.1-0-ga139cd53c506"
  :keywords '("tools" "convenience")
  :authors '(("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Sebastian Wiesner" . "swiesner@lunaryorn.com")))
