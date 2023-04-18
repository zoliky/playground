;; -*- mode: lisp-data; -*-

org-mode

(title "#+title: " p n "#+author: Zoltan Kiraly" n "#+date: " (format-time-string "[%Y-%m-%d %a %H:%M]"))
(music ":PROPERTIES:" n ":Genre: " p n ":Barcode: " n ":Label: " n ":END:")
(src "#+begin_src " q n> r> n "#+end_src")

emacs-lisp-mode

(let "(let (" p ")" n> r> ")")

c-mode

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
