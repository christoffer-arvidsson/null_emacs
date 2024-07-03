org-mode

(py & "#+begin_src python" n r n "#+end_src")
(sh & "#+begin_src bash" n r n "#+end_src")
(el & "#+begin_src elisp" n r n "#+end_src")
(as & "#+begin_aside" n r n "#+end_aside")
(al & "#+begin_algoritmh" n r n "#+end_algorithm")
(pr & "#+begin_proof" n r n "#+end_proof")
(th & "#+begin_theorem" n r n "#+end_theorem")
(cs & "#+begin_columns" n r n "#+end_columns")
(co & "#+begin_column" n r n "#+end_column")

(on & "#+name: ")
(oc & "#+caption: ")
(oo & "#+attr_org: ")
(ol & "#+attr_latex: ")

(bib & "* References" n> "#+print_bibliography: ")

(notebook &
          "#+property: header-args:python :results value :session " (s session) n
          "#+call: plt-setup[:session " (s session) "]()" n q)

(equ "\\begin{equation}" n> r> n>"\\end{equation}")
