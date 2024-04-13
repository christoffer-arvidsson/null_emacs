python-base-mode

(plt "import matplotlib.pyplot as plt" n> q)
(arg "parser.add_argument(\"-" p "\", \"--" p "\"," p ")" n>)
(arg_positional "parser.add_argument(\"" p "\", " p ")")
(ifmain "if __name__ == \"__main__\":" n> "main()"q)
(super_init "super().__init__(" p ")" n> q)
(sinit "super().__init__(" p ")" n> q)

(dc "@dataclass" n> "class " p ":" n> q)
