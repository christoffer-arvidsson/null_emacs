python-base-mode

(plt "import matplotlib.pyplot as plt" n> q)
(arg "parser.add_argument(\"-" p "\", \"--" p "\"," p ")" n>)
(arg_positional "parser.add_argument(\"" p "\", " p ")")
(ifmain "if __name__ == \"__main__\":" n> q)
