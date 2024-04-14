python-base-mode

(plt & "import matplotlib.pyplot as plt" n> q)
(pd & "import pandas as pd" n> q)
(sns & "import seaborn as sns" n> q)
(tflow & "import tensorflow as tf" n> q)
(tqdm & "from tqdm import tqdm" n> q)

(arg "parser.add_argument(\"-" p "\", \"--" p "\"," p ")" n>)
(arg_positional "parser.add_argument(\"" p "\", " p ")")
(ifmain "if __name__ == \"__main__\":" n> "main()"q)
(sinit "super().__init__(" p ")" n> q)

(dc "@dataclass" n> "class " p ":" n> q)
(call "def __call__(self, " p "):" n> q)
