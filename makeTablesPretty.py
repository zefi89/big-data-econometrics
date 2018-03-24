from os import listdir
from os.path import join

table_dir = "tables"

table_files = listdir(table_dir)

replacements = [
        ("lambda", "$\lambda$"),
        ("Correlation with PC forecasts (r=5)", "\hline\n  \makecell{Correlation with \\\\ PC forecasts (r=5)}"),
        ("Correlation with PC forecasts (r=10)", "\hline\n  \makecell{Correlation with \\\\ PC forecasts (r=10)}"),
        ("Decision tree", "\makecell{Decision \\\\ tree}"),
        ("Random forest", "\makecell{Random \\\\ forest}"),
        ("rr}", "rr|}"),
        ("{r", "{|c|"),
        ("cv", "CV"),
        ]

todelete = [
          "begin{table}",
          "centering",
          "end{table}",
        ]

for tf in table_files:

    with open (join(table_dir, tf), "r") as infile:

        if "pretty" in tf:
            continue

        data=infile.readlines()

        if "IPS10" in tf:
            label = "Industrial Production"
        elif "PUNEW" in tf:
            label = "Consumer Price Index"

        if "PC" in tf:
            descr = "Number of Principal Components"
        elif "RIDGE" in tf:
            descr = "In-sample Residual variance"
        elif "LASSO" in tf:
            descr = "Number of non-zero coefficients"
        else:
            descr = ""
        ncols = 0

        for i in range(len(data)):
            # Get the number of columns
            if "\\begin{tabular}" in data[i]:
                ncols = len(data[i].split("{")[-1].split("}")[0])

            if "\\begin{tabular}" in data[i-1]:
                data[i] = data[i] + "  \multicolumn{"+str(ncols)+"}{|c|}{"+label+"} \\\\\n"
                data[i] = data[i] + "\hline\n  {} & \multicolumn{"+str(ncols-1)+"}{c|}{"+descr+"} \\\\\n"

            if "lambda" in data[i]:
                lambdas = data[i].split(" & ")[1:]
                lambdas[-1] = lambdas[-1].split(" ")[0]
                lambdas = ['{0:.2f}'.format(float(x)) for x in lambdas]
                data[i] = "lambda & " + " & ".join(lambdas) + " \\\\"
                data[i] = data[i] + "  \hline\n"

            for r in replacements:
                data[i]  = data[i].replace(r[0], r[1])

            for td in todelete:
                if td in data[i]:
                    data[i] = ""

        with open(join(table_dir, tf.replace(".tex", "_pretty.tex")), 'w') as outfile:
            for l in data:
                outfile.write(l)
