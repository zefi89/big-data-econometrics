all: OutSample.Rda
	Rscript OutSamplSummary.R
	sh corpPlots.sh
	python makeTablesPretty.py
	pdflatex report.tex

OutSample.Rda:
	Rscript OutSample.R

clean:
	rm -rf plots
	rm -rf tables
	rm OutSample.Rda
	rm OutSample.csv
	rm report.log
	rm report.aux
	rm report.out
