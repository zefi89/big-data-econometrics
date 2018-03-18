all: OutSample.Rda plots/IPS10_forest-crop.pdf tables/LASSO_IPS10_pretty.tex
	pdflatex report.tex

OutSample.Rda:
	Rscript OutSample.R

plots/IPS10_forest-crop.pdf: plots/IPS10_forest.pdf
	sh cropPlots.sh

tables/LASSO_IPS10_pretty.tex: tables/LASSO_IPS10.tex
	python makeTablesPretty.py

plots/IPS10_forest.pdf:
	Rscript OutSampleSummary.R

tables/LASSO_IPS10.tex:
	Rscript OutSampleSummary.R

clean:
	rm -rf plots
	rm -rf tables
	rm OutSample.Rda
	rm OutSample.csv
	rm report.log
	rm report.aux
	rm report.out
