
all: report

VPATH=src:src/analysis:src/munge

report: report.html
report.html: analysis.R munge.R
	Rscript -e 'rmarkdown::render("report.Rmd", output_format = "html_document")'


