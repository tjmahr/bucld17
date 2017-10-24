
# Use this as the command for running R scripts
rscriptv := Rscript --vanilla

database-results = \
	data-raw/test-scores.csv \
	data-raw/looks.csv.gz \
	data-raw/trials.csv \
	data-raw/blocks.csv

screening-results = \
	data/scores.csv \
	data/screened.csv.gz

02-plot-data.md: 02-plot-data.Rmd $(screening-results)
	$(rscriptv) -e 'rmarkdown::render("$<")'

01-data-screening.md $(screening-results): 01-data-screening.Rmd $(database-results)
	$(rscriptv) -e 'rmarkdown::render("$<")'

$(database-results): get-data.R
	$(rscriptv) $<

clean:
	rm *.html

## variables   : Print variables.
# .PHONY : variables help
# variables :
# 	@echo database-results: $(database-results)
# 	@echo screening-results: $(screening-results)

# help : Makefile
# 	@sed -n 's/^##//p' $<

.PHONY: clean
