
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

model-data = \
	data/model.csv.gz

all: 04-maybe-bias.md 03-models.md 02-plot-data.md 01-data-screening.md

04-maybe-bias.md: 04-maybe-bias.Rmd data/bias.csv.gz plotting-helpers.R
	$(rscriptv) -e 'rmarkdown::render("$<")'

03-models.md: 03-models.Rmd $(model-data) plotting-helpers.R
	$(rscriptv) -e 'rmarkdown::render("$<")'

# Main plot of growth curves also saves files used for growth curve models
02-plot-data.md $(model-data) data/bias.csv.gz: 02-plot-data.Rmd $(screening-results) plotting-helpers.R
	$(rscriptv) -e 'rmarkdown::render("$<")'

01-data-screening.md $(screening-results): 01-data-screening.Rmd $(database-results)
	$(rscriptv) -e 'rmarkdown::render("$<")'

00-get-data.md: 00-get-data.Rmd
	$(rscriptv) -e 'rmarkdown::render("$<")'

$(database-results): 00-get-data.md


preview: 03-models.md
	open 03-models.html

check: $()
	$(rscriptv) -e 'list.files(pattern = "unnamed", recursive = TRUE, full.names = TRUE)'

clean:
	rm -f *.html;
	rm -f 0*.md;
	rm -f assets/figure/*.png;

.PHONY: clean all check preview
