# Spoken word recognition of children with cochlear implants

Research compendium for our presentation at the 42nd annual Boston University
Conference on Language Development.

## Repository contents

### Analysis notebooks

`00-get-data.Rmd` ([output](./00-get-data.md)) downloads the raw data from our
lab's internal database. It cannot be reproduced by people outside our lab.
Therefore, I try to keep the script brief and have it save the needed data into
the folder `data-raw`. Later processing steps can be reproduced in the 
following notebooks.

`01-data-screening.Rmd` ([output](./01-data-screening.md)) screens the raw 
eyetracking data, removing trials and participants with excessive missing data.

`02-plot-data.Rmd` ([output](./02-plot-data.md)) includes various plots of the
eyetracking data from the experiment. We had equivocated about how we would go
about revealing the plots through the presentation so a lot of options are
covered. This file also computes descriptives stats and prepares by-group plots
of test scores.

`03-models.Rmd` ([output](./03-models.md)) runs growth curve analyses
for the data.

`04-maybe-bias.Rmd` ([output](./04-maybe-bias.md)) runs an additional
exploratory growth curve model where trials are separated based on which image
the child fixates on at noun onset (the trial's _bias_).

### Directories

`data-raw/`: Raw data downloaded from the database.

`data/`: Screened data ready for plotting or modeling.

Large csv files are saved as compressed `.csv.gz` files. The readr package,
specifically the function `readr::read_csv()`, can automatically uncompress
these files.

`assets/`: The image files and model caches for the notebooks are stored here.

### Miscellany

`mahr-edwards-bucld42-ci-word-recognition.pdf` contains the Power Point slides
as a pdf file.

`plotting-helpers.R` stores helpers functions that are recycled in the different 
scripts.

`Makefile` is a makefile to automate running and rendering the notebook files.

The code's `LICENSE` is the GPL-3, but I don't think that should matter. The
data is copyrighted and belongs to the University of Wisconsin–Madison, 
I think. (We collected data at two different university labs.)

## Additional information

### Reproducibility tips

Don't hesitate to ask. These things can be finicky on other people's computers.

This code should install most of the needed packages. Otherwise, the 
session-info at the bottom of each script has the version and provenance of 
all R packages used.

```r
install.packages(c("knitr", "rmarkdown", "tidyverse", "lme4", 
                 "yaml", "polypoly", "devtools", "rprojroot",
                 "viridis"))
devtools::install_github("tjmahr/littlelisteners")
```

The easiest way to reproduce the analysis is to clone the repository from GitHub
into an RStudio project. In RStudio: File > New Project \> Version Control > Git
\> paste in the URL of this repository. You can "Knit" the individual Rmd files
to run and compile the notebooks, or you can use the Build tab to run the 
Makefile which updates any files that need to be updated.

### Abstract from conference handbook

> Spoken word recognition of children with cochlear implants
>
> _Tristan Mahr (University of Wisconsin–Madison)_ \
> _Jan Edwards (University of Maryland)_
>
> Children with cochlear implants (CIs) perform more poorly
than normal hearing (NH) children on virtually every aspect of
speech and language. While these deficits have been attributed in
large part to the impoverished signal, children with CIs may also
have different processing strategies because of their consistent
experience of listening to an impoverished signal. We used the
mispronunciation paradigm to examine processing differences
in word recognition. 26 children with CIs (age: 31–66 months)
were compared to NH children matched for age and sex.
Children with CIs recognized one-feature mispronunciations as
reliably as their NH peers, but it took them longer to look away
from the familiar image. They were also slower and less reliable
at recognizing highly familiar words, even in a two-image LWL
paradigm. These results suggest that the impoverished signal
of the CI may result in more uncertainty and longer lexical
decision times, especially for ambiguous speech cues.

### APA-format citation

This bit is mostly to help me copy-and-paste it.

Mahr, T., & Edwards, J. R. (2017, November). **Spoken word recognition of
children with cochlear implants**. Presentation at the 42nd annual Boston
University Conference on Language Development, Boston, MA.
