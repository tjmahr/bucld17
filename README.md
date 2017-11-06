# Spoken word recognition of children with cochlear implants

Research compendium for our presentation at the 42nd annual Boston University Conference on Language Development.

## Repository contents

`01-data-screening.Rmd` ([output](./01-data-screening.md)) screens the raw 
eyetracking data, removing trials and participants with excessive missing data.

`02-plot-data.Rmd` ([output](./02-plot-data.md)) includes various plots of the eyetracking 
data from the experiment. We had equivocated about how we would go about revealing the plots through 
the presentation so a lot of options are covered. This file also computes descriptives 
stats and prepares by-group plots of test scores.

`03-models.Rmd` ([output](./03-models.md)) runs growth curve analyses
for the data.

`04-maybe-bias.Rmd` ([output](./04-maybe-bias.md)) runs an additional exploratory 
growth curve model where trials are separated based on which image the child 
fixates on at noun onset (the trial's _bias_). 

`get-data.R` downloads the raw data from our lab's internal database. It cannot be 
reproduced by people outside our lab. Therefore, I try to keep the script brief and 
have it save the needed data into the folder `data-raw`. Later processing steps can 
be reproduced.

## Additional information

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

Mahr, T., & Edwards, J. R. (2017, November). **Spoken word recognition of children with cochlear implants**. Presentation at the 42nd annual Boston University Conference on Language Development, Boston, MA.
