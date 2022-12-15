# RegenerationDFM

This repository contains the data, code for the analysis and manuscript.

## How to contribute

Please if you want to contribute, clone the repository, do you changes locally and request a merge.

## How to compile the manuscript?

-   Run first code/dataPreparation.R
-   Then run codefigures.R
-   Then compile manuscript/manuscriptRW.Rmd

```{r}
rmarkdown::render('manuscript/manuscriptRW.Rmd', ' bookdown::pdf_document2')
```
