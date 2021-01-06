 -   [Gender Pay Gap](#gender-pay-gap)
    -   [Files](#files)
    -   [Data Source](#data-source)
    -   [How to Reproduce](#how-to-reproduce)
# Gender Pay Gap

## Files

### *`GPG.rmd`*
Rmarkdown file that would knit everything together into pdf.
* *`my_header.tex`* - **won't knit without it** as it part of `YAML` header
    
### *`GPG.pdf`*
Output pdf document (code is omitted for clarity)

There are 3 main and 1 helper R files:

### *`data.clean.r`* 
initial R script collecting, cleaning & transforming the data
* saves a few `rds` files to be used by other scripts and by `GPG.Rmd`
* ### *`gender.prob.r`*
   a helper script to grab gender proportions by name
   
<blockquote class="twitter-tweet" lang="en">

<p>

Data cleaning code cannot be clean. It's a sort of sin eater.

</p>

— Stat Fact (@StatFact)
<a href="https://twitter.com/StatFact/status/492753200190341120">July
25, 2014</a>

</blockquote>

### *`data.plot.R`* 
secondary R script containing EDA and visualization, stand along from the other scripts

### *`data.modeling.r`*
main R script that pulls the data, runs models and outputs results.
*train_models.RDS* gets created to avoid lenthly re-run for Rmd file 

## Data Source
[UK Goverment Equalities office](https://www.gov.uk/government/organisations/government-equalities-office)

## How to Reproduce
1. Make sure the R version is up to date

``` r
  R.version.string
```

2. install libraries from CRAN:

``` r
install.packages(c("tidyverse", "data.table", "rvest", "caret", "corrplot", "ggcorrplot", "e1071", "gridExtra", "doParallel", "pls"
"elasticnet", "nnet", "earth", "kernlab", "party", "RWeka", "ipred", "randomForest", "Cubist"))
  #RWeka requires Java
```
3. check `tinytex` installation for `rmd`
``` r
  if(!tinytex:::is_tinytex()) tinytex::install_tinytex()
```
4.  Run `data.clean.r` followed by `data.model.r', then `knit` 

**The entire repository can be cloned (or use "Download ZIP" option under big green "CODE" button).**
