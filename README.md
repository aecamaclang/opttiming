<<<<<<< HEAD
# opttiming: Predicting the optimal time to spend learning 

This R package implements the anaytical solution for predicting the optimal amount of time to spend learning before designating a species' habitat for protection, as described in:  
=======
# Optimal time to spend learning before designating protected habitat

This package implements the anaytical solution for predicting the optimal amount of time to spend learning before designating a species' habitat for protection, as described in:  
>>>>>>> 535579b8f6e77ddf12449435a2bdffa1ef2b41aa
  
Camaclang, AE, Chades, I, Martin, TG, and Possingham, HP. (2021)"Predicting the optimal amount of time to spend learning before designating protected habitat for threatened species."

## Installation

<<<<<<< HEAD
You can install the R package "opttiming" by running the following in R Studio:
=======
You can install the R package opttiming from [CRAN](https://CRAN.R-project.org) with:
>>>>>>> 535579b8f6e77ddf12449435a2bdffa1ef2b41aa

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("aecamaclang/opttiming")
```
Alternatively, download/clone the github repository from https://github.com/aecamaclang/opttiming.git, use R Studio to open the RProj file in the downloaded directory, then either: a) use Build -> Install and Restart; or b) use the console to run

``` r
devtools::install()
```

## Details
Includes groups of functions for  
1. predicting the optimal time to spend learning for different learning curves: optlin(), opthyp(), optsig()  
2. determining the accuracy of identification at a given time for different learning curve: linear(), hyperb(), sigmoid()  
3. calculating the proportion that is correctly identified at a given time for different learning curves: proplin(), prophyp(), propsig()


## Examples

To predict the optimal amount of time to spend learning under a hyperbolic learning curve with curve parameter values of b and m, given a habitat loss rate of x = 0.02 and a false positive threshold of B = 0.2:

``` r
library(opttiming)
result_optim <- opthyp(B = 0.2, b = 10, m = 5, x = 0.02)
```
To estimate the accuracy of identification after 5 years (x = 5) for a linear learning curves with slope m:

```r
accuracy <- linear(m = 9/50, yint = 1, x = 5)
```

To estimate the proportion of habitat correctly identified:

```r
prop <- propsig()
```

## Manuscript results and figures

<<<<<<< HEAD
R scripts used to generate the results and figures presented in the manuscript can be found in the analysis subfolder included in this repository.
=======
R scripts used to generate the results and figures presented in the manuscript can be found in the analysis subfolder.
>>>>>>> 535579b8f6e77ddf12449435a2bdffa1ef2b41aa

