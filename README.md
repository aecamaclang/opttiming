# opttiming: Predicting the optimal time to spend learning 

This R package implements the anaytical solution for predicting the optimal amount of time to spend learning before designating a species' habitat for protection, as described in:  
  
Camaclang, AE, Chades, I, Martin, TG, and Possingham, HP. (2021) Predicting the optimal amount of time to spend learning before designating protected habitat for threatened species. Methods in Ecology and Evolution: _in press._

### Installation

You can install the R package "opttiming" by running the following in R Studio:

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("aecamaclang/opttiming")
```

Alternatively, download/clone the github repository from https://github.com/aecamaclang/opttiming.git, use R Studio to open the RProj file in the downloaded directory, then either: a) use Build -> Install and Restart; or b) use the console to run

``` r
devtools::install()
```

### Details
Includes functions for  
1. predicting the optimal time to spend learning for different learning curves: optlin(), opthyp(), optsig()  
2. determining the accuracy of identification at a given time for different learning curve: linear(), hyperb(), sigmoid()  
3. calculating the proportion that is correctly identified at a given time for different learning curves: proplin(), prophyp(), propsig()


### Examples

To predict the optimal amount of time to spend learning under a hyperbolic learning curve with curve parameter values of b and m, given a habitat loss rate of x = 0.02 and a false positive threshold of B = 0.2:

``` r
library(opttiming)
ot_h1 <- opthyp(B = 0.2, b = 10, m = 5, x = 0.02)
```
To estimate the accuracy of identification under the same hyperbolic learning curve, if the optimal amount of time is spent learning:

```r
acc <- hyperb(b = 10, m = 5, x = ot_h1)
```

To estimate the proportion of habitat correctly identified at the optimal time:

```r
prop <- prophyp(B = 0.2, b = 10, m = 5, a = acc, t = ot_h1, x = 0.02)
```

# Manuscript results and figures

R scripts used to generate the results and figures presented in the manuscript can be found in the analysis subfolder of this repository.
