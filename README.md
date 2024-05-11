# mghlmm.laplace

### A Supplementary R Package to to "Marginal Inference for Hierarchical Generalized Linear Mixed Models with Patterned Covariance Matrices Using the Laplace Approximation"

##### Jay M. Ver Hoef<sup>1</sup>, Eryn Blagg<sup>2</sup>, Michael Dumelle<sup>3</sup>, Philip Dixon<sup>2</sup>, Dale L. Zimmerman<sup>4</sup>, Paul Conn<sup>1</sup>

##### <sup>1</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA
##### <sup>2</sup>Iowa State University, Department of Statistics, Ames, IA, USA
##### <sup>3</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>4</sup>University of Iowa, Department of Statistics and Actuarial Science, Iowa City, IA, USA

##### For correspondence, please email Jay M. Ver Hoef at jay.verhoef@noaa.gov

### Abstract

We develop methods for a wide and flexible class of models by taking a fully parametric approach to hierarchical generalized linear mixed models (HGLMM) with complex covariance dependence. The Laplace approximation is used to marginally estimate covariance parameters while integrating out all fixed and latent random effects, and Newton-Raphson optimization leads to predictions for the latent random effects.  We develop methodology for complete marginal inference, from estimating covariance parameters and fixed effects to making predictions for unobserved data, for any patterned covariance matrix in the HGLMM framework. The marginal likelihood is developed for six distributions that are often used for binary, count, and positive continuous data, and HGLMMs are easily extended to other distributions.  The methods are illustrated with simulations from stochastic processes with known parameters, and their efficacy shown in terms of bias and interval coverage. Examples with binary and proportional data on election results, count data for marine mammals, and positive-continuous data on heavy metal concentration in the environment are used to illustrate all six distributions with a variety of patterned covariance structures that include spatial models (geostatistical and areal models), time series models (first-order autoregressive models), and mixtures with typical random intercepts based on grouping.

### Package Overview

This supplementary R package contains all files files required to reproduce the results in the manuscript. Next we discuss how to use the package to access these files.

### Installation

To install the supplementary R package, run
```r
install.packages("remotes") # if you don't already have remotes installed
remotes::install_github("USEPA/mhglmm.laplace", ref = "main", dependencies = TRUE)
```

### Scripts

The files required to reproduce the results in the manuscript are available at the file path found by running
```r
system.file("scripts", package = "mhglmm.laplace")
```

### Output

The files required to view the results in the manuscript are available at the file path found by running
```r
system.file("output", package = "mhglmm.laplace")
```

## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

The views expressed in this manuscript are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency or the U.S. National Oceanic and Atmospheric Administration. Any mention of trade names, products, or services does not imply an endorsement by the U.S. government, the U.S. Environmental Protection Agency, or the U.S. National Oceanic and Atmospheric Administration. The U.S. Environmental Protection Agency and the U.S. National Oceanic and Atmospheric Administration do not endorse any commercial products, services, or enterprises.

### License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).
