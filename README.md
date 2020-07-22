localFDA
========

<!-- badges: start -->

[![License](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

Overview
--------

Software companion for the paper “Localization processes for functional
data analysis” by Elías, Antonio, Jiménez, Raúl, and Yukich, Joe, 2020.
It provides the code for computing localization processes and
localization distances.

Installation
------------

``` r
#install the package
devtools::install_github("aefdz/localFDA")
```

``` r
#load the package
library(localFDA)
```

Test usage
----------

Load the example data and plot it.

``` r
data(exampleData)
n <- ncol(X)
p <- nrow(X)
t <- as.numeric(rownames(X))

#plot the data set
df_functions <- data.frame(ids = rep(colnames(X), each = p),
                           y = c(X),
                           x = rep(t, n)
                           )

functions_plot <- ggplot(df_functions) + 
  geom_line(aes(x = x, y = y, group = ids, color = ids), color = "black", alpha = 0.25) + xlab("t") + theme(legend.position = "none")


functions_plot
```

<img class="center" src="README_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display:block; margin:auto;" />

### Compute *kth empirical localization processes*

Empirical version of Equation (1) of the paper. For one focal,

``` r
focal <- "1"

localizarionProcesses_focal <- localizationProcesses(X, focal)$lc
```

Plot localization processes of order 1, 50, 100 and 200:

``` r
df_lc <- data.frame(k = rep(colnames(localizarionProcesses_focal), each = p),
                           y = c(localizarionProcesses_focal),
                           x = rep(t, n-1)
                           )

lc_plots <- list()
ks <- c(1, 50, 100, 200)

for(i in 1:4){
  lc_plots[[i]] <- functions_plot + 
                   geom_line(data = filter(df_lc, k == paste0("k=", ks[i])), 
                             aes(x = x, y = y, group = k), 
                             color = "blue", size = 1) +
                   geom_line(data = filter(df_functions, ids == focal), 
                             aes(x = x, y = y, group = ids), 
                             color = "red", linetype = "dashed", size = 1)+
                   ggtitle(paste("k = ", ks[i]))
}

wrap_plots(lc_plots)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display:block; margin:auto;" />

### Compute *kth empirical localization distances*

Equation (18) of the paper. For one focal,

``` r
localizationDistances_focal <- localizationDistances(X, focal)

head(localizationDistances_focal)
```

    ##          k=1          k=2          k=3          k=4          k=5          k=6 
    ## 0.0005082926 0.0011346495 0.0017636690 0.0023955745 0.0030095117 0.0035089220

Plot the localization distances:

``` r
df_ld <- data.frame(k = names(localizationDistances_focal),
                           y = localizationDistances_focal,
                           x = 1:c(n-1)
                           )


ldistances_plot <- ggplot(df_ld, aes(x = x, y = y)) + 
                   geom_point() + 
                   ggtitle("Localization distances for one focal") + 
                   xlab("kth") + ylab("L")

ldistances_plot
```

<img src="README_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display:block; margin:auto;" />

### Sample *μ* and *σ*

``` r
localizationStatistics_full <- localizationStatistics(X, robustify = TRUE)

#See the mean and sd estimations for k = 1, 100, 200, 400, 600

localizationStatistics_full$trim_mean[c(1, 100, 200, 400, 600)]
```

    ##         k=1       k=100       k=200       k=400       k=600 
    ## 0.001083517 0.098465426 0.184940365 0.350528860 0.526580274

``` r
localizationStatistics_full$trim_sd[c(1, 100, 200, 400, 600)]
```

    ##          k=1        k=100        k=200        k=400        k=600 
    ## 0.0005326429 0.0329170846 0.0490732397 0.0686018224 0.0806314699

References
----------

Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization
processes for functional data analysis (submitted)
