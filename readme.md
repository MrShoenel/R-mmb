|Version|Status|Coverage|
|:-|:-|:-|
|[![CRAN version](https://www.r-pkg.org/badges/version/mmb)](https://www.r-pkg.org/badges/version/mmb)|[![Travis build status](https://travis-ci.org/MrShoenel/R-mmb.svg?branch=master)](https://travis-ci.org/MrShoenel/R-mmb)|[![Codecov test coverage](https://codecov.io/gh/MrShoenel/R-mmb/branch/master/graph/badge.svg)](https://codecov.io/gh/MrShoenel/R-mmb?branch=master)|


# R-package: `mmb`
This is the repository of the R-package __`mmb`__ for _arbitrary dependency mixed multivariate bayesian models for inferencing, regression and neighborhood search using joint probabilities and Kernel Density Estimation._

## Installation
The `master`-branch always contains a 100% working release version. As long as there is no release on CRAN, you can install this package using the `devtools`:

<pre>
&gt; devtools::install_github("https://github.com/MrShoenel/R-mmb", subdir = "pkg/mmb")
</pre>

## Building the package
That's easy! Just run:

<pre>
&gt; Rscript build.R all
</pre>

This builds everything, generates manuals (PDF and HTML) and packages the archive.
