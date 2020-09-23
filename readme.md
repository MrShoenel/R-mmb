|Version|Status|Coverage|
|:-|:-|:-|
|[![CRAN version](https://www.r-pkg.org/badges/version/mmb?r=1)](https://cran.r-project.org/package=mmb)|[![Travis build status](https://travis-ci.org/MrShoenel/R-mmb.svg?branch=master)](https://travis-ci.org/MrShoenel/R-mmb)|[![Codecov test coverage](https://codecov.io/gh/MrShoenel/R-mmb/branch/master/graph/badge.svg)](https://codecov.io/gh/MrShoenel/R-mmb?branch=master)|


# R-package: `mmb` [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4046001.svg)](https://doi.org/10.5281/zenodo.4046001)
This is the repository of the R-package __`mmb`__ for _arbitrary dependency mixed multivariate bayesian models for inferencing, regression and neighborhood search using joint probabilities and Kernel Density Estimation._

__Install using `CRAN`__:
<pre>
install.packages("mmb")
</pre>

## Installation of latest release
The `master`-branch always contains a release-version. Sometimes there may be a short delay between the master-version and what is on CRAN. In this case, you can still install the latest release using the package `devtools` (mind the _`subdir`_):

<pre>
R&gt; devtools::install_github("https://github.com/MrShoenel/R-mmb", subdir = "pkg/mmb", ref = "master")
</pre>

## Documentation and function reference

The latest documentation can be found at <https://mrshoenel.github.io/R-mmb/>.

## Citing
Please use the following BibTeX to cite the package __`mmb`__:

<pre>
@article{honel2020rmmb,
	title={mmb: An R Package for Mixed Multivariate Arbitrary Dependency Bayesian Models},
	DOI={10.5281/zenodo.4046002},
	url={https://doi.org/10.5281/zenodo.4046002},
	note={Install this package from CRAN: install.packages("mmb")},
	publisher={Zenodo},
	author={Sebastian Hönel},
	year={2020},
	month={Sep}
}
</pre>

## Building the package
That's easy! Just run:

<pre>
&gt; Rscript build.R all
</pre>

This builds everything, generates manuals (PDF and HTML) and packages the archive.
