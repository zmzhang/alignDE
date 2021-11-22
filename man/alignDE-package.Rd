\name{alignDE-package}
\alias{alignDE-package}
\alias{alignDE}
\docType{alignDE}
\title{
Peak alignment using wavelet pattern matching and differential evolution
}
\description{
Retention time shifts badly impair qualitative or quantitative results of chemometric analyses when entire chromatographic data are used. Hence, chromatograms should be aligned to perform further analysis. Being inspired and motivated by this purpose, a practical and handy peak alignment method (alignDE) is proposed, implemented in this research for first-order chromatograms, which basically consists of five steps: (1) chromatograms lengths equalization using linear interpolation; (2) accurate peak pattern matching by continuous wavelet transform (CWT) with the Mexican Hat and Haar wavelets as its mother wavelets; (3) flexible baseline fitting utilizing penalized least squares; (4) peak clustering when gap of two peaks is larger than a certain threshold; (5) peak alignment using differential evolution (DE) to maximize linear correlation coefficient between reference signal and signal to be aligned. This method is demonstrated with simulated chromatograms, chromatograms of HPLC-DAD at 202nm. It is implemented in R language and available as open source software to a broad range of chromatograph users (http://code.google.com/p/alignde).
}
\details{
\tabular{ll}{
Package: \tab alignDE\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2009-10-09\cr
License: \tab GPL (>= 2)\cr
}
}
\author{ yizeng liang<yizeng_liang@263.net>, zhimin zhang <zhangzhimin.csu@gmail.com>, chen shan <chenshan.csu@gmail.com>}                                                                                                                     
\keyword{ package }
