\name{alignDE-function}
\alias{alignDE-function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Peak alignment using wavelet pattern matching and differential evolution}
\description{
  Retention time shifts badly impair qualitative or quantitative results of chemometric analyses when entire chromatographic data are used. Hence, chromatograms should be aligned to perform further analysis. Being inspired and motivated by this purpose, a practical and handy peak alignment method (alignDE) is proposed, implemented in this research for first-order chromatograms, which basically consists of five steps: (1) chromatograms lengths equalization using linear interpolation; (2) accurate peak pattern matching by continuous wavelet transform (CWT) with the Mexican Hat and Haar wavelets as its mother wavelets; (3) flexible baseline fitting utilizing penalized least squares; (4) peak clustering when gap of two peaks is larger than a certain threshold; (5) peak alignment using differential evolution (DE) to maximize linear correlation coefficient between reference signal and signal to be aligned. This method is demonstrated with simulated chromatograms, chromatograms of HPLC-DAD at 202nm. It is implemented in R language and available as open source software to a broad range of chromatograph users (http://code.google.com/p/alignde).
}   
\usage{
alignDE(x,peakWidth,target,slack=70,n=4,control=list(termax = 100, CR = 0.7, refresh = 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{chromatogram to be aligned }
  \item{peakWidth}{ returned by \code{\link{widthEstimationCWT}} }
  \item{target}{reference chromatograms }
  \item{slack}{shifts the peak position can be adjusted }
  \item{n}{Gap for peak clustering}
  \item{control}{List datatype for DEoptim}
}

\value{
  the aligned chromatograms
}

\author{Yizeng Liang ,Zhang Zhimin, Chen Shan}


\examples{
require(alignDE) 
data(simulate)
main="Simulated chromatograms"
xlab ="Sample intervals"
ylab="mAU"
plot(p1,type='l',main=main,xlab=xlab,ylab=ylab)
lines(p2,lty=3)
legend(780, 120, c("R", "C"),
       text.col = "black", lty = c(1, 3))
#lines(warped,col='blue')


scales <-seq(1, 56, 1)
wCoefs <- cwt(p1, scales=scales, wavelet='mexh')
image(1:nrow(wCoefs), scales,apply(t(wCoefs),1,rev), col=terrain.colors(256), axes=FALSE, xlab='index', ylab='CWT coefficient scale', main='CWT coefficients')
box()
localMax <- getLocalMaximumCWT(wCoefs)
ridgeList <- getRidge(localMax, gapTh=3, skip=2)
plotRidgeList(ridgeList)
majorPeakInfo = identifyMajorPeaks(p1, ridgeList, wCoefs, SNR.Th=3,ridgeLength=5)
peakWidth1=widthEstimationCWT(p1,majorPeakInfo)
backgr = baselineCorrectionCWT(p1,peakWidth1,lambda=100,differences=1)
p1c=p1-backgr 
#plot(p1c,type='l')
#points(majorPeakInfo$peakIndex,p1c[majorPeakInfo$peakIndex])                                         

scales <-seq(1, 56, 1)
wCoefs <- cwt(p2, scales=scales, wavelet='mexh')
image(1:nrow(wCoefs), scales,apply(t(wCoefs),1,rev), col=terrain.colors(256), axes=FALSE, xlab='index', ylab='CWT coefficient scale', main='CWT coefficients')
box()
localMax <- getLocalMaximumCWT(wCoefs)
ridgeList <- getRidge(localMax, gapTh=3, skip=2)
plotRidgeList(ridgeList)
majorPeakInfo = identifyMajorPeaks(p2, ridgeList, wCoefs, SNR.Th=3,ridgeLength=5)
peakWidth2=widthEstimationCWT(p2,majorPeakInfo)
backgr = baselineCorrectionCWT(p2,peakWidth2,lambda=100,differences=1)
p2c=p2-backgr 
#plot(p2c,type='l')
#points(majorPeakInfo$peakIndex,p2c[majorPeakInfo$peakIndex])       

plot(p1c,type='l',main='Peak clustered and baseline corrected chromatograms',xlab=xlab,ylab=ylab)
lines(p2c,lty=3)
peakWidth2=peakClustering(peakWidth2,n=5)
plotwidthEstimation(p2c,peakWidth2)
legend(780, 120, c("R", "C"),
       text.col = "black", lty = c(1, 3))

########################################################

cp=list(NP=50, itermax =150, refresh = 10)
result=alignDE(p2c,peakWidth2,p1c,100,control=cp)

#######################################################

plot(p1c,type='l',main='Aligned and baseline corrected chromatograms',xlab=xlab,ylab=ylab)
#lines(p2c,col='red')
lines(result,lty=3)
legend(780, 120, c("R", "A"),
       text.col = "black", lty = c(1, 3))
}
\keyword{alignDE}
