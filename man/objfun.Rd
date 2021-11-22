\name{objfun}
\alias{objfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{object funtion for DEoptim}
\description{
  object funtion for DEoptim
}
\usage{
objfun(shift,peakWidth=list(),x=1:100,target=1:100,startindex=1,endindex=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shift}{shifts of peak postions}
  \item{peakWidth}{result of peak width estimation, see see \code{\link{widthEstimationCWT}}}
  \item{x}{chromatogram to be aligned}
  \item{target}{reference chromatogram}
  \item{startindex}{start index}
  \item{endindex}{end index}
}

\value{
The return is similarity of x and target
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
\keyword{ methods }
