\name{IBT}
\alias{IBT}
\docType{IBT}
\title{Real chromatograms}   
\description{
Real chromatograms are available from Refs. [11,35]. The chromatograms were analyses of extracts from fungal cultivated on Yeast Extract Sucrose agar (P. cyclopium, denoted by IBT 11415 and 15670) using ultrasonic extraction and HPLC, which collected at the Department of Biotechnology (IBT), Technical University of Denmark. IBT11415 and IBT15670 were downloaded as an MATLABTM 6 MAT-file from website mentioned in Ref. [11]
}
\usage{data(IBT)}
\format{
  two vector
  \describe{
    \item{\code{IBT11415 }}{vector 1 as reference}
    \item{\code{IBT15670 }}{vector 2 to be aligned}
  }
}
\details{
The eal chromatograms can be used for aligning.
}
\examples{ 
require(alignDE)
data(IBT)
main="Chromatograms of IBT"
xlab ="Sample intervals"
ylab="mAU"
p1=IBT11415
p2=IBT15670
plot(p1,type='l',main=main,xlab=xlab,ylab=ylab)
lines(p2,lty=3)
legend(2400, 750, c("IBT11415", "IBT15670"),
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
x11()
plot(p1c,type='l',main='Peak clustered and baseline corrected chromatograms',xlab=xlab,ylab=ylab)
lines(p2c,lty=3)
points(majorPeakInfo$peakIndex,p2c[majorPeakInfo$peakIndex])
peakWidth2=peakClustering(peakWidth2,n=5)
#plotwidthEstimation(p2c,peakWidth2)
legend(2400, 750, c("IBT11415", "IBT15670"),
       text.col = "black", lty = c(1, 3))

########################################################

cp=list(NP=200, itermax = 150, refresh = 10)
result=alignDE(p2c,peakWidth2,p1c,slack=100,control=cp)

#######################################################
similarity(result,p1c)

plot(p1c,type='l',main='Aligned and baseline corrected chromatograms',xlab=xlab,ylab=ylab)
#lines(p2c,col='red')
lines(result,lty=3)
legend(2400, 750, c("IBT11415", "IBT15670"),
       text.col = "black", lty = c(1, 3))
}
\keyword{datasets}
