"plotwidthEstimation" <-
function(x,peakWidth) {
  x=as.vector(x)
  lmindex=1:length(x)
  peakIndex=peakWidth$peakIndex
  LR = NULL
#  plot(x,type='l')                       
  points(peakIndex,x[peakIndex])
  for (i in 1:length(peakIndex)){
    peakWidth.i=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
    LR=c(LR,peakWidth.i[c(1,length(peakWidth.i))])  
  }
  points(LR,x[LR])
  
  return ("successful")
}