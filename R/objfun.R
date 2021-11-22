objfun = function(shift,peakWidth=list(),x=1:100,target=1:100,startindex=1,endindex=1){


  x=as.vector(x)
  shift=ceiling(shift)
  r=0
  xs=c()
  nPeakNum=length(peakWidth$peakIndex)
  peakIndex=peakWidth$peakIndex
  if(nPeakNum>1){
    for(i in 1:(nPeakNum-1)){
      peakCurrent=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
      baselineStart=peakCurrent[length(peakCurrent)]+1
      peakNext=peakWidth$peakIndexLower[i+1]:peakWidth$peakIndexUpper[i+1]
      baselineEnd=peakNext[1]-1
      if(peakCurrent[i]+shift[i]-startindex<=1){
        r=Inf
      }
      if((baselineEnd+shift[i+1])-(baselineStart+shift[i])<=1){
        r=Inf
      }else{
        xs=c(xs,x[peakCurrent])
        baseline_warp=approx(x[baselineStart:baselineEnd],n=((baselineEnd+shift[i+1])-(baselineStart+shift[i])+1) )
        xs=c(xs,baseline_warp$y)
      }
    }

  }
  
  if(peakWidth$peakIndexLower[nPeakNum]+shift[nPeakNum]-startindex<=1){
    r=Inf
  }
  peak=(peakWidth$peakIndexLower[nPeakNum]:peakWidth$peakIndexUpper[nPeakNum])
  if(endindex-(peak[length(peak)]+shift[nPeakNum])<=1)
  {
    r=Inf
  }else{
    xs=c(xs,x[peak])
  }

  if(r==0){
    targets=target[(peakWidth$peakIndexLower[1]+shift[1]):(peakWidth$peakIndexUpper[nPeakNum]+shift[nPeakNum])]
    sim=-similarity(xs,targets)
    return(sim)
  }else{
    return(r)
  }

}