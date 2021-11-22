alignDE<-function(x,peakWidth,target,slack=70,n=4,control=list(termax = 100, CR = 0.7, refresh = 1)){
    
#  control=list(NP=50, itermax =100,CR = 0.7, refresh = 10) 
#  slack=100
#  x=p2c
#  target=p1c
#  peakWidth=peakWidth21
             
  nPeakNum=length(peakWidth$peakIndex)
  result=list()
  shift=c()
  for(i in 1:floor(nPeakNum/n))
  {
    lower=rep(-slack,n)
    upper=rep(slack,n) 
    peakWidths=list() 
    peakWidths$peakIndex=peakWidth$peakIndex[(1+((i-1)*n)):(n+(i-1)*n)]
    peakWidths$peakIndexLower=peakWidth$peakIndexLower[(1+((i-1)*n)):(n+(i-1)*n)]
    peakWidths$peakIndexUpper=peakWidth$peakIndexUpper[(1+((i-1)*n)):(n+(i-1)*n)]
    
    if(i==1){
      startindex=1    
    }else{
      startindex=peakWidth$peakIndexUpper[(1+((i-1)*n))-1]+shift[length(shift)]+1
    }
    endindex=length(x)
    result[[i]]=DEoptim(objfun,lower,upper,control,peakWidth=peakWidths,x=x,target=target,startindex=startindex,endindex=endindex)
    shift=c(shift,result[[i]]$optim$bestmem)
  }
  

  nLeftPeaks=nPeakNum%%n
  i=i+1
  if(nLeftPeaks>=1){
    lower=rep(-slack,nLeftPeaks)
    upper=rep(slack,nLeftPeaks) 
    peakWidths=list() 
    peakWidths$peakIndex=peakWidth$peakIndex[(nPeakNum-nLeftPeaks+1):nPeakNum]
    peakWidths$peakIndexLower=peakWidth$peakIndexLower[(nPeakNum-nLeftPeaks+1):nPeakNum]
    peakWidths$peakIndexUpper=peakWidth$peakIndexUpper[(nPeakNum-nLeftPeaks+1):nPeakNum]
    if(i==1){
      startindex=1    
    }else{
      startindex=peakWidth$peakIndexUpper[(1+((i-1)*n))-1]+shift[length(shift)]+1
    }
    endindex=length(x)
    result[[i+1]]=DEoptim(objfun,lower,upper,control,peakWidth=peakWidths,x=x,target=target,startindex=startindex,endindex=endindex)
    shift=c(shift,result[[i+1]]$optim$bestmem)      
  }     
#######################################################################################

  x=as.vector(x)
  shift=ceiling(shift)
  peakIndex=peakWidth$peakIndex
  peak=peakWidth$peakIndexLower[1]:peakWidth$peakIndexUpper[1]
  xs=c()
    baseline_warp=approx(x[1:(peak[1]-1)],n=peak[1]+shift[1]-1)
  xs=c(xs,baseline_warp$y)
  for(i in 1:(nPeakNum-1)){
    peakCurrent=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
    peakNext=peakWidth$peakIndexLower[i+1]:peakWidth$peakIndexUpper[i+1]
    baselineStart=peakCurrent[length(peakCurrent)]+1
    baselineEnd=peakNext[1]-1

    xs=c(xs,x[peakCurrent])
    baseline_warp=approx(x[baselineStart:baselineEnd],n=((baselineEnd+shift[i+1])-(baselineStart+shift[i])+1) )
    xs=c(xs,baseline_warp$y)
  }
  peak=peakWidth$peakIndexLower[nPeakNum]:peakWidth$peakIndexUpper[nPeakNum]
  xs=c(xs,x[peak])
  baseline_warp=approx(x[(peak[length(peak)]+1):length(x)],n=(length(x)-(peak[length(peak)]+shift[nPeakNum]+1)+1))
  xs=c(xs,baseline_warp$y)
###############################################################################################

  return(xs)
}