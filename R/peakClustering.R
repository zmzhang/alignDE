peakClustering=function(peakWidth, n=10)
{

  nPeakNum=length(peakWidth$peakIndex)
  nGap=rep(0,(nPeakNum-1))
  for(i in 1:(nPeakNum-1)){
    peakCurrent=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
    peakNext=peakWidth$peakIndexLower[i+1]:peakWidth$peakIndexUpper[i+1]
    lengthCurrent=length(peakCurrent) 
    lengthNext=length(peakNext) 
    nGap[i]=(peakNext[1]-peakCurrent[lengthCurrent])
    if((peakNext[1]-peakCurrent[lengthCurrent])<=n){
      peakWidth$peakIndexLower[i+1]=peakWidth$peakIndexLower[i]       
    }
  } 
  
  nSmallGap=length(which(nGap<=n))
  if(nSmallGap>=1){
    for(j in 1:nSmallGap){
      peakWidth$peakIndexLower=peakWidth$peakIndexLower[-which(nGap<=n)[j]+j-1]
      peakWidth$peakIndexUpper=peakWidth$peakIndexUpper[-which(nGap<=n)[j]+j-1]
      peakWidth$peakIndex=peakWidth$peakIndex[-which(nGap<=n)[j]+j-1]
    }
  }
  
  return(peakWidth)
}