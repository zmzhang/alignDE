similarity=function(target,x)
{
##########################################################################################
# calculate the correlation of two vector (typically: one dimension chromatography)
# parameters:
#              x: vector of one chromatography to be aligned
#              target: vector of another chromatography, as a reference
# return:
#              sim: the similarity of two vector mention above
#
# programmer: 
#              zhangzhimin@central south university
# date:
#              Sept,15,2009
# email:
#              zhangzhimin.csu@gmail.com
###########################################################################################
  x=as.vector(x)
  target=as.vector(target)
  cx=x-mean(x)
  ctarget=target-mean(target)
  sim=sum(cx*ctarget)/(sqrt(sum(cx^2))*sqrt(sum(ctarget^2)))
  return(sim)
}