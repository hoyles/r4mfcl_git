create.ver6.frq <- function(frq=base.frq)
{
##================================================================================  
## upgrades a version 4 frq file to a version six model taking advantage of the code
## from SDH's read.frq which attachs the new bits in
## SJH 29/06/09
## change the flag
##================================================================================  
  if(frq$struct$te==6) return(frq)
  frq$struct$te <- 6
  # add the Season-region flags
  frq$reg$"seas_reg_flags" <- matrix(1,ncol=frq$struct$nreg,nrow=frq$struct$tc)
  # add the age_nage   age_age1
  frq$struct$"age_inds" <- c(0,-1)
  frq$mat <- cbind(frq$mat[,1:6],rep(-1,dim(frq$mat)[1]),frq$mat[,7:dim(frq$mat)[2]])
  return(frq)
}
