 read.catchrep <- function(catchrep.file) {
  # Simon Hoyle June 2008
  # SDH 1-Sep-2013, change catch matrix orientation
  totcatch <- scan(file=catchrep.file,nlines=1,skip=1)
  nyears <- length(totcatch)
  a <- scan(file=catchrep.file,skip=3)
#  fishcatch <- matrix(a,nyears,length(a)/nyears) # 1-Sep-2013
  fishcatch <- t(matrix(a,nyears,length(a)/nyears)) # 1-Sep-2013
  return(list(totcatch=totcatch,fishcatch=fishcatch))
  }
