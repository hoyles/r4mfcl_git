 read.catchrep <-
function(catchrep.file) {
  # Simon Hoyle June 2008
  totcatch <- scan(file=catchrep.file,nlines=1,skip=1)
  nyears <- length(totcatch)
  a <- scan(file=catchrep.file,skip=3)
  fishcatch <- matrix(a,length(a)/nyears,nyears)
  return(list(totcatch=totcatch,fishcatch=fishcatch))
  }
