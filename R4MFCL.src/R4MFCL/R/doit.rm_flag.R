 doit.rm_flag <-
function(a,flagtype,flag,value) {
  # by Simon D Hoyle March 2010
  mt <- paste(formatC(flagtype,format="d"),flag,value,sep=" ")
  loc <- grep(mt,a)                                             # find the rows
  if(length(loc)!=0) { a <- a[-loc] }
  return(a)
  }
