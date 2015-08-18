scanText <- function(string, what=character(0),...){
  tc <- textConnection(string)
  result <- scan(tc, what=what, quiet=TRUE,...)
  close(tc)
  return(result)
}
