 sort.frq <-
function(frq.obj) {
  # by Simon Hoyle June 2008
  frq.obj$fish <- frq.obj$fish[order(frq.obj$fish[,3]),]
  m <- frq.obj$mat
  frq.obj$mat <- m[order(m[,4],m[,1],m[,2]),]
  return(frq.obj)
}
