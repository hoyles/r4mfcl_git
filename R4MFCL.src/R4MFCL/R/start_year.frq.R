 start_year.frq <-
function(frq.obj,start_yr,halfyr=F) {
  # by Simon Hoyle June 2008
  m <- frq.obj$mat
  m <- m[m[,1]>=start_yr,]
  if(halfyr) { m <- m[m[,1]>start_yr | m[,2]>6,] }
  frq.obj$mat <- m
  frq.obj$struct$yr1 <- start_yr
  return(frq.obj)
}
