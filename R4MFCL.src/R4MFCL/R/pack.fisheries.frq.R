 pack.fisheries.frq <-
function(frq.obj) {
  # by Simon Hoyle June 2008
  m <- frq.obj$mat
  f.in <- unique(m[,4])
  f.out <- 1:length(f.in)
  frq.obj$fish[,3] <- f.out
  for (i in 1:length(f.in)) {m[m[,4]==f.in[i],4] <- f.out[i]}
  frq.obj$mat <- m
  return(frq.obj)
  }
