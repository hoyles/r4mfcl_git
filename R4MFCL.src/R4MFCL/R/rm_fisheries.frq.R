 rm_fisheries.frq <-
function(frq.obj,fishery) {
  # by Simon Hoyle June 2008
  # by Nick Davies November 2010 - adjust .$dl$dsets to the new length after removal of the fishery
  m <- frq.obj$mat
  for (i in 1:length(fishery))  {
    frq.obj$fish <- frq.obj$fish[frq.obj$fish[,3]!=fishery[i],]
    m <- m[m[,4]!=fishery[i],]
    }
  frq.obj$mat <- m
  frq.obj$struct$nf <- length(frq.obj$fish$fishery)
  frq.obj$dl$dsets <- dim(frq.obj$mat)[1]   # Adjust the number of data sets
  return(frq.obj)
}
