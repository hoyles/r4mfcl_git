 retro.frq <-
function(frq.obj,retro.tag.obj=NA) {
  # Simon D Hoyle Jan 2009
  # Rebuild the frq file for a retrospective analysis
  a <- frq.obj
  a$mat <- a$mat[a$mat[,1]<=yr,]
  if(is.na(retro.tag.obj)==F) a$struct$ntg <- retro.tag.obj$hd$nrel
  return(a)
  }
