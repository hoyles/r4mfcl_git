 rename.fisheries.frq <-
function(frq.obj,oldfish,newfish) {
  # by Simon Hoyle June 2008
  m <- frq.obj$mat
  for (i in 1:length(oldfish))  {
    frq.obj$fish[frq.obj$fish[,3]==oldfish[i],3] <- newfish[i]
    frq.obj$mat[m[,4]==oldfish[i],4] <- newfish[i]
    }
  return(frq.obj)
}
