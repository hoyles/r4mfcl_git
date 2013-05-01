 seas.frq <-
function(frq.obj, seas.fish) {
  # by Simon Hoyle June 2008
  x<-frq.obj ;  m <- x$mat ;  f <- x$fish
  a <- m[m[,4] %in% seas.fish,c(4,2)]
  m[m[,4] %in% seas.fish,4] <- a[,1] + a[,2]/12
  oldfish <- sort(unique(m[,4]))
  newfish <- 1:length(oldfish)
  for (i in 1:length(seas.fish))
  {
    newf <- sort(unique(m[x$mat[,4]==seas.fish[i],4]))
    oldrow <- f[f$fishery==seas.fish[i],]
    f[f$fishery==seas.fish[i],3] <- newf[1]
    for (j in 2:length(newf))
    {
      oldrow[3] <- newf[j]
      f <- rbind(f,oldrow)
    }
  }
  f[,3] <- newfish[match(f[,3],oldfish)]
  f <- f[order(f[,3]),]
  m[,4] <- newfish[match(m[,4],oldfish)]
  x$fish <- f ; x$mat <- m ; frq.obj <- x
  return(frq.obj)
}
