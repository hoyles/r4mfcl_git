 timesplit.frq <-
function(frq.obj, divyrs, div.fish) {
  # by Simon Hoyle June 2008
  x<-frq.obj ;  m <- x$mat ;  f <- x$fish
  a <- cbind(m[m[,4] %in% div.fish,1:4],0)
  cutyrs <- c(min(m[,1]),divyrs,max(m[,1])+1)
  a[,5] <- a[,4] + as.numeric(cut(a[,1],cutyrs-0.1,include.lowest=T))/100

  # divide the frq years among the classes represented in div.fish, so below the first -> 1.01, then 1.02 etc subset(x, ...)
  m[match(paste(a[,1],a[,2],a[,4]),paste(m[,1],m[,2],m[,4])),4] <- a[,5]
  oldfish <- sort(unique(m[,4]))
  newfish <- 1:length(oldfish)
  for (i in 1:length(div.fish))
  {
    newf <- sort(unique(m[x$mat[,4]==div.fish[i],4]))
    oldrow <- f[f$fishery==div.fish[i],]     # store the previous state
 #   browser()
    f[f$fishery==div.fish[i],3] <- newf[1]   # change the fishery number
    if (length(newf)>1) {
      for (j in 2:length(newf))
      {
        oldrow[3] <- newf[j]
        f <- rbind(f,oldrow)
      }
    }
  }
  f[,3] <- newfish[match(f[,3],oldfish)]
  f <- f[order(f[,3]),]
  m[,4] <- newfish[match(m[,4],oldfish)]
  x$fish <- f ; x$mat <- m ; frq.obj <- x
  fishsplit <- t(rbind(trunc(oldfish),newfish,tapply(m[,1],m[,4],min),f$nations))
  return(list(frq=frq.obj,fishsplit=fishsplit))
}
