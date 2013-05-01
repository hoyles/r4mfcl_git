 timesplit.tag <-
function(tag.obj,fishsplit) {
  # by Simon Hoyle June 2008
  fishsplit <- matrix(as.numeric(fishsplit[,1:3]),ncol=3)
  a <- tag.obj$rel.recov
  b <- a
  for (i in 1:dim(fishsplit)[1]) {
    fd <- fishsplit[i,]
    fishgrp <- fishsplit[fishsplit[,1]==fd[1],3]
    minyr <- fd[3]
    tmp <- match(fd[3],fishgrp)
    maxyr <- ifelse(tmp==length(fishgrp),max(a[,4]+1),fishgrp[tmp+1]) # the end of the time group for the fishery
    b[a[,3]==fishsplit[i,1] & a[,4] %in% minyr:(maxyr-1),3] <- fishsplit[i,2]
    }
  tag.obj$rel.recov <- b
  return(tag.obj)
}
