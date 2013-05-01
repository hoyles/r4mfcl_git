 seas.tag <-
function(tag.obj,fishlist) {
  # by Simon Hoyle June 2008
  ms <- rbind(c(1,2,3,4),c(2,5,8,11))
  a <- tag.obj$rel.recov
  b <- a
  for (i in (dim(fishlist)[1]):1) {
    m <- ms[2,fishlist[i,3]]
    if (fishlist[i,3] == 0) {  b[a[,3]==fishlist[i,2],3] <- fishlist[i,1] } else {
      b[a[,3]==fishlist[i,2] & a[,5] == m,3] <- fishlist[i,1]
      }
    }
  tag.obj$rel.recov <- b
  return(tag.obj)
}
