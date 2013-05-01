 rm_fisheries.tag <-
function(tag.obj,fisheries) {
  # by Simon Hoyle June 2008
  a <- tag.obj$rel.recov
  for (i in 1:length(fisheries)) {
    a <- a[a[,3]!=fisheries[i],]
    }
  tag.obj$nrecov.grp <- as.vector(table(factor(a$grp,levels=(1:length(tag.obj$nrecov.grp)))))
  tag.obj$rel.recov <- a
  return(tag.obj)
}
