 merge.tag <-
function(tag.obj,oldf,newf) {
 # By Simon D Hoyle
  a <- tag.obj$rel.recov
  a[a[,3] %in% oldf,3] <- newf
  tag.obj$rel.recov <- a
  return(tag.obj)
}
