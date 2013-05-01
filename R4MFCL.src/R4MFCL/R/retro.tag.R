 retro.tag <-
function(tag.obj,yr) {
  # Simon D Hoyle Jan 2009
  a <- tag.obj
  keep <- (1:a$hd$nrel)[a$rel$y < yr]
  a$hd$nrel <- length(keep)
  a$rel <- a$rel[keep,]
  a$rel.recov <- a$rel.recov[a$rel.recov$grp %in% keep,]
  a$rel.recov <- a$rel.recov[a$rel.recov$yr < yr,]
  a$rel.recov$grp <- match(a$rel.recov$grp,keep)
  a$nrecov.grp <- a$nrecov.grp[keep]
  for (i in 1:a$hd$nrel) { a$nrecov.grp[i] <- sum(a$rel.recov$grp==i) }
  a$rel.lens <- a$rel.lens[keep,]
  return(a)
}
