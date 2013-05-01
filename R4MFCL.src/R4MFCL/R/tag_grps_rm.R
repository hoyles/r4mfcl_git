tag_grps_rm <- function(tag.obj,keep) {
# NMD 28 Jun 2011 - included tag program labels
#
  obj <- tag.obj
  obj$hd$nrel <- length(keep)
  obj$nrecov.grp <- obj$nrecov.grp[keep]
  obj$tagprog <- obj$tagprog[keep]
  obj$rel.recov <- obj$rel.recov[obj$rel.recov$grp %in% keep,]
  obj$rel <- obj$rel[keep,]
  obj$rel.lens <- obj$rel.lens[keep,]
  obj$rel.recov$grp <- match(obj$rel.recov$grp,keep)
  return(obj)
  }
