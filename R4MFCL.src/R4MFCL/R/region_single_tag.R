 region_single_tag <-
function(tag,region,keepfish) {
  # Simon D Hoyle March 2010
  # Change to a single region, removing all fisheries outside that region
  keep <- tag$rel$reg==region
  tag$rel <- tag$rel[keep,]
  tag$hd$nrel <- sum(keep)
  tag$rel.lens <- tag$rel.lens[keep,]
  tag$rel.recov <- tag$rel.recov[keep[tag$rel.recov$grp] & tag$rel.recov$fsh %in% keepfish,]
  tag$nrecov.grp <- as.vector(table(factor(tag$rel.recov$grp,levels=(1:length(tag$nrecov.grp)))))
  tag$nrecov.grp <- tag$nrecov.grp[keep]
  return(tag)
  }
