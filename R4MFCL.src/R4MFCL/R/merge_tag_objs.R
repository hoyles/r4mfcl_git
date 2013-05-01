 merge_tag_objs <-
function(obj1,obj2,relgrps) {
  # by Simon Hoyle June 2010 for skj 
  # relgrps is the grps from b to add to a
  a <- obj1
  b <- obj2
  nrelnums <- (a$hd$nrel+1):(a$hd$nrel + length(relgrps))
  a$hd$nrel <- a$hd$nrel + length(relgrps)
  a$nrecov.grp <- c(a$nrecov.grp,b$nrecov.grp[relgrps])
  a$rel <- rbind(a$rel,b$rel[relgrps,])
  a$rel.lens <- rbind(a$rel.lens,b$rel.lens[relgrps,])
  x_rel.recov <- b$rel.recov[b$rel.recov[,1] %in% relgrps,]
  x_rel.recov[,1] <- nrelnums[match(x_rel.recov[,1],relgrps)]
  a$rel.recov <- rbind(a$rel.recov,x_rel.recov)
  return(a)
}
