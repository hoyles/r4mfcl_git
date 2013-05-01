 rename.fisheries.tag <-
function(tag.obj,oldfish,newfish) {
  # by Simon Hoyle June 2008
  a<-tag.obj
  for (i in 1:length(oldfish))
  {
    a$rel.recov$fsh[a$rel.recov$fsh==oldfish[i]] <- newfish[i]
  }
  tag.obj <- a
  return(tag.obj)
}
