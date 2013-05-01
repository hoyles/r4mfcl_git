 merge.frq <-
function(frq.obj,oldf,newf,mergelf=FALSE) {
  # by Simon Hoyle June 2008
  x<-frq.obj ;  m <- x$mat ;  f <- x$fish
  #sum catch and effort
  lb <- paste(m[,1],m[,2])           # label all rows by newfish,y,m
  lbx <- paste(m[,4],m[,1],m[,2])
  oldyq <- lb[m[,4] == oldf]  # yr_qrs in the old
  newyq <- lb[m[,4] == newf]  # yr_qrs in the new
  inboth <- newyq[newyq %in% oldyq]     # yrqs in both
  onlyold <- oldyq[(oldyq %in% newyq)==F]
                        # if old fishery has a matching yr and qtr in new fishery, then sum catch and effort
  m[lbx %in% paste(newf,inboth),5:6] <- m[lbx %in% paste(oldf,inboth),5:6] + m[lbx %in% paste(newf,inboth),5:6]
  if (mergelf) {
    m[lbx %in% paste(oldf,inboth),7] <- max(0,m[lbx %in% paste(oldf,inboth),7])
    m[lbx %in% paste(newf,inboth),7] <- max(0,m[lbx %in% paste(newf,inboth),7])
    m[lbx %in% paste(newf,inboth),7:106] <- m[lbx %in% paste(oldf,inboth),7:106] + m[lbx %in% paste(newf,inboth),7:106]
    a <- apply(m[lbx %in% paste(newf,inboth),7:106],1,sum)
    m[lbx %in% paste(newf,inboth),7][a==0] <- -1
    }
  m[lbx %in% paste(oldf,onlyold),4] <- newf    # if no match in the new fishery, then replace the fishery name
  m <- m[m[,4]!=oldf,]                         # remove the old rows
  f <- f[f$fishery != oldf,]                   # remove old fishery from f

  x$mat <- m ;  x$fish <- f
  frq.obj <- x
  return(frq.obj)
}
