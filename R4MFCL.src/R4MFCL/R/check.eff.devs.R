 check.eff.devs <-
function(parfile,repfile,frqfile,parlim=5.9) {
# By Simon D Hoyle 2009
  par.obj <- read.par(parfile)
  rep.obj <- read.rep(repfile)
  frq <- read.frq(frqfile)
  b <- frq$mat
  locs <- abs(par.obj$effdevcoffs)>parlim
  locs <- grep(T,locs)
  dims <- dim(par.obj$effdevcoffs)
  yq <- rep.obj$Rlz.t.fsh[locs]
  fi <- rep(1:dims[1],dims[2])[locs]
  ed <- par.obj$effdevcoffs[locs]
  y <- trunc(yq)
  q2 <- 12*(yq-trunc(yq))+0.5
  ce <- b[b[,4] %in% fi & b[,1] %in% y & b[,2] %in% q2,5:6]
  cbind(fi,y,q2,yq,ed)
  a <- data.frame(cbind(b[paste(b[,4],b[,1],b[,2]) %in% paste(fi,y,q2),1:7],ed))
  names(a) <- c("Y","M","X","fishery","catch","effort","x2","effdev")
  return(a)
}
