 frq.change.nint <-
function(in.frq,add.lfint,add.wfint) {
  # function to increase the number of length and/or weight bins
  # SDH 3rd Feb 2009
  x <- in.frq
  mat <- x$mat
  nmat <- dim(mat)[1]
  got_lf <- mat[,7]!=-1
  ind_wf <- rep(8,nmat)
  ind_wf[got_lf] <- 7 + old.lfint
  got_wf <- mat[cbind(1:nmat,ind_wf)]!=-1

  old.lfint <- x$dl$lfint;  old.wfint <- x$dl$wfint;
  lfstart <- rep(7,dim(mat)[1])
  lfend <- lfstart + old.lfint - 1
  lfend[got_lf==F] <- lfstart[got_lf==F]

  new.lfstart <- lfstart
  new.lfend <- lfend
  new.lfend[got_lf] <- lfend[got_lf] + add.lfint

  wfstart <- lfend + 1
  wfend <- wfstart + old.wfint - 1
  wfend[got_wf==F] <- wfstart[got_wf==F]

  new.wfstart <- new.lfend + 1
  new.wfend <- new.wfstart + old.wfint - 1
  new.wfend[got_wf==F] <- new.wfstart[got_wf==F]

  n <- matrix(0,nrow=nmat,ncol=dim(mat)[2] + add.lfint+add.wfint)
  n[,1:7] <- mat[,1:7]

  for (i in 1:nmat) {
    n[i,7:lfend[i]] <- mat[i,7:lfend[i]]
    n[i,new.wfstart[i]:new.wfend[i]] <- mat[i,wfstart[i]:wfend[i]]
    }
    
  x$dl$lfint <- x$dl$lfint + add.lfint
  x$dl$wfint <- x$dl$wfint + add.wfint
  x$mat <- n
  return(x)
}
