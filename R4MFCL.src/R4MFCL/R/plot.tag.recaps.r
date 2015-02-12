
plot.tag.recaps <- function(tagdat, parfile){
  ##  obs and pred caps by tag programme
# Correct the name of the CS programme
  tagz <- tagdat
  tagz$auxdat$prog[tagz$auxdat$prog=="m CS"] <- "CS"

  mixprd <- parfile$tfl[1,1] / 4 # time periods as proportion of 1 year

  sel <- with(tagz$auxdat,which((ctime-rtime)>mixprd))
  tt <- with(tagz$auxdat[sel,],aggregate(list(orec=orec),
                                        by=list(ctime=ctime,pg=prog),
                                        sum))
  prec <- with(tagz$auxdat[sel,],aggregate(list(orec=orec,prec=prec),
                                           by=list(ctime=ctime),sum))
  pgn <- c(1:length(unique(tagz$auxdat$prog)))
  names(pgn) <- unique(tagz$auxdat$prog)

  par(mfrow=c(1,1))#,mar=c(6,6,3,3))
  with(tt,plot(ctime,orec,log="y",las=1, xlab='Time', ylab='log(Recaptures)', ylim=c(0.5,max(orec))))  #,ylim=c(.1,2200)

  with(tt,points(ctime,orec,col=pgn[pg]+1,pch=20,cex=1.5))
  with(prec,lines(ctime,prec,lwd=2))
  with(prec,points(ctime,orec,cex=1.5))
  ymax <- max(c(prec$orec,prec$prec))*2
  box()
  legend("topleft",legend=names(pgn),pch=c(20,20,20),cex=1.5,col= c(pgn)+1,bty="n")
}


plot.tag.diags2(kk, yftpar)

