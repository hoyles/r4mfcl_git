plot.tagreportrates <- function(parf=basepar,tagrep1=tagrep1) {
#----------------------------------------------------------------------------
# code to make tag reporting rate plot
# modified from original by Adam? 
# 17/7/2011 SDH modified to work for multiple reporting rate structure
#----------------------------------------------------------------------------
  if(is.character(parf)) parf <- read.par(parf)
  # The following code sets up the input parameter tagrep1 for skipjack, which has different RR for each fishery RR group * release group. 
  # fishgrp is a label for each fishery RR group
  # params links each fishery RR group to its fisheries
  # str(params): 'data.frame':   18 obs. of  2 variables:
  # $ fishery: int  1 2 3 4 5 6 7 8 9 10 ...
  # $ par_grp: int  1 1 1 1 1 2 3 4 4 5 ...
  # fishgrp <- c("JP all","R2 PG PL","R2 SB PL","R2 all PS","R2 PH dom","R2 ID dom","R3 FJ PL","R3 all PS") 
  # params <- read.csv("../Data_preparation/basefiles/RRparam_design18.csv")
  # fish_grp <- tapply(params$fishery,params$par_grp,paste,collapse=" ")
  # tagrep1 <- data.frame(grp=rep(1:8,4),release=rep(unique(tagobj$tagprog),each=8),flag=rep(fish_grp,4),label=fishgrp)
  # str(tagrep1) -> 'data.frame':   32 obs. of  4 variables:
  # $ grp    : int  1 2 3 4 5 6 7 8 1 2 ...
  # $ release: Factor w/ 4 levels " JPN","PTTP",..: 4 4 4 4 4 4 4 4 3 3 ...
  # $ flag   : Factor w/ 8 levels "1 2 3 4 5 12 13 17",..: 1 6 7 8 2 3 4 5 1 6 ...
  # $ label  : Factor w/ 8 levels "JP all","R2 all PS",..: 1 4 6 2 5 3 8 7 1 4 ...

  taggrp <- sort(unique(as.vector(parf$trpgpfl)))
  tagests <- tapply(as.vector(parf$trpfl),as.vector(parf$trpgpfl),mean)
  tagpriors <- tapply(as.vector(parf$treptarg)/100,as.vector(parf$trpgpfl),mean)
  tagpens <- tapply(as.vector(parf$treppen),as.vector(parf$trpgpfl),mean)
  tagrepgrps <- data.frame(RR=tagests, target = tagpriors, cv = 1/(2*sqrt(tagpens)), grp = taggrp, nms = paste(tagrep1$release,"rel,", tagrep1$label, "rec"))


  nfish <- dim(tagrepgrps$grp)[2]
  estrr <- tagrepgrps$RR

  # get priors
  mean <- tagrepgrps$target
  sd <- tagrepgrps$cv
  prior <- data.frame(mean=tagrepgrps$target,lcb=mean-1.96*sd,ucb=mean+1.96*sd)

  x11(height=12,width=16)
  par(mar=c(10,5,1,2)+.1,lwd=.5)
  opar <- par(mar=c(11.5,5,1,2)+.1,lwd=.5)
  on.exit(par(opar))

  plot(c(1,length(estrr)),c(0,1),type='n',ann=F,axes=F)

  dx <- .3
  x <- seq(estrr)
  for(i in x) {
    y1 <- max(0,prior$lcb[i])
    y2 <- min(0.9,prior$ucb[i])
    polygon(c(rep(i-dx,2),rep(i+dx,2)),c(y1,rep(y2,2),y1),
          border="white",col="LightSteelBlue")
  }
  points(x,prior$mean,pch=18,cex=2,col="white")
  points(x,estrr,pch=19,cex=2)

  box(bty='l',lwd=.1)
  axis(2,lwd=.1,cex.axis=.7,las=2)
  title(ylab="Tag reporting rate",cex.lab=1)
  #  labs <- scan("labels.tmp",what="",sep=":",quiet=T)[1:17]
  labs <- tagrepgrps$nms
  axis(1,lwd=.1,at=x,labels=labs,las=2,cex.axis=.65)

  box()

legend(1,1.04,pch=c(5,19),col=1,cex=.7,legend=c("Prior","Estimate"),y.intersp=1.5, bty="n", ncol=2)
}

