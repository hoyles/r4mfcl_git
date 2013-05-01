 plot.base.comparison <-
function(baseres,labs) {
  # Run COMPARISON PLOT
  # Adam Langley 2007
  # Simon Hoyle March 2009
  cols <- rainbow(length(labs),alpha=0.5)
  par(mfrow=c(2,1), mar=c(4,4,1,1))
  plot(baseres$Bmsy/1000, baseres$MSY/1000, pch=16, cex=2, col=cols, ylab="MSY (Annual, mt 1000s)", xlab="Bmsy (mt 1000s)")
  text(baseres$Bmsy/1000, baseres$MSY/1000, as.character(1:length(labs)), cex=0.7)
  #legend(1400, 360, cex=0.75, labs, pch=16, col=cols, ncol=2)
  x <- baseres$Bcurr.Bmsy; y <- baseres$Fcurr.Fmsy
  plot(x, y, pch=16, cex=1.9, col=cols, xlab="B/Bmsy", ylab="F/FMSY", xlim=c(min(x)/1.4,max(x)), ylim=c(0,max(y)))
  text(x, y, as.character(1:length(labs)), cex=0.6, col=1)
  abline(h=1, lty=2)
  abline(v=1, lty=2)
  legend("topleft", cex=0.75, paste(labs," (",1:length(labs),")",sep="") ,
  pch=16, col=cols, ncol=1, bg="white", x.intersp = 0.5, bty="n")
}
