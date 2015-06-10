plot.periods.at.liberty = function(tmp.rep=read.rep(baserep))
{
at.lib <- tmp.rep$ObsvPredbyLib
plot(at.lib[,1],type="p",col="red",pch=19,lwd=3,las=1,xlab="Periods at liberty (quarters)",ylab="No. tag returns")
lines(at.lib[,2],lwd=2)
legend("topright",pch=c(19,NA),lty=c(NA,1), lwd=c(1,2), col=c("red","black"),cex=1.2,legend=c("Observed","Predicted"),bty="n")
}




