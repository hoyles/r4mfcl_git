 plot.nofishing <-
function(
plotdir="H:/rmfcl/test/figs/",plotrep=testq0,
type="SSB",
plotname="H:/rmfcl/test/figs/Kobe",
plottype="wmf",
COL=T)
{
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------

# Dimensioning
#time steps
nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr
#number regions
nreg <- plotrep$nReg

year <- trunc(seq(year1,length=nyr,by=1/tsteps))


# reading in the numbers
if(type=="SSB")
{
  B <- plotrep$AdultBiomass
  Bnof <- plotrep$AdultBiomass.nofish
  textlab <- "Adult biomass (1000s mt)"
}
else
{
  B <- plotrep$TotBiomass
  Bnof <- plotrep$TotalBiomass.nofish
  textlab <- "Total biomass (1000s mt)"
}
  ##-- add on totals  and divide by 1000
  B <- cbind(B,apply(B,1,sum)) /1000
  Bnof <- cbind(Bnof,apply(Bnof,1,sum))  /1000

  ##--- aggregate by year
  B <- aggregate(B,list(year),mean)
  #year <- B[,1]
  #B <- B[,-1]
  Bnof <- aggregate(Bnof,list(year),mean)


  nplt <- nreg+1
  opar <- par(mfrow=c(4,2),mar=c(2,4,1,2)+.1,lwd=.5,xpd=T, omi=c(0,0.2,0,0))
  #on.exit(par(opar))

  labs <- c(paste("Region",seq(nplt-1)),"WCPO")
   year <- B[,1]
  for(i in 1:nplt){
    plot(year,Bnof[,i+1],type='n',
         ylim=range(0,B[,i+1],Bnof[,i+1]),ann=F,axes=F,lwd=2,col=2)
    lines(year,B[,i+1],lwd=2,lty=1)

if(COL)
{
    lines(year,Bnof[,i+1],lwd=2,lty=2,col=2)
}
else
{
    lines(year,Bnof[,i+1],lwd=2,lty=2,col=1)
}

    box(bty='l',lwd=.1)
    axis(2,lwd=.1,cex.axis=.7)
    if(i==6 | i==nplt) axis(1,lwd=.1,cex.axis=.7) else axis(1,labels=F,lwd=.1)
    mtext(labs[i],side=3,adj=0,line=0,cex=.7)
}
#plot legend
 plot(year,Bnof[,1+1],type='n',
         ylim=range(0,B[,1+1],Bnof[,1+1]),ann=F,axes=F,lwd=2,col=2)
if(COL)
{
    legend(1955,mean(c(0,max(Bnof[,1+1]))),lty=1:2,lwd=2,col=1:2,cex=.7,
         legend=c("Fished biomass","Unfished biomass"),y.intersp=1.5)
}
else
{
    legend(1955,mean(c(0,max(Bnof[,1+1]))),lty=1:2,lwd=2,col=1,cex=.7,
         legend=c("Fished biomass","Unfished biomass"),y.intersp=1.5)
}

mtext(side=2, outer=T, text=textlab, line=0)

savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}
