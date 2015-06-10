plot.biomass.betyft <- function(plotrep=read.rep(baserep),varfile=NULL,type="SSB",nrow=5,legpos="topright")
{
##-----------------------------------------------------------------------
# SJH 20/1/2009
# Plots biomass by region and then combined
# Option to add on CI as a polygoon
#
# SJH 16/11/2011 -- save the plot outside the function
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
      textlab <- "Spawning potential"
    }
    else
    {
      B <- plotrep$TotBiomass
      textlab <- "Total biomass (1000s mt)"
    }

      if(is.null(varfile))
      {
      }
      else
      {
          if(type=="SSB")
          {
            Breg <- varfile$ln_adult_reg_bio
            Btot <- varfile$adult_rbio
          }
          else
          {
            Breg <- varfile$ln_reg_bio
            Btot <- varfile$rbio
          }
      Btot.ub <- exp(Btot[,2]+2*Btot[,1]) /1000000
      Btot.lb <- exp(Btot[,2]-2*Btot[,1]) /1000000
      Breg.ub <- exp(Breg[,,2]+2*Breg[,,1]) /1000000
      Breg.lb <- exp(Breg[,,2]-2*Breg[,,1]) /1000000
      # now get them into the same format as the B
      B.ub <- cbind(Breg.ub[,1:plotrep$nReg],Btot.ub)
      B.lb <- cbind(Breg.lb[,1:plotrep$nReg],Btot.lb)
      ##--- aggregate by year
      B.ub <- aggregate(B.ub,list(year),mean)
      B.lb <- aggregate(B.lb,list(year),mean)
      }
  ##-- add on totals  and divide by 1000
  B <- cbind(B,apply(B,1,sum)) /1000
  ##--- aggregate by year
  B <- aggregate(B,list(year),mean)
  #browser()
  nplt <- nreg+1
  opar <- par(mfrow=c(nrow,2),mar=c(2,4,1,2)+.1,lwd=.5,xpd=T, omi=c(0,0.2,0,0))
  on.exit(par(opar))

  labs <- c(paste("Region",seq(nplt-1)),"WCPO")
   year <- B[,1]
 #browser()

  for(i in 1:nplt){
    if(is.null(varfile)||i<nplt)
    {
    y.ub <- max(c(0,B[,i+1]))
    }
    else
    {
    y.ub <- max(c(0,B.ub[,i+1]))
    }

    plot(year,B[,i+1],type='n',ylim=range(0,y.ub,na.rm=T),ann=F,axes=F,lwd=2,col=2)
# Add the CI if necessary
    if(is.null(varfile))
    {
    }
    else
    {
    if(i==nplt) polygon(c(year,rev(year)),c(B.ub[,i+1],rev(B.lb[,i+1])),border="white",col="LightSlateGrey")
    }
    lines(year,B[,i+1],lwd=2,lty=1)
    box(bty='l',lwd=.1)
    axis(2,lwd=.1,cex.axis=.7,las=1)
    if(i==nreg | i==nplt) axis(1,lwd=.1,cex.axis=.7) else axis(1,labels=F,lwd=.1)
    #mtext(labs[i],side=3,adj=0,line=0,cex=.7)
    legend(legpos,labs[i],bty="n",cex=0.8)
    }
mtext(side=2, outer=T, text=textlab, line=0)
}


