 plot.fishery.impact.r <-
function(
plotdir="H:/rmfcl/test/figs/",
type = "Total",
plotrep = testq0,
impnames = c("ll","psass","psunass","idph","other"),
plotname="plotimpact",
plottype="wmf",
COL=T)
{
#SJH 21/01/2009
# Does the fishery impact plot by taking the output straight from the plot.rep files
# You need to give the file names for the input files
#
# Type -- total or spawning biomass
# rundir -- the dir which will contain the fished and unfished trajectories for base case
# runname -- name of the run file, e.g. plotq0.rep
# impdir -- the dir with the specific impact runs
# impnames -- character string of the names (without rep) of the impact files
if(type=="Total")
{
fish.lab <- "TotBiomass"
nofish.lab <- "TotalBiomass.nofish"
}
else
{
fish.lab <- "AdultBiomass"
nofish.lab <- "AdultBiomass.nofish"
}

# Dimensioning stuff
###############################
# Rep file with dimensions
tmp <- plotrep
#time steps
nyr <- tmp$nTimes
#first year
year1 <- tmp$Year1
#number of time steps per year
tsteps <- tmp$nRecs.yr
#year <- trunc(seq(year1,length=nyr,by=1/tsteps))
year <- trunc(seq(year1,length=nyr,by=1/tsteps))

# Aget all the major input files
###############################
fish <- tmp[fish.lab]
nofish <- tmp[nofish.lab]

for(i in 1:length(impnames))
{
data <- get(impnames[i])[nofish.lab]
data <- cbind(data[[1]],apply(data[[1]],1,sum))
data <- aggregate(data, list(year), mean)
assign(paste(impnames[i],".ann",sep=""),data)
}

# Do same calcs for fished and unfished
x <- c("fish","nofish")
for(i in 1:length(x))
{
data <- get(x[i])
data <- cbind(data[[1]],apply(data[[1]],1,sum))
data <- aggregate(data, list(year), mean)
assign(paste(x[i],".ann",sep=""),data)
}

# get gear-specific impacts
# keep a sum of total impact for use in next step
all.imp <- matrix(0,nrow=nrow(fish.ann),ncol=ncol(fish.ann))
for(i in 1:length(impnames))
{
data <- get(paste(impnames[i],".ann",sep=""))
data <- data[,2:ncol(data)]-fish.ann[,2:ncol(fish.ann)]
all.imp <- all.imp + data
assign(paste(impnames[i],".imp",sep=""),data)
}

# now do it on a regional basis
# Array by time, fishery, and region (with total)
reg.imp <- array(NA, c(nrow(fish.ann), length(impnames), ncol(fish.ann)-1))
# get gear-specific impacts
for(i in 1:length(impnames))
{
data <- get(paste(impnames[i],".imp",sep=""))
reg.imp[,i,] <- as.matrix((data/all.imp)*((nofish.ann[,2:ncol(nofish.ann)]-fish.ann[,2:ncol(fish.ann)])/nofish.ann[,2:ncol(fish.ann)])*100)
}
reg.imp[is.na(reg.imp)] <- 0

# Lazy reassigning to avoid making changes to the plotting code
# Note that there is some five fishery / six region hardwiring there
mat <- reg.imp
labs <- impnames
titles <- c(paste(rep("Region ", 6), 1:6), "WCPO")
yr <- unique(year)

if(COL){
den <- rep(NULL,5)
ang <- rep(NA,5)
cols <- c("blue", "yellow", "red", "green","grey")
}
else{
#den <- c(-1, 25, 25, -1,75)
#ang <- c(NA, NA, NA, NA,NA)
#cols <- c("black", "slate grey", "black", "gray99","slate grey")
den <- c(-1, 25, -1, -1,-1)
ang <- c(NA, 45, NA, NA,NA)
cols <- c("black", "black", "slate grey", "gray99","grey")
}

par(mfrow=c(4,2), mar=c(2,3,1,2), omi=c(0.2,0.3,0,0))
for (k in 1:7){
  maxylim <- apply(mat[,1:length(labs),k], 1, sum)
  plot(yr, maxylim,ylim=c(0,max(maxylim, na.rm=T)), type="n", xlim=c(1950, 2008), xlab="", ylab="")
  a <- c(yr, rev(yr))
  b <- rep(0, length(yr))
  d <- mat[,1,k]
  polygon(a, c(b,rev(d)), col=cols[1])
  for(i in 2:length(labs)){
    b <- d
    d <- d+ mat[,i,k]
    polygon(a, c(b,rev(d)), col=cols[i], density = den[i], angle = ang[i])
    }
  text(1960, 0.9*max(maxylim, na.rm=T), titles[k], cex=1.2)
  }
mtext(side=2, line=1, outer =T, "Impact %")
plot(yr, maxylim,ylim=c(0,max(maxylim, na.rm=T)), type="n", xlim=c(1950, 2005), xlab="", ylab="", yaxt="n", xaxt="n", bty="n")


if(COL){
legend(1950, max(maxylim, na.rm=T), legend=rev(labs),cex=1.3, pch=15,col=rev(cols))
}
else{
legend(1950, max(maxylim, na.rm=T), legend=rev(labs),cex=1.3, fill=rev(cols), density = rev(den), angle = rev(ang))
}
savePlot(filename=paste(plotdir,paste(plotname,type,sep=""),sep=""),type=plottype)
dev.off()

# Do a separate plot for the whole area
par(mfrow=c(1,1))
for (k in 7:7){
  maxylim <- apply(mat[,1:length(labs),k], 1, sum)
  plot(yr, maxylim,ylim=c(0,max(maxylim, na.rm=T)), type="n", xlim=c(1950, 2008), xlab="", ylab="")
  a <- c(yr, rev(yr))
  b <- rep(0, length(yr))
  d <- mat[,1,k]
  polygon(a, c(b,rev(d)), col=cols[1])
  for(i in 2:length(labs)){
    b <- d
    d <- d+ mat[,i,k]
    polygon(a, c(b,rev(d)), col=cols[i], density = den[i], angle = ang[i])
    }
  #text(1960, 0.9*max(maxylim, na.rm=T), titles[k], cex=1.2)
  }

if(COL){
legend(1950, max(maxylim, na.rm=T), legend=rev(labs),cex=1.3, pch=15,col=rev(cols))
}
else{
legend(1950, max(maxylim, na.rm=T), legend=rev(labs),cex=1.3, fill=rev(cols), density = rev(den), angle = rev(ang))
}
mtext(side=2,outer=T,text="Impact %",cex=1,line=-1.5)
mtext(side=1,outer=T,text="Year",cex=1,line=-2.5)
savePlot(filename=paste(plotdir,paste(plotname,"WCPO",sep=""),sep=""),type=plottype)
dev.off()
}
