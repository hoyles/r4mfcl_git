# File to compare results in MFCL plot.rep output file
#
compare.bio.rep <- function(vrep = c(""), vpar = c(""), vcrep = c(""), 
  modnms = c(""), plotfol = c(""), plotnm ="", lwidth = 1, yr_rng = NA){
 
# List of arguments passed to function:
# vrep: vector of plot.rep path/filenames
# vpar: vector of par path/filenames
# vcrep: vector of catch.rep path/filenames
# modnms: vector of model names
# plotfol: folder into which output is to be put
# plotnm: prefix for all output files
# lwidth: width of line for plots
# yr_rng: range of years in recruitment time series to be plotted, e.g. c(150:244)

# Ideas for development:
# - function arguments: 
#   vector of model names
#   vector of model filepaths
#   output folder location
#   working directory location
#
#
# Read in outputs
if(length(vrep) > 7){
  print("No. models to compare > 7!!")
  break()
}
rslt <- c("a_rslt","b_rslt","c_rslt","d_rslt","e_rslt","f_rslt","g_rslt")
pars <- c("a_par","b_par","c_par","d_par","e_par","f_par","g_par")
lst.rslt <- list()
lst.pars <- list()
for(i in 1:length(vrep)){
  lst.rslt[[i]] <- read.rep(vrep[i])
  lst.pars[[i]] <- read.par(vpar[i])
}
names(lst.rslt) <- rslt[c(1:length(vrep))]
names(lst.pars) <- pars[c(1:length(vrep))]

# Get actual model years 
tmp <- 0
tmp2 <- vector(mode="numeric")
tmp3 <- vector(mode="numeric")
for(i in 1:length(vrep)){
  tmp <- max(tmp,lst.rslt[[i]]$nTimes)
  tmp2 <- append(tmp2,lst.rslt[[i]]$Year1)
  tmp3 <- append(tmp3,lst.rslt[[i]]$nRecs.yr)
}
if(length(unique(tmp2)) > 1 | length(unique(tmp3)) > 1){
  print("ERROR: start year or nRec.yr is different among models")
  stop()
}
yr1 <- tmp2[1] + ((1/tmp3[1])/2)
realyrs <- seq(from=yr1, to= yr1+((tmp-1)*(1/tmp3[1])), by= 1/tmp3[1])
#
mods <- c("a","b","c","d","e","f","g")
mods <- mods[c(1:length(vrep))]
modnames <- modnms
#
# Output location 
plotfol <- plotfol
#
#
# Compare total biomass
ymax <- 0
nmax <- 0
nmodyrs <- vector(mode="numeric", length=length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    yy <- lst.rslt[[i]][["TotBiomass"]]  
  } else {
    yy <- rowSums(lst.rslt[[i]][["TotBiomass"]])
  }
  ymax <- max(max(yy),ymax)
  nmax <- max(length(yy),nmax)
  nmodyrs[i] <- length(yy)
}
ymat <- matrix(NA,nrow = nmax, ncol= length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    ymat[c(1:nmodyrs[i]),i] <- lst.rslt[[i]][["TotBiomass"]]
  } else {
    ymat[c(1:nmodyrs[i]),i] <- rowSums(lst.rslt[[i]][["TotBiomass"]])
  }
}
yy <- ymat[,1]
if(length(yy) != length(realyrs)){
  print("ERROR:year dimensions don't match")
  stop()
}
plot(realyrs, yy, type="n", lty =1, lwd = lwidth, main = "Comparison total biomass", xlab = "Time interval", ylab = "Biomass (mt)", ylim = c(0,ymax))
for(i in 1:length(mods)){
  lines(realyrs,ymat[,i],type="l",lty = i, lwd = lwidth, col = i) 
}
legend("topright",legend = modnames, lty = c(1: length(mods)), col =  c(1: length(mods)), lwd = lwidth)
filnm <- paste(plotfol,"/",plotnm,"_biom_comprsn",sep="")
savePlot(paste(filnm,".png",sep=""), type = "png")
#
# Compare growth functions
ymax <- 0
for(i in 1:length(mods)){
  yy <- lst.rslt[[i]][["mean.LatAge"]]
  yy_sd <- lst.rslt[[i]][["sd.LatAge"]]
  ymax <- max(max(yy+yy_sd),ymax)
}
yy <- lst.rslt[[1]][["mean.LatAge"]]
yy_sd <- lst.rslt[[1]][["sd.LatAge"]]
plot(1:length(yy), yy, type="n", lty =1, lwd = lwidth, main = "Comparison growth curves (+/- 1sd)", xlab = "Age", ylab = "Length (cm)", ylim = c(0,ymax))
for(i in 1:length(mods)){
  yy <- lst.rslt[[i]][["mean.LatAge"]]
  yy_sd <- lst.rslt[[i]][["sd.LatAge"]]
  lines(1:length(yy),yy,type="l",lty = 1, lwd = 3, col = i) 
  lines(1:length(yy),yy+yy_sd,type="l",lty = 3, lwd = lwidth, col = i) 
  lines(1:length(yy),yy-yy_sd,type="l",lty = 3, lwd = lwidth, col = i) 
}
legend(0,ymax,legend = modnames, lty = 1, col =  c(1: length(mods)), lwd = lwidth)
filnm <- paste(plotfol,"/",plotnm,"_growth_comprsn",sep="")
savePlot(paste(filnm,".png",sep=""), type = "png")
#
# Table of Likelihoods
like_tbl <- data.frame(Mods = mods, Like = rep(NA,length=length(mods)))
for(i in 1:length(mods)){
  like_tbl[i,"Like"] <- lst.pars[[i]][["obj"]]  
}
filnm <- paste(plotfol,"/",plotnm,"_likl_comprsn.txt",sep="")
write.table(like_tbl,file=filnm,quote=FALSE,row.names=FALSE)
#
# Get outcomes of assessment
mod_outs <- list()
for(i in 1:length(mods)){
  mod_outs[[i]] <- get.outcomes.2014(vrep[i],vpar[i],vcrep[i])  
}
names(mod_outs) <- modnames
#
# Put into a dataframe
outputs <- c(names(mod_outs[[1]]))
comprsn_outs <- data.frame(Qnts = outputs, dummy = rep(NA, length=length(outputs)))
# Add model outputs to the dataframe
for(i in 1:length(mods)){
  comprsn_outs[,(i+1)] <- unlist(mod_outs[[i]])
}
names(comprsn_outs)[2:(length(mods)+1)] <- modnames
#
# Percentage difference relative to first model
comprsn_pcntg_outs <- comprsn_outs
comprsn_pcntg_outs[,c(2:(length(mods)+1))] <-  comprsn_pcntg_outs[,c(2:(length(mods)+1))]/comprsn_outs[,2]
#
filnm <- paste(plotfol,"/",plotnm,"_comprsn_outs.txt",sep="")
write.table(comprsn_outs,file=filnm,quote=FALSE,row.names=FALSE)
#
filnm <- paste(plotfol,"/",plotnm,"_comprsn_pcntg_outs.txt",sep="")
write.table(comprsn_pcntg_outs,file=filnm,quote=FALSE,row.names=FALSE)

# Compare total recruitment
ymax <- 0
nmax <- 0
nmodyrs <- vector(mode="numeric", length=length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    yy <- lst.rslt[[i]][["Recruitment"]]
  } else {
    yy <- rowSums(lst.rslt[[i]][["Recruitment"]])
  }
  ymax <- max(max(yy),ymax)
  nmax <- max(length(yy),nmax)
  nmodyrs[i] <- length(yy)
}
ymat <- matrix(NA,nrow = nmax, ncol= length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    ymat[c(1:nmodyrs[i]),i] <- lst.rslt[[i]][["Recruitment"]]
  } else {
    ymat[c(1:nmodyrs[i]),i] <- rowSums(lst.rslt[[i]][["Recruitment"]])
  }
}
yy <- ymat[,1]
if(!is.na(yr_rng)){
  rng <- yr_rng
} else {
  rng <- c(1:length(yy))
}
plot(realyrs[rng], yy[rng], type="n", lty =1, lwd = lwidth, main = "Comparison total recruitment", xlab = "Time interval", ylab = "Recruitment (numbers)", ylim = c(0,ymax))
for(i in 1:length(mods)){
  lines(realyrs[rng],ymat[rng,i],type="l",lty = i, lwd = lwidth, col = i) 
}
legend(min(realyrs[rng]),ymax,legend = modnames, lty = c(1: length(mods)), col =  c(1: length(mods)), lwd = lwidth)
filnm <- paste(plotfol,"/",plotnm,"_recr_comprsn",sep="")
savePlot(paste(filnm,".wmf",sep=""), type = "wmf")
#

# Difference among recruitments
ymax <- 0
nmax <- 0
nmodyrs <- vector(mode="numeric", length=length(mods))
nrw <- vector(mode="numeric", length=length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    nrw[i] <- length(lst.rslt[[i]][["Recruitment"]])
  } else {
    nrw[i] <- dim(lst.rslt[[i]][["Recruitment"]])[1]
  }
}
yy <- matrix(NA, nrow = max(nrw),  ncol=length(mods))
for(i in 1:length(mods)){
  if(lst.rslt[[i]][["nReg"]] == 1){
    yy[c(1:nrw[i]),i] <- lst.rslt[[i]][["Recruitment"]]
  } else {
    yy[c(1:nrw[i]),i] <- rowSums(lst.rslt[[i]][["Recruitment"]])
  }
}  
yy <- yy[,1]/yy[,2]
ymax <- max(max(yy,na.rm=TRUE),ymax)
nmax <- max(length(yy),nmax)
nmodyrs[i] <- length(yy)
plot(1:length(yy), yy, type="l", lty =1, lwd = lwidth, main = "Difference in total recruitment", xlab = "Time interval", ylab = "Recruitment diff: Ref / Sensitivity", ylim = c(0,ymax))
abline(h=1)
filnm <- paste(plotfol,"/",plotnm,"_recr_diffrnc",sep="")
savePlot(paste(filnm,".wmf",sep=""), type = "wmf")
#


#  End of Function
}


########################################################################################################################################################################################################


