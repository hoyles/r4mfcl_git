 get.weight.output <-
function(REGION=1,DIR="P:/yft/2009/Data Preparation/size data/")
{
# By Adam Langley
par(mfcol=c(8,4), mar=c(2,2,2,2))
getregion <- REGION  # REGION X
filename <- paste(DIR,"LLwtdataR", getregion, ".txt", sep="")
maxwt <- 200
minwt <- 1
wtstep <- minwt:maxwt
out <- paste("###REGION", getregion, "WEIGHT DATA 1-200, 1kg intervals")
write.table(out, filename, quote = FALSE, sep = " ", eol = "\n", na = "NA", row.names = FALSE, col.names = FALSE)
rm(out)
out <- c("###YR, QTR, 1, 2, 3, 4, ......200")
write.table(out, filename, quote = FALSE, append = TRUE, sep = " ", eol = "\n", na = "NA", row.names = FALSE, col.names = FALSE)
rm(out)
#get data from region
data <- cbind(wtdata, region[,getregion])
data <- data[region[,getregion] > 0,]
names(data) <- c(names(wtdata), "region_prop")
data <- data[data$wt <= maxwt,]
data <- data[data$wt >= minwt,]
##for BET there are two different area strata used for 1965-current
#need to talk to PW about this.
#in the interim delete the finer resolution data set
data <- data[data$astrat != "0",]
data$index <- paste(data$yr, data$qtr)
index <- sort(unique(data$index))
#areaid <- unique(paste(data$astrat, data$minlat, data$minlong, data[,28]))
areaid <- unique(paste(data$minlat2, data$minlong2, data$region_prop))
#areaid <- unique(paste(data$minlat2, data$minlong2))
#areaidll <- unique(paste(data$astrat, data$minlat, data$minlong))
areaidll <- unique(paste(data$minlat2, data$minlong2))
for (i in 1:length(index)){
#stratwt <- unique(data$astrat[data$index == index[i]])
#if(length(stratwt) > 1)
#  {
#  print("More than one areal stratification in time step XXXXXXX!!!!!")
#  break
#  }
##convert catch data into the same temporal strata
#longbin <- strat$long[match(stratwt, strat$strat_id)]
longbin <- 20
ll$minlong <- trunc(ll$LOND/longbin) * longbin
#latbin <- strat$lat[match(stratwt, strat$strat_id)]
latbin <- 10
ll$minlat <- trunc(ll$LATD/latbin) * latbin
ll$minlat <- ifelse(ll$LATD < 0 , ll$minlat - latbin, ll$minlat)
##get corresponding catch data by cell
ll2 <- ll[ll$index == index[i],]
#areaid2 <- paste(stratwt, ll2$minlat, ll2$minlong)
##exclude catches from those areas not in region
areaid2 <- paste(ll2$minlat, ll2$minlong)
ll2 <- ll2[is.na(match(areaid2, areaidll))==F,]
##checks if any catch taken otherwise skips to next time step
if (dim(ll2)[[1]] == 0){next}
data2 <- data[data$index == index[i],]
##add some dumby data to cover all cells
#areaid3 <- areaid[is.na(match(substring(areaid,1,1),unique(data2$astrat)))==F]
areaid3 <- areaid
a <- unlist(strsplit(areaid3, " "))
b <- length(a)/3
longs <- as.numeric(a[(1:b)*3-1])
lats <- as.numeric(a[(1:b)*3-2])
prop <- as.numeric(a[(1:b)*3])
data3 <- data2[1:length(longs),]
data3$freq <- rep(0, length(data3$freq))
#data3$minlong <- longs
#data3$minlat <- lats
data3$minlong2 <- longs
data3$minlat2 <- lats
data3$region_prop <- prop
data2 <- rbind(data2, data3)
rm(data3)
ll3 <- ll2[1:length(longs),]
ll3$YFT_NO <- rep(0, length(ll3$YFT_NO))
ll3$minlong <- longs
ll3$minlat <- lats
ll2 <- rbind(ll2, ll3)
ll2$areabin <- paste(ll2$minlat, ll2$minlong)
data2$areabin <- paste(data2$minlat2, data2$minlong2)

catch <- tapply(ll2$YFT_NO, ll2$areabin, sum)/ sum(ll2$YFT_NO)
wt <- tapply(data2$freq, data2$areabin, sum)
##chose the most important cells for catch - those that account for about 70% of the catch
try <- cumsum(sort(catch, decreasing=T))
if(try[1] > 0.7) {try2 <- try[1]}
if(try[1] < 0.7) {try2 <- try[try < 0.7]}
##this is a check to see the difference between the last cell included
if(max(try2) < 0.6) {try2 <- try[1:(length(try2)+1)]}
catch2 <- names(try2)
wt2 <- wt[match(catch2, dimnames(wt)[[1]])]
## only generate weight frequency if more than 15 fish in each of the main cells fished and a minimum of 60 fish in these cells
wt3 <- wt2[wt2 > 15]
#plot all samples
b <- tapply(data2$freq, data2$wt, sum)
plot(as.numeric(names(b)), b, type="l", xlim=c(0,80), col="red", lwd=3)
mtext(side=3, index[i])
if (sum(wt2) > 60 & length(wt3) == length(catch2)){
data2$scale <- catch[match(data2$areabin,names(catch))]
##dumby data for each weight interval
a <- rep(1, length(wtstep))
data2 <- rbind(data2[a,], data2)
data2$freq[1:length(wtstep)] <- rep(0,length(wtstep))
data2$wt[1:length(wtstep)] <- wtstep
##get proportion at length by cell
a <- tapply(data2$freq, list(data2$wt, data2$areabin), sum)
a <- ifelse(is.na(a)==T, 0, a)
tot <- apply(a, 2, sum)
for (j in 1:length(tot)){
a[,j] <- a[,j]/tot[j]
}
a <- ifelse(is.na(a)==T, 0, a)
##scale by catch proportion in cell
##scale by proportion of areabin in region
prop <- tapply(data2$region_prop, data2$areabin, mean)
for (j in 1:length(catch)){
a[,j] <- a[,j] * catch[j] *prop[j]
}
a <- ifelse(is.na(a)==T, 0, a)
sumwt <- apply(a, 1, sum)
##get sample size based on the number of fish measured in the main cell
nsamples <- sum(wt3)
##all samples scaled to number of fish measured
sumwt <- round(sumwt*nsamples/sum(sumwt), 0)
par(new=T)
plot(as.numeric(names(sumwt)), sumwt, type="l", xlim=c(0,80), lwd=2, yaxt="n")
#output record
out <- cbind(index[i],matrix(sumwt, 1, 200))
write.table(out, filename, append=T,  quote = FALSE, sep = " ", eol = "\n", na = "NA", row.names = FALSE, col.names = FALSE)
rm(out)
}
}
}
