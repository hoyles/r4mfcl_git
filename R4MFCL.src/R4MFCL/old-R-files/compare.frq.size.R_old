 compare.frq.size <-
function(filename,fisheries=c(1,2,4,7,10,12)) {
# SDH 17/6/2009 using mostly Adam Langley's code. 

##get the actual sample size; i.e, number of fish in length and weight samples

#filename <- paste(path, "bet.frq", sep="")
#filename <- paste(path, "bet2.frq", sep="")
#filename <- paste(path, "bet3.frq", sep="")

# DEFINITION OF FISHERIES
# Fishery 1   Longline All                               [region 1]
# Fishery 2   Longline All except HW                     [region 2]
# Fishery 3   Longline HW                                [region 2]
# Fishery 4   Longline All except CH, TW-off, PG - ex PNG waters        [region 3]
# Fishery 5   Longline CH, TW-off                        [region 3]
# Fishery 6   Longline PG                                [region 3]
# Fishery 7   Longline All except CH, TW-off, HW         [region 4]
# Fishery 8   Longline CH, TW-off                        [region 4]
# Fishery 9   Longline HW                                [region 4]
# Fishery 10  Longline All except AU                     [region 5]
# Fishery 11  Longline AU                                [region 5]
# Fishery 12  Longline JP, KR, TW-dw, CH-dw              [region 6]
# Fishery 13  Longline Pac. Is. domestic                 [region 6]
# Fishery 14  PS log/FAD sets, exclude AFADs             [region 3]
# Fishery 15  PS school + miscell. sets                  [region 3]
# Fishery 16  PS log/FAD sets                            [region 4]
# Fishery 17  PS school + miscell. sets                  [region 4]
# Fishery 18  GN, HL-small, PL, RN, TR, Unclass. PH      [region 3]
# Fishery 19  HL-large PH & ID                           [region 3]
# Fishery 20  PS JP coastal                              [region 1]
# Fishery 21  PL JP coastal                              [region 1]
# Fishery 22  PL All, except ID                          [region 3]
# Fishery 23  Longline All except CH, TW-off, PG - PNG waters       [region 3]
# Fishery 24  GN, HL-small, PL, RN, TR, Unclass. ID      [region 3]
# Fishery 25  HL HW                                      [region 4]

obj <- readLines(filename)
line1 <- grep("# Datasets /", obj)
intdat <- as.numeric(unlist(strsplit(obj[line1 +1], split=" +"))[-1])

a <- count.fields(filename, skip=line1)
## as per frq file
lfint <- intdat[2]
lfirst <- intdat[3]
lint <- intdat[4]

#key fisheries for including in iterative reweighting - JP LL
#fish1 <- c(1,2,4,7,10,12)
#years <- 1952:2007
years2 <- sort(unique(trunc(years/10) * 10))
strat <- levels(interaction(fisheries, years2))

##summary matrix
frqsumm <- matrix(NA, length(strat), 6)
a <- unlist(strsplit(strat, split="\\."))
#fishery
frqsumm[,1] <- as.numeric(a[1:(length(a)/2) * 2 -1])
#decade
frqsumm[,2] <- as.numeric(a[1:(length(a)/2) * 2])
index <- paste(frqsumm[,1], frqsumm[,2])

##max samp from MFCL
maxsamp <- 1000

##get weight data from frq
line1 <- grep("# Datasets /", readLines(filename))
a <- count.fields(filename, skip=line1+1)
b <- c(0,cumsum(a))
dat <- scan(filename, skip=line1+1, comment.char="#")


## as per frq file
wtint <- intdat[6]
wfirst <- intdat[7]
wint <- intdat[8]

mat <- matrix(0, length(a), wtint+4)
for (i in 1:length(a)){
#no size data
if(a[i] == 8){next()}
#only length data
if(a[i] == lfint+7){next()}
#only weight data
if(a[i] == wtint+7){
  mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
  mat[i,5:(wtint+4)] <- dat[(b[i]+8):(b[i]+7+wtint)]
  }
#lf & weight data
if(a[i] == lfint + wtint+6){
  mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
  mat[i,5:(wtint+4)] <- dat[(b[i]+8+lfint-1):(b[i]+7+wtint+lfint-1)]
  }
}

#no weight data
sumwt <- apply(mat[,5:(wtint+4)], 1, sum)
mat <- mat[sumwt > 0,]
rm(sumwt)
alb <- as.data.frame(mat)
#select fisheries
alb <- alb[is.na(match(alb[,4],fisheries)) ==F,]

##fishery, decade index
index2 <- paste(alb[,4], trunc(alb[,1]/10) * 10)
##number of wt records per fishery and decade
a <- tapply(alb[,4], index2 ,length)
frqsumm[match(names(a), index),5] <- a
## number per sample
a <- apply(alb[,-(1:4)], 1, sum)
## maximum = maxsamp
a <- ifelse(a >= maxsamp, maxsamp, a)
##mean per fishery/decade
wtmean <- tapply(a, index2 ,mean)
frqsumm[match(names(wtmean), index),6] <- wtmean


##get length data from frq
line1 <- grep("# Datasets /", readLines(filename))
a <- count.fields(filename, skip=line1+1)
b <- c(0,cumsum(a))
dat <- scan(filename, skip=line1+1, comment.char="#")
## as per frq file
lfint <- 95
lfirst <- 10
lint <- 2

mat <- matrix(0, length(a), lfint+4)
for (i in 1:length(a)){
#no size data
if(a[i] == 8){next()}
#only weight data
if(a[i] == wtint+7){next()}
# length data only or lf & weight data
if(a[i] == lfint+7 | a[i] == lfint + wtint+6){
  mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
  mat[i,5:(lfint+4)] <- dat[(b[i]+7):(b[i]+6+lfint)]
  }
}

#no length data
sumlf <- apply(mat[,5:(lfint+4)], 1, sum)
mat <- mat[sumlf > 0,]
rm(sumlf)

alb <- as.data.frame(mat)
#select fisheries
alb <- alb[is.na(match(alb[,4],fisheries)) ==F,]

##fishery, decade index
index2 <- paste(alb[,4], trunc(alb[,1]/10) * 10)
##number of lf records per fishery
a <- tapply(alb[,4], index2 ,length)
frqsumm[match(names(a), index),3] <- a
## number per sample
a <- apply(alb[,-(1:4)], 1, sum)
## maximum = maxsamp
a <- ifelse(a >= maxsamp, maxsamp, a)
##mean per fishery
lfmean <- tapply(a, index2 ,mean)
frqsumm[match(names(lfmean), index),4] <- lfmean
return(frqsumm)
}
