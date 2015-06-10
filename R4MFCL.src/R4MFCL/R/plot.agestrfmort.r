plot.agestrfmort.bet  <- function(plotrepfile=baserep,yr2plt=seq(1955,2005,by=10)){
# SJH 16/7/2011  - might have fixed a bug - plotting wrong Natage!!!
# Requires Pierre's library - too lazy to fix for now!!

#plot prop at age and average F for 10 year intervals starting at 1953

plotrep <- plotrepfile
#plotproportion at age by year
Nage <- getNya(plotrep)
yr <- getyear1(plotrep) + seq(nrow(Nage)) - 1
Nage2 <- apply(Nage, 1, sum)
Nage <- Nage/Nage2
#### This was missing before!!! - the next line ....
Nage <- Nage[match(yr2plt,yr),]

#browser()
ylim <- max(Nage, na.rm=T)
par(mfcol=c(length(yr2plt),2), mar=c(1,2,0,0), omi=c(0.4,0.5,0.1,0.7))
for( i in 1:length(yr2plt)){
barplot(Nage[i,], ylim=c(0,ylim), yaxt="n", xlim=c(0,ncol(Nage)))
axis(2,labels=F)
#text(25, 0.9*ylim, yr2plt[i])
legend("top",legend=yr2plt[i],bty="n")
}
axis(1)
axis(2,las=1)
#mtext(side=1, outer=T, "Age class", line=1.5)
mtext(side=2, outer=T, "Proportion at age", line=1)

#a <- getFya2(plotrep)
a <- getFya(plotrep)
ages <- ncol(a)
times <- getyear1(plotrep) + seq(nrow(a))/4 - 0.25
#times
a <- aggregate(a, list(floor(times)), mean)
a <- a[,-1]
yr <- sort(unique(floor(times)))
a <- a[match(yr2plt,yr),]
ylim <- max(a, na.rm=T)
for( i in 1:length(yr2plt)){
plot(1:ages, a[i,], type="l", lwd=2, col=1, ylim=c(0,ylim), xaxt="n", yaxt="n")
#axis(1, labels=F)
axis(4,labels=F)
legend("top",legend=yr2plt[i],bty="n")##critical age
#lines(rep(8,2), c(0,ylim), lty=2)
}
mtext(side=1, outer=T, "Age class (quarters)", line=1.5)
mtext(side=4, outer=T, "Fishing mortality", line=1)
axis(1)
axis(4,las=1)
}



############# all the 'get' shit that goes with this function

getNya <- function(plotrepfile = "plot.rep", byyear = TRUE) 
{
    apply(getNyar(plotrepfile, byyear = byyear), 1:2, sum)
}

getNyar <- function(plotrepfile = "plot.rep", byyear = TRUE) 
{
    nreg <- getnreg(plotrepfile)
    nages <- getnages(plotrepfile)
    npd <- getnpd(plotrepfile)
    dat <- getplotdat2("# Population [Nn]umber by age", "# Region", 
        plotrepfile)
    a <- array(dat, c(nages, npd, nreg))
    if (byyear) 
        a <- a[, seq(1, d <- dim(a)[2], d/getnyr(plotrepfile)), 
            , drop = FALSE]
    aperm(a, c(2, 1, 3))
}

getnreg <- function(plotrepfile = "plot.rep") 
{
    getplotdat1(plotrepfile, h = "# Number of regions")
}

getplotdat1 <- function(h = "", plotrepfile, skip = 1) 
{
    dat <- readLines(plotrepfile)
    recnum <- grep(h, dat)
    scanText(dat[recnum + skip], what = 0)
}

getnages  <- function(plotrepfile = "plot.rep") 
{
    getplotdat1(plotrepfile, h = "# Number of age classes")
}

getnpd <- function(plotrepfile = "plot.rep") 
{
    getplotdat1(plotrepfile, h = "# Number of time periods")
}

getplotdat2 <- function(h, k = "", plotrepfile) 
{
    dat <- readLines(plotrepfile)
    rec1 <- grep(h, dat)
    if (length(rec1) <= 0) 
        stop(paste("\"", h, "\"", "not found in", plotrepfile, 
            " Die yuppie scum!"))
    recnum <- rec1 + 1
    tt <- numeric(0)
    for (i in recnum:length(dat)) {
        if (regexpr(k, dat[recnum]) != -1) {
            recnum <- recnum + 1
            next
        }
        if (regexpr("^#", dat[recnum]) != -1) 
            break
        tt <- c(tt, scanText(dat[recnum], what = 0))
        recnum <- recnum + 1
    }
    tt
}

getFya <- function(plotrepfile = "plot.rep") 
{
    nages <- getnages(plotrepfile)
    dat <- getplotdat4("# Fishing mortality by .*down)$", plotrepfile)
    matrix(dat, byrow = TRUE, ncol = nages)
}

getplotdat4 <- function(h = "", plotrepfile) 
{
    dat <- readLines(plotrepfile)
    rec1 <- grep(h, dat)
    if (length(rec1) <= 0) 
        stop(paste("\"", h, "\"", "not found in", plotrepfile, 
            " Die yuppie scum!"))
    recnum <- rec1 + 1
    tt <- numeric(0)
    for (i in recnum:length(dat)) {
        if (regexpr("^#", dat[i]) != -1) 
            break
        tt <- c(tt, scanText(dat[i], what = 0))
    }
    tt
}

getyear1 <- function(plotrepfile = "plot.rep") 
{
    round(min(unlist(getrtimes(plotrepfile))))
}
getrtimes <- function(plotrepfile = "plot.rep") 
{
    dat <- getplotdat4("# Time of each realization by fishery", 
        plotrepfile)
    nreal <- getnreal(plotrepfile)
    nfish <- getnfish(plotrepfile)
    splitter <- rep(seq(nfish), nreal)
    split(dat, splitter)
}
getnreal <- function(plotrepfile = "plot.rep") 
{
    getplotdat1(plotrepfile, h = "# Number of realizations per fishery")
}
getnfish <- function(plotrepfile = "plot.rep") 
{
    getplotdat1(plotrepfile, h = "# Number of fisheries")
}

getnyr <- function(plotrepfile = "plot.rep") 
{
    1 + diff(floor(range(unlist(getrtimes(plotrepfile)))))
}