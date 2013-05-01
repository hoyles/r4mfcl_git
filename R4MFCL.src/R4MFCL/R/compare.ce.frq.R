 compare.ce.frq <-
function(file1,file2,fm,plotname,fdesc="") 
{
# By Adam Langley 
# Change 9-Jun-09 SDH added option to read in fishery description - see example below
#fdesc <- data.frame(num=1:30,
#  method=c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L"),
#  flag=c("JP","KR","TWDW","AU","NC","FJ","OT","JP","KR","TWDW","AS/WS","TO","PF","OT","JP","KR","TWDW","AU","NZ","OT","JP","KR","TWDW","OT","ALL","ALL","ALL","ALL","ALL","ALL"),
#  reg=c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
# updated SJH 30/06/09 to use read.frq to allow for new frq file
##first file

mat <- read.frq(file1)$mat

#sk <- grep("# Datasets",readLines(file1,n=100)) + 2
#a <- count.fields(file1, skip=sk)
#b <- c(0,cumsum(a))
#dat <- scan(file1, skip=sk, comment.char="#")

fdesc <- as.matrix(fdesc)
#mat <- matrix(0, length(a), max(a))
#for (i in 1:length(a)){
#if(a[i] == 7)
#  mat[i,1:a[i]] <- dat[(b[i]+1):b[i+1]]
#}
#mat <- mat[,1:6]
#mat <- as.data.frame(mat)
mattstep <- mat[,1] + mat[,2]/12

##second file
mat2 <- read.frq(file2)$mat

#sk <- grep("# Datasets",readLines(file2,n=100)) + 2
#a <- count.fields(file2, skip=sk)
#b <- c(0,cumsum(a))
#dat <- scan(file2, skip=sk, comment.char="#")

#mat2 <- matrix(0, length(a), max(a))
#for (i in 1:length(a)){
##if(a[i] == 7)
#  mat2[i,1:a[i]] <- dat[(b[i]+1):b[i+1]]
#}
#mat2 <- mat2[,1:6]
#mat2 <- as.data.frame(mat2)

mat2tstep <- mat2[,1] + mat2[,2]/12

##fisheries
fish <- sort(unique(mat2[,4]))

for(i in 1:dim(fm)[2]){
#i <- 1
par(mfrow=c(3,1), mar=c(2,4,2,2))
##catch
ymax <- max(mat[,5][mat[,4] == fish[fm[1,i]]], mat2[,5][mat2[,4] == fish[fm[2,i]]])
plot(mattstep[mat[,4] == fish[fm[1,i]]], mat[,5][mat[,4] == fish[fm[1,i]]], type="l", ylim=c(-1,ymax), xlim=c(1952, 2007), ylab="Catch", xlab="")
lines(mat2tstep[mat2[,4] == fish[fm[2,i]]], mat2[,5][mat2[,4] == fish[fm[2,i]]], lty=1, col=2)
ti <- paste("Fishery", i)
#browser()
if (dim(fdesc)[1]>1) { ti <- paste(c("Fishery", i, "-",fdesc[i,2:4]),collapse=" ") }
mtext(side=3, line=0.5, ti)
##effort
ymax <- max(mat[,6][mat[,4] == fish[fm[1,i]]], mat2[,6][mat2[,4] == fish[fm[2,i]]])
plot(mattstep[mat[,4] == fish[fm[1,i]]], mat[,6][mat[,4] == fish[fm[1,i]]], type="l", ylim=c(-1,ymax), xlim=c(1952, 2007), ylab="Effort", xlab="")
lines(mat2tstep[mat2[,4] == fish[fm[2,i]]], mat2[,6][mat2[,4] == fish[fm[2,i]]], lty=1, col=2)
##CPUE
cpue1 <- mat[,5][mat[,4] == fish[fm[1,i]]]/mat[,6][mat[,4] == fish[fm[1,i]]]
cpue2 <- mat2[,5][mat2[,4] == fish[fm[2,i]]]/mat2[,6][mat2[,4] == fish[fm[2,i]]]
cpue1[is.na(cpue1)] <- 0; cpue2[is.na(cpue2)] <- 0
plot(mattstep[mat[,4] == fish[fm[1,i]]], cpue1, type="l", ylim=c(0,max (cpue1, cpue2)), xlim=c(1952, 2007), ylab="CPUE", xlab="")
lines(mat2tstep[mat2[,4] == fish[fm[2,i]]], cpue2, lty=1, col=2)
savePlot(paste(c(plotname,"fishery",i),collapse=" "),type="png")
}
}
