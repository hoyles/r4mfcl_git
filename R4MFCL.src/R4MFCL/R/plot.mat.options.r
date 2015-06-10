plot.mat.options <- function(repfiles=c(baseini,
baseini),modlab = c("Base","Base"))
{
# SJH 12/7/2011
# Plot Mat-at-age from some different runs
# Plots basecase last
# something different in read.ini that requires the it to be done within the function

    Mat <- list()
    for(i in 1:length(repfiles))
    {
        Mat[[i]] <- read.ini(repfiles[i])$mat
    }
    # get the maximum value across the runs
    maxy <- max(unlist(lapply(Mat,max)))


    # if only one series then just make a plot
    if(length(repfiles)==1)
    {
        plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab="Reproductive output", xlab="Age class", ylim=c(0,maxy),las=1)
        lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
    }
    else{
    plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab="Reproductive output", xlab="Age class", ylim=c(0,maxy),las=1)

    for(i in 2:length(repfiles))
    {
        lines(1:length(Mat[[i]]), Mat[[i]], lwd=2, col=i)
    }

    lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
    legend("topright", lwd=2, col=1:length(Mat), lty=c(1,1), legend=modlab,bty="n")
    }
}
