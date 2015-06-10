plot.m.options <- function(repfiles=list(read.rep(baserep),
read.rep(baserep)),modlab = c("Base","Base"))
{
# SJH 12/7/2011
# Plot M-at-age from some different runs
# Plots basecase last

    M <- list()

    for(i in 1:length(repfiles))
    {
        M[[i]] <- repfiles[[i]]$MatAge
    }

    # get the maximum value across the runs
    maxy <- max(unlist(lapply(M,max)))

    # if only one series then just make a plot
    if(length(repfiles)==1)
    {
        plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
        lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
    }
    else{
        plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)

        for(i in 2:length(repfiles))
        {
            lines(1:length(M[[i]]), M[[i]], lwd=2, col=i)
        }

        lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
        legend("topright", lwd=2, col=1:length(M), lty=c(1,1), legend=modlab,bty="n")
    }
}
