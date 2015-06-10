get.outcome.percentiles <- function(obj=grid.out,perc=c(50,5,95))
{
#browser()
# take a grid object and get percentiles
out.obj <- obj[,1:length(perc)]

    for(j in 1:length(perc))
    {
    out.obj[,j] <- apply(obj,1,quantile,probs=perc[j]/100)
    }

dimnames(out.obj)[[2]] <- paste(perc,"%",sep="")
return(out.obj)
}
