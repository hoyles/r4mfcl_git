 change.size.frq <-
function(ver=6,data=data,FISH=1,
# By Shelton Harley 2009

LF.FILE="P:\\yft\\2009\\Data Preparation\\size data\\LLlendataR1.txt",
WT.FILE="P:\\yft\\2009\\Data Preparation\\size data\\LLwtdataR1.txt")
{
#  modified 30/06/09 to be able to handle ver 4 and ver 6 frq files

# then do the matching
fish <- FISH
fishery2 <- data[data[,4] == fish,]
fishindex <- paste(fishery2[,1], round(fishery2[,2]/4,0) + 1)
lf <- read.table(LF.FILE)
lfindex <- paste(lf[,1], lf[,2])
a <- match(fishindex,lfindex)
wt <- read.table(WT.FILE)
wtindex <- paste(wt[,1], wt[,2])
b <- match(fishindex,wtindex)

# lets define the various columns that might be needed depending on the frq file
# frq4
frq4 <- c(7,301,8,207,7,101,102,301)
frq6 <- frq4+1

if(ver>=6) {labs <- frq6 } else {labs<-frq4 }



# lets wipe out all existing data first
#fishery2[,7:301] <- 0
fishery2[,labs[1]:labs[2]] <- 0

for(i in 1:nrow(fishery2))
{
    # No length data
    if(is.na(a[i])==T)
    {
#    fishery2[i,7] <- -1
    fishery2[i,labs[1]] <- -1

        # No weight data either
        if(is.na(b[i])==T)
        {
#        fishery2[i,8] <- -1
        fishery2[i,labs[3]] <- -1
        }
        else
        {
        # No length, but some weight data
        #fishery2[i,8:207] <- as.vector(unlist(wt[b[i],3:202]))
        fishery2[i,labs[3]:labs[4]] <- as.vector(unlist(wt[b[i],3:202]))
        }
    }

    else
    {
    # Length data
#    fishery2[i,7:101] <- as.vector(unlist(lf[a[i],3:97]))
    fishery2[i,labs[5]:labs[6]] <- as.vector(unlist(lf[a[i],3:97]))

        # Length, but no weight
        if(is.na(b[i])==T)
        {
#        fishery2[i,102] <- -1
        fishery2[i,labs[7]] <- -1
        }
        else
        {
        # No length, but some weight data
        fishery2[i,labs[7]:labs[8]] <- as.vector(unlist(wt[b[i],3:202]))
        }
     }
}
return(fishery2)
}
