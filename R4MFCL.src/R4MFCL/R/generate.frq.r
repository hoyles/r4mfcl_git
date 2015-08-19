generate.frq <- function(frq=BET$frq,nyears=10,avyrs=fshry.avyrs.ptr,navyrs=fshry.nyrs.ptr,caef=fshry.caef.ptr,scalar=fshry.caef.sclr)
{
# 16 September 2010
# Building on Nick's work - a function that sets up a frq file for a projection
# Need to account for:
    # final year of model
    # frq file type
    # whether both size and weight data are used
# assign the data part of the frq file

# 10/26/2011 3:34:56 PM - fixing to allow to work for SP-ALB
# 20/08/2015 9:04:29 AM - RDS - code cleaning, no changes to results

Data <- frq$mat

# check that the lengths of avyrs and caef are the same as the number of fisheries
nfish <- frq$struct$nf
if(length(avyrs)!=nfish) stop("ERROR: NUMBER OF FISHERIES NOT THE SAME AS avyrs")
if(length(caef)!=nfish) stop("ERROR: NUMBER OF FISHERIES NOT THE SAME AS caef")

#####  A kludge for the fact that the SP-ALB model ends mid-year
# If the data ends before the end of the year - we need to start the projection also from the middle of the year!
x <- Data[Data[,1]==max(Data[,1]),1:2]
lastmn <- max(x[,2])
allmnths <- sort(unique(Data[,2]))
#browser()
if(lastmn==max(allmnths)){
  lastyr <- max(Data[,1])
  mnth <- allmnths
  endmn <- T
}
else{
  lastyr <- max(Data[,1])-1
  #ind <- match(lastmn,allmnths)
  #mnth <- allmnths[c(((ind+1):length(allmnths)),(1:(ind)))]
  mnth <- c(7:12,1:6)
  endmn <- F
}

#type of frq file
frq.type <- frq$struct$te
# length data
len <- ifelse(frq$dl$lfint>0,1,0)
#weight data
wei <- ifelse(frq$dl$wfint>0,1,0)
#
#determine dimensions of projection data for each fishery
#YEAR|MONTH|WEEK|FISHERY|CATCH|EFFORT
proj.row <- nyears*length(mnth)
proj.col <- 6+ifelse(frq.type==6,1,0)+len+wei

# Loop through fisheries to get projection data
projfrq <- NULL

for(i in 1:nfish){

   if(!is.na(avyrs[i])){
     #do nothing for fisheries with avyrs==NA as it is not included in the projections
     projfrq <- get.fishery.data(Data=Data,fishery=i,avyr=avyrs[i],navyr=navyrs[i],ce=caef[i],nyears,mnth,proj.col,proj.row,lastyr,frq.type,sclr=scalar[i],Endmn=endmn)
   }
}

#Now tack on the extra zeros to the rows to make sure it all lines up properly
jnk <- matrix(0,nrow(Data)+nrow(projfrq),ncol(Data))
jnk[1:nrow(Data),] <- Data
jnk[(nrow(Data)+1):nrow(jnk),1:ncol(projfrq)] <- projfrq

frq$mat <- jnk

#now fix the rest of the frq file
frq$dflags[2,] <- as.numeric(paste(rep(lastyr+1,nfish),collapse=NULL)) # Replace second row of zeroes with starting year of projections
frq$dflags[3,] <- as.numeric(paste(rep(mnth[1],nfish),collapse=NULL))       # Replace third row with the first month of the projection period; = 2 (Feb) for BET, YFT and SKJ models
frq$dl$dsets <-nrow(jnk)

return(frq)
}
