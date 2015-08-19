get.fishery.data <- function(Data,fishery,avyr,navyr,ce,nyears,mnth,proj.col,proj.row,lastyr,frq.type,sclr,Endmn)
{
# Called from generate.frq
# Gets the data for the period for averaging

# just get the rows for those years
### 20 July 2011 -- made more robust for instances where there is only 1 quarter to project
#browser()
#if(fishery==137) browser()
favyrs <- seq(avyr,length=navyr)
tmp <- Data[Data[,4]==fishery,1:proj.col]
tmp <- tmp[tmp[,1] %in% favyrs,]
#browser()

#define matrix
mat <- matrix(NA,proj.row,proj.col)
#### Need to sort out the years right if we are starting mid-year.
#need to have an error message for missing data .....
if(Endmn){
mat[,1] <- sort(rep(seq(lastyr+1,lastyr+nyears,by=1),length(mnth)))
}else{
# Number of months in the final year of the model and final proj.yr to fill
fynum <- max(rank(mnth))-rank(mnth)[1]+1
lynum <- length(mnth)-fynum
#browser()

mat[,1] <- sort(c(rep(lastyr+1,fynum),rep(lastyr+nyears+1,lynum),
rep(seq(lastyr+2,(lastyr+nyears),by=1),length(mnth))))
}

mat[,2] <- rep(mnth,nyears)
#mat[,3] <- rep(1,nyears*length(mnth))
mat[,3] <- 1
mat[,4] <- rep(fishery,nyears*length(mnth))

    # if we have temporal effdevs - 1 for fish with them and -1 for without
    if(frq.type==6)
    {
        # Needed if only one quarter is to be projected!!!
        if(is.null(nrow(tmp)))
        {
        mat[,7] <- ifelse(max(tmp[7])>0,1,-1)
        }else{
        mat[,7] <- ifelse(max(tmp[,7])>0,1,-1)
        }
    }
    # will either have len or wei data
    else{
    mat[,7] <- -1
    }
    #if we have frq.type=6 and len and wei
    if(proj.col>7)  mat[,8:proj.col] <- -1


# SORT OUT THE CATCH AND EFFORT
    #generalize so that it does catch or effort - whichever is chosen
    ceave <- rep(NA,length=length(mnth))
        for(j in 1:length(mnth))
        {
                    if(is.null(nrow(tmp)))
                    {
                    ceave[j] <- mean(tmp[4+ce][tmp[2]==mnth[j]&tmp[4+ce]!=-1])
                    }else{
                    ceave[j] <- mean(tmp[,4+ce][tmp[,2]==mnth[j]&tmp[,4+ce]!=-1])
                    }
        }
        # put in the catch or effort
        if(ce==1)
        {
        mat[,5] <- rep(ceave,nyears) *sclr
        mat[,6] <- -1
        }else
        {
        mat[,5] <- -1
        mat[,6] <- rep(ceave,nyears) *sclr
        }
#if(fishery==1) browser()
#remove any missing values at the end
mat <- mat[!is.na(mat[,4+ce]),]
return(mat)
}





