 add.catch.frq <-
function(frq,filepath,fshries) {
# Find base directory sub-folder holding sensitivity option files
  sens.frq <- read.frq(filepath)
# Replace the catch column relating to the fisheries specified - while accounting for the possibility that the number of rows or order differ between the sensitivity runs
# This script assumes that the sensitivity *.frq will contain either: only those rows which are to be modified; or all rows to be modified;
#   Check length and match of rows to be modified
#     Source *.frq
  m <- frq$mat
  lm <- length(m[(m[,4] %in% fshries)==T,1])
  mfshstrat <- paste(m[,4],m[,1],m[,2],sep="_")
  m <- cbind(m,mfshstrat)
#     Sensitivity *.frq
  b <- sens.frq$mat
  lb <- length(b[(b[,4] %in% fshries)==T,1])
  bfshstrat <- paste(m[,4],m[,1],m[,2],sep="_")
  b <- cbind(b,bfshstrat)
#
  if(lm == lb) {  #Number of rows for specified fisheries has same length
    ms <- m[(m[,4] %in% fshries)==T,"mfshstrat"]
    bs <- b[(b[,4] %in% fshries)==T,"bfshstrat"]
    chk <- sort(ms)==sort(bs)
    if(unique(chk)){      #Rows match exactly: replace catches
      # Replace catches in all rows in frq.mat corresponding to PSfisheries
      m[(m[,4] %in% fshries)==T,5] <- b[(b[,4] %in% fshries)==T,5]
      } else {    # Else replace the values in the rows that match the sensitivity frq$mat
        tmp4 <- match(m[,"mfshstrat"],b[,"bfshstrat"],nomatch=0)
        m <- cbind(m,tmp4)
        tmp5 <- as.numeric(m[(m[,4] %in% fshries)==T,"tmp4"])   #Only access the corresponding rows in the sensitivity.frq for the specified fisheries
        m[(m[,4] %in% fshries)==T,5] <- b[tmp5,5]   # Exchange the catches
      }
    } else {      # Else replace the values in the rows that match the sensitivity frq$mat
    print("WARNING!!! add.catch.frq catch sensitivity *.frq file has different dimensions to base.frq")
    tmp4 <- match(b[,"bfshstrat"],m[,"mfshstrat"],nomatch=0)  # only identify those matching rows present in the sensitivity file
    m <- cbind(m,tmp4)
    tmp5 <- as.numeric(m[(m[,4] %in% fshries)==T,"tmp4"])   #Only access the corresponding rows in the sensitivity.frq for the specified fisheries
    m[(m[,4] %in% fshries)==T,5] <- b[tmp5,5]   # Exchange the catches
    }
# Replace the mat object into frq (excluding the mfshstrat and tmp4 columns added on during the above)
    frq$mat <- m[,c(1:(attributes(frq$mat)$dim[2]))]
#
  return(frq)
}
