 timesplit.doitall <-
function(doitall,fishsplit,qsplit=T) {
  # by Simon Hoyle June 2008
  flags <- fishsplit[,4]
  fishsplit <- matrix(as.numeric(fishsplit[,1:3]),ncol=3)
  a <- doitall
  oldfish <- unique(fishsplit[,1])
  oldvals24 <- rep(0,length(oldfish))
  oldvals60 <- rep(0,length(oldfish))
  for (i in 1:length(oldfish)) {              # get all the old values
    mt24 <- paste(c(-oldfish[i],"[[:blank:]]+24[[:blank:]]+[[:digit:]]+"),collapse="") # locate the flag
    mt60 <- paste(c(-oldfish[i],"[[:blank:]]+60[[:blank:]]+[[:digit:]]+"),collapse="") # locate the flag
    loc24 <- grep(mt24,a) ; loc60 <- grep(mt60,a)
    if (length(loc24)>=1) { for (j in 1:length(loc24)) {
      unl <- unlist(strsplit(a[loc24[j]],"[[:blank:]]+"))
      oldvals24[i] <- as.numeric(unl[min(c(4,length(unl)))])
      } }
    if (length(loc60)>=1) { for (j in 1:length(loc60)) {
      unl <- unlist(strsplit(a[loc60[j]],"[[:blank:]]+"))
      oldvals60[i] <- as.numeric(unl[min(c(4,length(unl)))])
      } }
    }
  newvals24 <- oldvals24[fishsplit[,1]]
  for (i in 2:length(fishsplit[,2])) {                # work out what the new values should be
    if (fishsplit[i,1]==fishsplit[i-1,1])  {          # if not base, calculate orig_diff & nlast_split, reset countsplit
      newvals24[i] <- newvals24[i-1]+0.01
    }
  }
  y <- unique(newvals24) ; x <- rank(y) ; newvals24 <- x[match(newvals24,y)]
  newvals60 <- oldvals60[fishsplit[,1]]
  for (i in 2:length(fishsplit[,2])) {                # work out what the new values should be
     if(flags[i]!="JP" & fishsplit[i,1]==fishsplit[i-1,1])  {          # if base, calculate orig_diff & nlast_split, reset countsplit
      if(qsplit) {
        newvals60[i] <- newvals60[i-1]+0.01
        } else {
        newvals60[i] <- newvals60[i-1]
        }
      }
    }
  y <- unique(newvals60) ; x <- rank(y) ;
#  browser()
  newvals60 <- x[match(newvals60,y)]
#  browser()
  for (i in length(oldfish):1) {
    oldf <- oldfish[i]
    newf <- fishsplit[fishsplit[,1]==oldf,2]
    nfish <- length(newf)
    # if there's only one fishery then just change the flag
    # if there's more than 1, add a matching flag for each one
    for (g in c(49,34,16,29,10)) {                        # 34 tag rr, 16 non-dec sel, 29+60 q grps, 10 q timeseries
      if(nfish==1) { a<-rename.fishery.grps.doitall(a,oldf,newf,g,keep=T) }
      if(nfish>1) {
        mt <- paste(c(-oldf,"[[:blank:]]+",g," [[:digit:]]+"),collapse="") # locate the flag
        loc <- grep(mt,a)
        if (length(loc)>=1) { for (j in 1:length(loc)) {
          unl <- unlist(strsplit(a[loc[j]],"[[:blank:]]+"))
          oldval <- unl[min(c(4,length(unl)))]
          newl <- paste(" ",-newf[1],g,oldval,sep=" ")              # collapse into a vector
          a <- c(a[1:(loc[j]-1)],newl,a[(loc[j]+1):length(a)])                 # add it to the doitall file
                                   # new line for each new fishery with same flag
          for (k in 2:length(newf)) {
            newl <- paste(" ",-newf[k],g,oldval,sep=" ")                  # collapse into a vector
            a <- c(a[1:(loc[j]+k-2)],newl,a[(loc[j]+k-1):length(a)])                 # add it to the doitall file
            }
          }
        } } }
    for (g in c(24)) {                        # for 24, individual selecitivites so new value for new fishery
      if(nfish==1) {
        a<-rename.fishery.grps.doitall(a,oldf,newf,g,keep=F)
        a<-change.fishflag(a,newf,24,newvals24[newf])
      }
      if(nfish>1) {
        # change initial fishery
        mt <- paste(c(-oldf,"[[:blank:]]+",g," [[:digit:]]+"),collapse="") # locate the flag
        loc <- grep(mt,a)
        # for all occurrences of the fishery/flag
        if (length(loc)>=1) { for (j in 1:length(loc)) {
          unl <- unlist(strsplit(a[loc[j]],"[[:blank:]]+"))
          oldval <- unl[min(c(4,length(unl)))]
          newl <- paste(" ",-newf[1],g,newvals24[newf[1]],sep=" ")              # collapse into a vector
          a <- c(a[1:(loc[j]-1)],newl,a[(loc[j]+1):length(a)])           # add it to the doitall file
                                   # new line for each new fishery with same flag
          mt <- paste(c(-oldf,"[[:blank:]]+",g," [[:digit:]]+"),collapse="") # locate the flag
          for (k in 2:length(newf)) {
            newl <- paste(" ",-newf[k],g,newvals24[newf[k]],sep=" ")                  # collapse into a vector
            a <- c(a[1:(loc[j]+k-2)],newl,a[(loc[j]+k-1):length(a)])                 # add it to the doitall file
            } }
          }
      } }
      for (g in c(60)) {                        # for 60, individual catchabilities so new value for new fishery, unless qsplit==F
        if(nfish==1) {
          a<-rename.fishery.grps.doitall(a,oldf,newf,g,keep=F)
          a<-change.fishflag(a,newf,60,newvals60[newf])
        }
        if(nfish>1) {
          # change initial fishery
          mt <- paste(c(-oldf,"[[:blank:]]+",g," [[:digit:]]+"),collapse="") # locate the flag
          loc <- grep(mt,a)
          # for all occurrences of the fishery/flag
          if (length(loc)>=1) { for (j in 1:length(loc)) {
            unl <- unlist(strsplit(a[loc[j]],"[[:blank:]]+"))
            oldval <- unl[min(c(4,length(unl)))]
            newl <- paste(" ",-newf[1],g,newvals60[newf[1]],sep=" ")              # collapse into a vector
            a <- c(a[1:(loc[j]-1)],newl,a[(loc[j]+1):length(a)])           # add it to the doitall file
                                     # new line for each new fishery with same flag
            mt <- paste(c(-oldf,"[[:blank:]]+",g," [[:digit:]]+"),collapse="") # locate the flag
            for (k in 2:length(newf)) {
              newl <- paste(" ",-newf[k],g,newvals60[newf[k]],sep=" ")                  # collapse into a vector
              a <- c(a[1:(loc[j]+k-2)],newl,a[(loc[j]+k-1):length(a)])                 # add it to the doitall file
              } }
            }
        } }
      }
  return(a)
  }
