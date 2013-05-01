 rename.fishery.grps.doitall <-
function(doitall,oldfs,newfs,flag,keep=T,newgrps=c(0)) {
  # by Simon Hoyle June 2008
  # Takes all fishery refs in oldfs with flag 'flag', and changes the fishery to newfs
  a <- doitall
  for (i in 1:length(oldfs)) {
    mt <- paste(-oldfs[i],"[[:blank:]]+",flag,"[[:blank:]]+[[:digit:]]+",sep="")
    loc <- grep(mt,a)
    if (length(loc)>0) { for (j in 1:length(loc)) {
      unl <- unlist(strsplit(a[loc[j]],"[[:blank:]]+"))
      oldval <- ifelse(unl[1]=="",unl[4],unl[3])
      newl <- paste(" ",-newfs[i],flag,ifelse(keep,oldval,newgrps[i]),sep=" ")              # collapse into a vector
      a <- c(a[1:(loc[j]-1)],newl,a[(loc[j]+1):length(a)])
      }
    } }
  return(a)
  }
