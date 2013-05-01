 regroup_fishery_grps.doitall <-
function(doitall,f,flag,newgrps) {
  # by Simon Hoyle June 2008
  # Takes all fisheries in f and changes the group they are in 
  a <- doitall
  for (i in 1:length(f)) {
    mt <- paste(-f[i],"[[:blank:]]+",flag,"[[:blank:]]+[[:digit:]]+",sep="")
    loc <- grep(mt,a)
    if (length(loc)>0) { for (j in 1:length(loc)) {
      newl <- paste(" ",-f[i],flag,newgrps[i],sep=" ")  
      a <- c(a[1:(loc[j]-1)],newl,a[(loc[j]+1):length(a)])
      }
    } }
  return(a)
  }
