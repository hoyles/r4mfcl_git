 change.fishflag <-
function(a,fisheries,flagnum,newvals) {
  # by Simon Hoyle June 2008
  for (i in 1:length(fisheries)) {
    nv <- ifelse(length(newvals)>1,newvals[i],newvals)
    fi <- ifelse(length(fisheries)>1,fisheries[i],fisheries)
    mt <- paste(c(-fi," ",flagnum," -?[[:digit:]]+"),collapse="")
    loc <- grep(mt,a)                                             # find the row
    if (length(loc)>0) { for (j in 1:length(loc)) {
      rpl <- paste(c(" ",-fi,flagnum, nv),collapse=" ") # collapse into a vector
      a <- c(a[1:(loc[j]-1)],rpl,a[(loc[j]+1):length(a)])                 # replace it in the doitall file
      } }
    }
  return(a)
}
