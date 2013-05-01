 rename.fisheries.doitall <-
function(doitall,oldfs,newfs) {
  # by Simon Hoyle June 2008
  for (i in 1:length(oldfs)) {
    mt <- paste(c(-oldfs[i]," [[:digit:]]+ [[:digit:]]+"),collapse="")
    oldf_txt <- paste(-oldfs[i]," ",sep="")
    newf_txt <- paste(-newfs[i]," ",sep="")
    loc <- grep(mt,doitall)                                             # find the rows
    doitall[grep(mt,doitall)] <- gsub(oldf_txt,newf_txt,doitall[grep(mt,doitall)])
    }
  return(doitall)
  }
