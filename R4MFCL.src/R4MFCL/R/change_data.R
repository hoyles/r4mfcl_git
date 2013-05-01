 change_data <-
function(obj,searchtext,xlines,newline) {
# By Simon D Hoyle 2008
  pos <- grep(searchtext,obj) + xlines
  obj[pos] <- paste("",newline,collapse="")
  return(obj)
  }
