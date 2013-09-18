create.missing.ce <- function(data=data, yr=2008, termfish){
# By Shelton J Harley
# modified 11/06/09 - it fell over when only 1 quarter of data was available!!
# Updated 26/06/10 to have flexibility in the yr
# By Nick Davies 25 June 2011
# included option for terminated fisheries (not present in the reference year)

quart <- c(2,5,8,11)
output <- c()
fshrys <- unique(data[,4])
if(!missing(termfish)) fshrys <- fshrys[!fshrys %in% termfish]
for(j in fshrys){
d <- data[data[,4]==j & data[,1]==yr,]
miss.len <- 4-ifelse(is.null(nrow(d)),1,nrow(d))

  # all there  - do nothing
  if(miss.len==0){ }
  # something missing
  else
  {
      # all missing
      if(miss.len==4)
      {
      out <- cbind(rep(yr,4),c(2,5,8,11),rep(1,4),rep(j,4))
      }
      #some missing
      else
      {
          if(miss.len==3)
          {
          mquarters <- quart[-1*match(d[2],quart)]
          out <- cbind(rep(yr,miss.len),mquarters,rep(1,miss.len),rep(j,miss.len))
          }
          else
          {
          mquarters <- quart[-1*match(d[,2],quart)]
          out <- cbind(rep(yr,miss.len),mquarters,rep(1,miss.len),rep(j,miss.len))
          }
      }
  output <- rbind(output,out)
  }
#print(j)
}
return(output)
}
