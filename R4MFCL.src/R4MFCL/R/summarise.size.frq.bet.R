 summarise.size.frq.bet <-
function(frq1,fishery=5)
{
# takes two frq files and compares the length and weight data on an annual basis for the range of years
# just does the last 15 years at the moment

# get data
#frq1 <- yyy
data1 <- frq1$mat[frq1$mat[,4]==fishery,]
lens <- seq(from=10,length=95,by=2)
weis <- 1:200

# first create an objects for length and weight for each dataset - missing values are zeros
len.1 <- matrix(0,nrow=nrow(data1),ncol=97)
len.1[,1:2] <- data1[,1:2]
wei.1<- matrix(0,nrow=nrow(data1),ncol=202)
wei.1[,1:2] <- data1[,1:2]

# get length and size data
      for(i in 1:nrow(data1))
      {
          #Length data first
          if(data1[i,7]==-1)
          {
          # do nothing as already zeros
              if(data1[i,8]==-1)
              {
              # no weight data either
              }
              else
              {
              wei.1[i,3:202] <- data1[i,8:207]
              }
          }
          else
          {
          len.1[i,3:97] <- data1[i,7:101]
              if(data1[i,102]==-1)
              {
              # no weight data
              }
              else
              {
              wei.1[i,3:202] <- data1[i,102:301]
              }
          }
      }

# aggregate to the year
ann.len <- aggregate(len.1[,3:97],list(len.1[,1]),sum)
ann.wei <- aggregate(wei.1[,3:202],list(wei.1[,1]),sum)
ann.catch <- aggregate(data1[,5],list(data1[,1]),sum)
ann.effort <- aggregate(data1[,6],list(data1[,1]),sum)
ann.cpue <- ann.effort
ann.cpue[,2] <- ann.catch[,2]/ann.effort[,2]

# setup data.frame to take quantiles
out.len <- data.frame(year=ann.len[,1],p25=rep(NA,length=nrow(ann.len)),p50=rep(NA,length=nrow(ann.len)),p75=rep(NA,length=nrow(ann.len)))
out.wei <- data.frame(year=ann.len[,1],p25=rep(NA,length=nrow(ann.len)),p50=rep(NA,length=nrow(ann.len)),p75=rep(NA,length=nrow(ann.len)))
#browser()

    # get the quantiles of length and weight for each year
    for(i in 1:nrow(ann.len))
    {
        if(rowSums(ann.len[i,2:96])==0)
        {
        } else {
        out.len[i,2:4] <- quantile(rep(lens,ann.len[i,2:96]),probs=c(0.25,0.5,0.75))
        }

        if(rowSums(ann.wei[i,2:96])==0)
        {
        } else {
        out.wei[i,2:4] <- quantile(rep(weis,ann.wei[i,2:201]),probs=c(0.25,0.5,0.75))
        }
    } 
return(list(fish=fishery,len=out.len,wei=out.wei,catch=ann.catch,effort=ann.effort,cpue=ann.cpue))
}
