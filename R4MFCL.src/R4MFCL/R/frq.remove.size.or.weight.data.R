 frq.remove.size.or.weight.data <-
function(data=test.data,exclude=exclude,size=T,weight=F)
{
# SJH 08/06/2009
# Provide a matrix (exclude) containing the following columns: Year | Month | week | fishery
# it will then remove the size and/or weight observations depending on the T/F flags

# match up records to modify
data.id <- paste(data[,1],data[,2],data[,3],data[,4],sep=".")
exclude.id <- paste(exclude[,1],exclude[,2],exclude[,3],exclude[,4],sep=".")
match.id <- match(exclude.id,data.id)

for(i in 1:nrow(exclude))
{
#data[match.id,]

    #If we are removing size data
    if(size==T)
    {
        #if already missing then do nothing
        if(data[match.id[i],7]==-1)
        {
        }
        #otherwise put in a -1 and move all the weight records and take more zeros on the end
        #it won't matter if there is no weight data as it will move the -1 to column 8
        else
        {
        data[match.id[i],7:301]  <- c(-1,data[match.id[i],102:301],rep(0,94))
        }
    }

    #If we are removing weight data
    if(weight==T)
    {
        #if already missing then do nothing
        if(data[match.id[i],8]==-1 | data[match.id[i],102]==-1)
        {
        }
        #otherwise put in a -1 and move all the weight records and take more zeros on the end
        #it won't matter if there is no weight data as it will move the -1 to column 8
        else
        {
            #If no length data then just put -1 in column 8 and set rest of row to zero
            if(data[match.id[i],7]==-1)
            {
            data[match.id[i],8:301]  <- c(-1,rep(0,293))
            }
            else
            {
            data[match.id[i],102:301]  <- c(-1,rep(0,199))
            }
        }

    }
}
return(data)
}
