get.subset.outcomes <- function(obj,mods,vars,rnd=NULL)
{
#SJH 7/3/2014 9:47:18 PM
# subsets the big outcomes object

if(mods[1]=="ALL") mods <- 1:ncol(obj)
if(vars[1]=="ALL") vars <- 1:nrow(obj)

new.obj <- obj[vars,mods]

  if(is.null(rnd))
  {
  }else{

      if(length(rnd)!=length(vars)) stop("Length of rounding different than length of variables")

      for(i in 1:nrow(new.obj)){
      new.obj[i,] <- round(new.obj[i,],rnd[i])
      }

  }
#dimnames(out.round)[[2]] <- runs

return(new.obj)
}
