 get.critical.age <-
function(data=Base.rep)
{
# calculates the age (and associated length, and weight)
# where the weight of a cohort is maximised

model <- data.frame(age=seq(1,data$nAges),length=data$mean.LatAge,
weight=data$mean.WatAge,M=data$MatAge,Numbers=rep(NA,data$nAges),Biomass=rep(NA,data$nAges))
# First age class
model$Numbers[1] <- 1000
for(i in 2:(data$nAges-1))
{
model$Numbers[i] <- model$Numbers[i-1]*exp(-model$M[i-1])
}


# While this is the more correct plus group calc - is stuff up the 'critical age selection'
#model$Numbers[data$nAges] <- (model$Numbers[data$nAges-1]*exp(-model$M[data$nAges-1]))/(1-exp(-model$M[data$nAges]))
model$Numbers[data$nAges] <- model$Numbers[data$nAges-1]*exp(-model$M[data$nAges-1])

model$Biomass <- model$Numbers*model$weight
output <- list(crit=model[rank(model$Biomass)==data$nAges,],yield.at.age=model$Biomass,rel.yield.at.age=model$Biomass/max(model$Biomass))
return(output)
}
