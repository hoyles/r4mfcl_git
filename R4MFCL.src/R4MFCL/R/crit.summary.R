 crit.summary <-
function(crit,years)
{
# Takes the output from do.critical.calcs and gets the key reference points
# SJH 22/06/09

mage <- mean(crit$overall.mean.age[match(years,dimnames(crit$overall.mean.age)[[1]])])
mlen <- mean(crit$overall.mean.length[match(years,dimnames(crit$overall.mean.length)[[1]])])
lost.yield <- crit$rel.yield.at.age[round(mage,0)]


resout <- list(
        Critage = crit$crit[1],
        Critlen = crit$crit[2],
        Mage = mage,
        Mlen = mlen,
        Lost.yield = 1-lost.yield)
return(resout)
}
