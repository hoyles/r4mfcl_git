 crit.fishery.summary <-
function(crit)
{
# Takes the output from do.critical.calcs and gets the key reference points
# SJH 22/06/09

mage <- apply(crit$fishery.mean.age,2,mean,na.rm=T)
mlen <- apply(crit$fishery.mean.length,2,mean,na.rm=T)
lost.yield <- crit$rel.yield.at.age[round(mage,0)]

resout <- list(
        Critage = crit$crit[1],
        Critlen = crit$crit[2],
        Mage = mage,
        Mlen = mlen,
        Lost.yield = 1-lost.yield)
return(resout)
}
