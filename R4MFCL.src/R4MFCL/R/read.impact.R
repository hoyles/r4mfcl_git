 read.impact <-
function(impdir = "H:\\rmfcl\\test\\",impnames = c("ll","psassoc","psunassoc","idph","other"))
{
for(i in 1:length(impnames))
{
data <- read.rep(paste(impdir,impnames[i],".rep",sep=""))
assign(impnames[i],data,pos=1)
}
}
