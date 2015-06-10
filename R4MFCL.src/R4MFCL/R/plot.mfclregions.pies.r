plot.mfclregions.pies <- function(lims=c(100,260,-40,45),add.reg=T,spp="betyft") {
# need these libraries
#library(maps)
#library(mapproj)
#library(mapdata)
# Plots the MFCL regions assummed in the 2009 stock assessments
# By Adam Langley
# Modified by Simon Hoyle
eez <- read.table("I:/assessments/Pop dy modeling/MFCL/R functions/EZNEW2.TXT")
plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col="lightblue")
polygon(eez[,1], eez[,2], lwd=1, col="white")
lines(eez[,1], eez[,2], lwd=1, col="slate grey")
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands"), fill=T, add=T, yaxt="n", xaxt="n", col="grey")
box(lwd=3)


if(add.reg) #to add MFCL regions to the plot
{
    if(spp=="betyft")
    {
        abline(v=210,lty=1,lwd=2)
        abline(v=170,lty=1,lwd=2)
        lines(c(110,210),c(20,20),lty=1,lwd=2)
        lines(c(110,210),c(-10,-10),lty=1,lwd=2)
        lines(c(110,110),c(-10,20),lty=1,lwd=2)
        lines(c(140,140),c(-50,20),lty=1,lwd=2)
        lines(c(140,155),c(0,0),lty=1,lwd=2);lines(c(155,155),c(0,-5),lty=1,lwd=2);lines(c(155,160),c(-5,-5),lty=1,lwd=2);lines(c(160,160),c(-5,-10),lty=1,lwd=2)
        lines(c(140,150),c(-15,-15),lty=1,lwd=2);lines(c(150,150),c(-15,-20),lty=1,lwd=2);lines(c(150,140),c(-20,-20),lty=1,lwd=2)
        text(c(c(135,145,143),rep(160,3),rep(190,3)),c(15,-17.5,-5,33,10,-17.5,33,10,-17.5),labels=as.character(c(7,9,8,1,3,5,2,4,6)),cex=1.2,col="red")
     }

    if(spp=="skj")
    {
        lines(c(210,210),c(-20,50),lty=1,lwd=2)
        lines(c(170,170),c(-20,50),lty=1,lwd=2)
        lines(c(110,210),c(20,20),lty=1,lwd=2)
        lines(c(110,210),c(-20,-20),lty=1,lwd=2)
        lines(c(110,110),c(-20,20),lty=1,lwd=2)
        lines(c(210,210),c(-20,50),lty=1,lwd=2)
        lines(c(120,120),c(20,50),lty=1,lwd=2)
        lines(c(140,140),c(-20,20),lty=1,lwd=2)
        lines(c(140,155),c(0,0),lty=1,lwd=2);lines(c(155,155),c(0,-5),lty=1,lwd=2);lines(c(155,160),c(-5,-5),lty=1,lwd=2);lines(c(160,160),c(-5,-20),lty=1,lwd=2)
        text(c(135,143,155,155,200),c(15,-5,15,35,15),labels=as.character(c(4,5,2,1,3)),cex=1.2,col="red")
     }
}

axis(1, at=seq(lims[1],lims[2],by=10), labels=F)
axis(2, at=seq(lims[3],lims[4],by=5), labels=F)
latseq <- seq(lims[3]+10,lims[4]-10,by=10) ;latseq2 <- as.character(latseq)
lonseq <- seq(lims[1]+20,lims[2]-20,by=20) ;lonseq2 <- as.character(lonseq)
latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180],"E",sep="")
lonseq2[lonseq > 180] <- paste(360-lonseq[lonseq > 180],"W",sep="")
axis(2, at=latseq, labels=latseq2, cex.axis=0.75)
axis(1, at=lonseq, labels=lonseq2, cex.axis=0.75)
#mtext(side=3, line=0.5, plot_title)
}
