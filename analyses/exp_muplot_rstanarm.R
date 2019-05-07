## Started 3 April 2019 ##
## By Cat - based off code from Lizzie's OSPREE plots ##

# Runs from models_stan_plotting.R #
    
# with COLORS for each species #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
spnum <- length(unique(chill.stan$species))
pdf(file.path(figpath, paste("", nameforfig, figpathmore, ".pdf", sep="")),
    width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
     xlab="Model estimate change \nin relative growth rate", ylab="", main=nameforfig)
axis(2, at=1:1, labels=rev(c("Treatment")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_tx")
ppeffects <- c("mu_tx") # or 1:4 here...
for(i in 1:1){
  pos.y<-(1:1)[i]
  pos.x<-df[rownameshere[i],"mean"]
  lines(df[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(df),fixed=TRUE))[2:2]
  jitt<-runif(1,0.1,0.2)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-df[pos.sps.i[i],"mean"]
  lines(df[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
      col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[spsi], alphahere))
  
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", chill.stan$species.name))), pch=19,
   col=alpha(my.pal[1:spnum], alphahere),
   cex=1, bty="n", text.font=3)
dev.off()

}
