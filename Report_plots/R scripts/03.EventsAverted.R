
# 03.EventsAverted.R

# Author: Bob Verity
# Date: 15/09/2015
# updated: Melissa Penny 19/07/2021

# Purpose:
# Reads in RTSS data from all four modelling groups and produces plot showing clinical cases and deaths averted by prevalence both with and without booster.

# ------------------------------------------------------------------
# ------------------------------------------------------------------

# ------ must run _1_setWorkingPaths_Plots FIRST to set paths -----
# name of data to load

# nameDATA_toLoad = "Data_IVIRAC_5prev_20210719TESTreduced_paper.Rdata"
# NumGroupsToPlot = 2

# test 2021and recover past with new simulations
# nameDATA_toLoad =  "Data_2021_recoverPast.Rdata"
# NumGroupsToPlot = 1


# 2021 WHO report
nameDATA_toLoad = "Data_20210721_WHOreport_Cov80.Rdata"
# nameDATA_toLoad = "Data_20210721_WHOreport_Cov90.Rdata"
# nameDATA_toLoad = "Data_20210721_WHOreport_Cov50.Rdata"
NumGroupsToPlot = 1
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# install reshape2 package
if (!"reshape2"%in%rownames(installed.packages()))
	install.packages("reshape2")
require("reshape2")

# produce error bars at given positions
errorBars <- function(x,ymin,ymax,width,lty=1,lwd=1) {
	segments(x,ymin,x,ymax,lty=lty,lwd=lwd)
	segments(x-width/2,ymin,x+width/2,ymin,lty=lty,lwd=lwd)
	segments(x-width/2,ymax,x+width/2,ymax,lty=lty,lwd=lwd)
}

# start optional save-to-file function
plotOn <- function(active=FALSE, fileroot='', filestem1='', filestem2='', fileindex='', type='pdf', width=6, height=6, res=300) {	
	if (active) {
		if (type=='pdf') {
			pdf(file=paste(fileroot,filestem1,filestem2,fileindex,".pdf",sep=""), bg="white", width=width, height=height)
		}
		if (type=='png') {
			png(file=paste(fileroot,filestem1,filestem2,fileindex,".png",sep=""), bg="white", width=width*500, height=height*500,res=res)
		}
	}
}

# end optional save-to-file function
plotOff <- function(active=FALSE) {	
	if (active) {
		dev.off()
	}
}

# produce barplot of events averted
eventsAverted <- function(df, y_at, prev_values, xlab="", ylab="", main="", NumGrpsToPlot= 2) {
  # df = plotData
  
	# extract median values and lower and upper bounds
	median <- dcast(subset(df, select=c("Group","Transmission","median")), ...~Group, value.var="median")
	median <- median[match(prev_levels,median$Transmission), match(groupVec,names(median))]
	lower <- dcast(subset(df, select=c("Group","Transmission","Q0.025")), ...~Group, value.var="Q0.025")
	lower <- lower[match(prev_levels,lower$Transmission), match(groupVec,names(lower))]
	upper <- dcast(subset(df, select=c("Group","Transmission","Q0.975")), ...~Group, value.var="Q0.975")
	upper <- upper[match(prev_levels,upper$Transmission), match(groupVec,names(upper))]
	
	# produce barplot
	b <- barplot(t(as.matrix(median)), beside=T, border=NA, names=rep(NA,length(prev_values)), ylim=c(0,max(y_at)), xlab=xlab, ylab=ylab, main=main, axes=F, cex.lab=1.2, cex.main=1.3)
	abline(h=y_at,col=grey(0.97))
	par(lwd=0.5)
	barplot(t(as.matrix(median)), beside=T, names=prev_values, col=colVec, axes=F, add=T)
	
	if(NumGroupsToPlot>1){
	  for (i in 1:nrow(b)) {
		  errorBars(b[i,],lower[,i],upper[,i],width=0.25)
	  }
	}else{
	  for (i in 1:ncol(b)) {
	    errorBars(b[i],lower[i],upper[i],width=0.25)
	  }
	}
	axis(1,at=colMeans(b),labels=F)
}

# ------------------------------------------------------------------

# set working directory
# setwd("~/Dropbox/Bob/Work/RTSS/RTSS Comparison - Shared Drive/Data_finalPapers/Combined")
# instead run _1_setWorkingPaths_Plots

# decide whether to save plot to file
saveToFile <- T

# load "Data" object
load(paste(pathData, nameDATA_toLoad, sep=""))

# choose prevalences to plot
prev_values <- c(3,5,10,15,20,25,35,45,55,65)
prev_levels <- paste("prev",prev_values,sep="")

# set group order and colours
# groupVec <- c("EMOD DTK","Imperial","OpenMalaria","GSK")
# groupVec <- c("EMOD DTK","Imperial","OpenMalaria","GSK")
# colVec <- c("#009E73","#0072A7","#C879C8","#D55E00")
groupVec <- c("Imperial","OpenMalaria")
colVec <- c("#0072A7","#C879C8")



# 2021 define colourBlind safe colors
# if (NumGroupsToPlot==2){
my_recomCol = c("#0072A7","#C879C8")
colVec=c(my_recomCol) #,"#009E73")
names(colVec)=c("Imperial","OpenMalaria")
groupVec = c( "Imperial", "OpenMalaria")
# }
if (NumGroupsToPlot==1){
  my_recomCol = c("#C879C8")
  colVec=c(my_recomCol) #,"#009E73")
  names(colVec)=c("OpenMalaria")
  groupVec = c("OpenMalaria")
}


# subset data
df <- subset(Data,Transmission%in%prev_levels)
df <- subset(df,Vaccine%in%c("with_booster","without_booster"))
df <- subset(df,Event%in%c("clinical_cases_averted_per_100000_vac", "deaths_averted_per_100000_vac"))
df <- subset(df,Summary%in%c("median","Q0.025","Q0.975"))
df <- subset(df,age_granulation%in%c("all_ages"))
df <- subset(df,select=c("Group","Transmission","Vaccine","Event","Summary","cumy15"))
df$plot <- df$cumy15#/100		# convert to per 1000 fully vaccinated

# split into clinical vs. deaths and with vs. without booster
clin_boost <- subset(df,Event=="clinical_cases_averted_per_100000_vac" & Vaccine=="with_booster",select=c("Group","Transmission","Summary","plot"))
clin_noBoost <- subset(df,Event=="clinical_cases_averted_per_100000_vac" & Vaccine=="without_booster",select=c("Group","Transmission","Summary","plot"))
deaths_boost <- subset(df,Event=="deaths_averted_per_100000_vac" & Vaccine=="with_booster",select=c("Group","Transmission","Summary","plot"))
deaths_noBoost <- subset(df,Event=="deaths_averted_per_100000_vac" & Vaccine=="without_booster",select=c("Group","Transmission","Summary","plot"))


# produce plots
# plotOn(saveToFile,'~/Desktop/Figure3_eventsAverted',width=10,height=6)
# 
plotOn(saveToFile,paste(pathPlots_limitedPlots,'Figure3_eventsAverted',sep=''),width=10,height=6)
par(mfrow=c(2,2))

par(mar=c(2,10,5,0.5))
y_at <- seq(0,2.4e5,2e4)
plotData <- dcast(clin_noBoost,...~Summary,value.var="plot")
eventsAverted(plotData, y_at=y_at, prev_values=prev_values, ylab="clinical cases averted\nper 100,000 FVC\n", main=expression(paste("3 dose schedule")), NumGrpsToPlot=NumGroupsToPlot)
axis(2, at=y_at, labels=format(y_at,big.mark=",",scientific=F), las=1, cex.axis=0.8)

par(mar=c(2,0.5,5,10))
plotData <- dcast(clin_boost,...~Summary,value.var="plot")
eventsAverted(plotData, y_at=y_at, prev_values=prev_values, main=expression(paste("4 dose schedule")), NumGrpsToPlot=NumGroupsToPlot)

par(mar=c(5,10,2,0.5))
y_at <- seq(0,1200,100)
plotData <- dcast(deaths_noBoost,...~Summary,value.var="plot")
eventsAverted(plotData, y_at=y_at, prev_values=prev_values, ylab="deaths averted\nper 100,000 FVC\n", NumGrpsToPlot=NumGroupsToPlot)
axis(2, at=y_at, labels=format(y_at,big.mark=",",scientific=F), las=1, cex.axis=0.8)

par(mar=c(5,0.5,2,10))
plotData <- dcast(deaths_boost,...~Summary,value.var="plot")
eventsAverted(plotData, y_at=y_at, prev_values=prev_values, NumGrpsToPlot=NumGroupsToPlot)

par(xpd=NA)
text(-2,-400,expression(italic(Pf)*PR[2-10]),cex=1.2)
#legend(55,16,legend=c("GSK","EMOD DTK","Imperial","OpenMalaria"), fill=c("#D55E00","#009E73","#0072A7","#C879C8"), cex=0.8)
par(xpd=F)

plotOff(saveToFile)
