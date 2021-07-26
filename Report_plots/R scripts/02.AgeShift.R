
# 02.AgeShift.R

# Author: Bob Verity
# Date: 14/09/2015
# updated: Melissa Penny 19/07/2021

# Purpose:
# Reads in RTSS data from all four modelling groups and produces plot showing age shift in clinical cases averted.

# ------------------------------------------------------------------
# ------------------------------------------------------------------

# ------ must run _1_setWorkingPaths_Plots FIRST to set paths -----
# name of data to load

# nameDATA_toLoad = "Data_IVIRAC_5prev_20210719TESTreduced_paper.Rdata"

# test 2021and recover past with new simulations
# nameDATA_toLoad =  "Data_2021_recoverPast.Rdata"

# 2021 WHO report
nameDATA_toLoad = "Data_20210721_WHOreport_Cov80.Rdata"
# nameDATA_toLoad = "Data_20210721_WHOreport_Cov90.Rdata"
# nameDATA_toLoad = "Data_20210721_WHOreport_Cov50.Rdata"

# ------------------------------------------------------------------
# ------------------------------------------------------------------
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

# produce age shift plot
agePlot <- function(df, xlim, ylim, ylines, groups, main) {
	
	# set colours
	redBlue <- c("#F7523A","#8EBEDA")
	df$col <- redBlue[1]
	df$col[df$median<0] <- redBlue[2]
	
	# produce barplot
	barplot(df$median, space=0, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, axes=F)
	abline(h=ylines, col=grey(0.95))
	barplot(df$median, space=0, col=df$col, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, xlab="age (years)", ylab="clinical cases averted\n (thousands)", main=main, axes=F, add=T)
	
	# add error bars and axes
	errorBars(x=(1:length(df$median))-0.5, ymin=df$min, ymax=df$max, width=0.25)
	axis(1,at=seq(0,length(df$median),5))
	axis(2)
}

# ------------------------------------------------------------------

# set working directory
# setwd("~/Dropbox/Bob/Work/RTSS/RTSS Comparison - Shared Drive/Data_finalPapers/Combined")

# decide whether to save plot to file
saveToFile <- T

# load "Data" object
# load "Data" object
load(paste(pathData, nameDATA_toLoad, sep=""))


# subset data to median clinical cases averted
df <- subset(Data,Transmission%in%c("prev10","prev50") & age_granulation=="fine")
df <- subset(df,Vaccine=="with_booster")
df <- subset(df,Event=="clinical_cases_averted")
df <- subset(df,Summary=="median")
df <- subset(df,select=c("Group","Transmission","Age_lower","Age_upper","cumy15"))

# extract actual plotting data
df$Age_width <- df$Age_upper-df$Age_lower
df$plotData <- df$cumy15/df$Age_width/1e3

# split into prev10 and prev50. Calculate median, min and max over groups
df_prev10 <- df_prev50 <- NULL
for (i in 0:14) {
	tmp <- subset(df,Transmission=="prev10" & Age_lower<=i & Age_upper>=(i+1))
	df_prev10 <- rbind(df_prev10, data.frame(median=median(tmp$plotData), min=min(tmp$plotData), max=max(tmp$plotData)))
	
	tmp <- subset(df,Transmission=="prev50" & Age_lower<=i & Age_upper>=(i+1))
	df_prev50 <- rbind(df_prev50, data.frame(median=median(tmp$plotData), min=min(tmp$plotData), max=max(tmp$plotData)))
}

# save to file
plotOn(saveToFile,paste(pathPlots_limitedPlots,'Figure2_ageShift',sep=''),width=10,height=4.5)
par(mfrow=c(1,2))

# low prevalence plot
par(mar=c(5.1,10.1,4.1,2.1))
par(xpd=F)
agePlot(df_prev10, xlim=c(0,15), ylim=c(-5,41), ylines=seq(-5,40,5), groups, main="low endemicity\n")
par(xpd=T)
text(15/2,45,expression(paste("(",italic(Pf),PR[2-10],"=10%)",sep="")),cex=0.8)

# high prevalence plot
par(mar=c(5.1,2.1,4.1,10.1))
par(xpd=F)
agePlot(df_prev50, xlim=c(0,15), ylim=c(-5,41), ylines=seq(-5,40,5), groups, main="high endemicity\n")
par(xpd=T)
text(15/2,45,expression(paste("(",italic(Pf),PR[2-10],"=50%)",sep="")),cex=0.8)

plotOff(saveToFile)
