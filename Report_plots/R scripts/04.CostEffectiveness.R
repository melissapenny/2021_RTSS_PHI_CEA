
# 04.CostEffectiveness.R

# Author: Bob Verity
# Date: 15/09/2015
# updated: Melissa Penny 19/07/2021

# Purpose:
# Reads in RTSS data from all four modelling groups and produces plot showing cost-effectiveness in terms of cost per clinical case averted and per DALY averted

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

# produce cost-effectiveness plot
plotCE <- function(df,xlim=c(0,60),ylim,xlab='',ylab='',main='',cex.lab=1) {
	plot(0, type="n", xlim=xlim, ylim=ylim, xlab='', ylab='', xaxs="i", yaxs="i", axes=F)
	abline(h=seq(0,max(ylim),50),col=grey(0.97))
	abline(v=seq(0,max(xlim),10),col=grey(0.97))
	#abline(h=100,col=grey(0.75),lty=1,lwd=1.5)
	# groupVec <- c("GSK","EMOD DTK","Imperial","OpenMalaria")
	# colVec <- c("#D55E00","#009E73","#0072A7","#C879C8")
	
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
	
	vaccineVec <- c("without_booster","with_booster")
	for (i in 1:length(groupVec)) {
		for (j in 1:length(vaccineVec)) {
			z <- subset(df,Group==groupVec[i] & Vaccine==vaccineVec[j])
			z <- z[order(z$prev_2_10),]
			lines(z$prev_2_10, z$cumy15, type='l', lwd=1, lty=j, col=colVec[i])
		}
	}
	par(new=T)
	par(xpd=NA)
	plot(0, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, cex.lab=cex.lab, main='', xaxs="i", yaxs="i", axes=F)
	box()
	text(max(xlim)/2,max(ylim)*1.1,main,cex=1.4)
	par(xpd=F)
}

# ------------------------------------------------------------------

# set working directory
# setwd("~/Dropbox/Bob/Work/RTSS/RTSS Comparison - Shared Drive/Data_finalPapers/Combined")

# decide whether to save plot to file
saveToFile <- T

# load "Data" object

load(paste(pathData, nameDATA_toLoad, sep=""))

# subset data
df <- subset(Data,Vaccine%in%c("with_booster","without_booster"))
df <- subset(df,Event%in%c("cost_per_clinical_case_averted", "cost_per_DALY_averted_(undiscounted)"))
df <- subset(df,Summary=="median")
df <- subset(df,age_granulation=="all_ages")
df <- subset(df,select=c("Group","prev_2_10","Vaccine","Vaccine_price","Event","cumy15"))
df$prev_2_10 <- as.numeric(df$prev_2_10)
df <- subset(df,prev_2_10<=60)

# split into the six desired panels
df_clin_2 <- subset(df,Event=="cost_per_clinical_case_averted" & Vaccine_price==2)
df_clin_5 <- subset(df,Event=="cost_per_clinical_case_averted" & Vaccine_price==5)
df_clin_10 <- subset(df,Event=="cost_per_clinical_case_averted" & Vaccine_price==10)
df_DALY_2 <- subset(df,Event=="cost_per_DALY_averted_(undiscounted)" & Vaccine_price==2)
df_DALY_5 <- subset(df,Event=="cost_per_DALY_averted_(undiscounted)" & Vaccine_price==5)
df_DALY_10 <- subset(df,Event=="cost_per_DALY_averted_(undiscounted)" & Vaccine_price==10)


# produce plots
# plotOn(saveToFile,'~/Desktop/Figure4_costEffectiveness',width=8,height=4.5)

plotOn(saveToFile,paste(pathPlots_limitedPlots,'Figure4_costEffectivenessd',sep=''),width=8,height=4.5)

layout(matrix(1:10,2,5,byrow=T))

par(mar=c(0.5,0.5,5,0.5))
frame()

plotCE(df_clin_2, ylim=c(0,250), main="USD $2", ylab="cost in USD per clinical\ncase averted", cex.lab=1.1)
axis(2,las=2,cex.axis=0.8)

plotCE(df_clin_5, ylim=c(0,250), main="USD $5")

plotCE(df_clin_10, ylim=c(0,250), main="USD $10")

frame()
par(mar=c(5,0.5,0.5,0.5))
frame()

plotCE(df_DALY_2, ylim=c(0,600), ylab="cost in USD per DALY\naverted", cex.lab=1.1)
axis(1,cex.axis=0.8)
axis(2,las=2,cex.axis=0.8)

plotCE(df_DALY_5, ylim=c(0,600), xlab=expression(italic(Pf)*PR[2-10]), cex.lab=1.4)
axis(1,cex.axis=0.8)

plotCE(df_DALY_10, ylim=c(0,600))
axis(1,cex.axis=0.8)

#par(xpd=NA)
#legend(80,700,legend=c("GSK","EMOD DTK","Imperial","OpenMalaria"), col=c("#D55E00","#009E73","#0072A7","#C879C8"), lwd=1, box.lwd=0.5)
#legend(80,400,legend=c("3 doses","4 doses"), lty=c(1,2), box.lwd=0.5)
#par(xpd=T)

frame()
plotOff(saveToFile)
