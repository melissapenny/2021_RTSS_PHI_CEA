# load packages and data
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("RColorBrewer") 

################################

# ------ must run _1_setWorkingPaths_Plots FIRST to set paths -----
# ------ must also have run all figures and tables-----

pathTablesMain = paste(pathPaperSummary, "tablesForPaper/",sep="")
pathTablesSupplment = paste(pathPaperSummary, "tablesForSupplement/",sep="")
pathPaperSummary_extra = paste(pathPaperSummary, "tablesForPaper_extraSummaries/",sep="")
dir.create(pathTablesMain, showWarnings = FALSE)
dir.create(pathTablesSupplment, showWarnings = FALSE)
dir.create(pathPaperSummary_extra, showWarnings = FALSE)

# pathReportMainSummary
################################
# prevalence tables

namesOfTable = c("propDeathsAverted_U5", "propClinicalCasesAverted_U5","propDeathsAverted", "deathper100kvaccaverted",  "severeCasesper100kvaccaverted", "clinicalCasesper100kvaccaverted", "costperdaly_2", "costperdaly_5","costperdaly_10", "costperClinicalCase_2", "costperClinicalCase_5", "costperClinicalCase_10")

summary = list()
summary$outcome = NULL
summary$vaccine = NULL
summary$med_3to65 = NULL
summary$range_3to65 = NULL
summary$med_10to65 = NULL
summary$range_10to65 = NULL
summary$med_10to50 = NULL
summary$range_10to50 = NULL
summary$med_3 = NULL
summary$range_3 = NULL
summary$med_30to50 = NULL
summary$range_30to50 = NULL

summaryTable = list()
summaryTable$outcome = NULL
summaryTable$vaccine = NULL
summaryTable$range_3to65 = NULL
summaryTable$range_10to65 = NULL
summaryTable$range_10to50 = NULL
summaryTable$range_3 = NULL
summaryTable$range_30to50 = NULL

summaryLow= list()
summaryLow$outcome = NULL
summaryLow$vaccine = NULL
summaryLow$med_3 = NULL
summaryLow$range_3 = NULL
summaryLow$med_5 = NULL
summaryLow$range_5 = NULL
summaryLow$med_7.5 = NULL
summaryLow$range_7.5 = NULL
summaryLow$med_10 = NULL
summaryLow$range_10 = NULL

summaryLowTable= list()
summaryLowTable$outcome = NULL
summaryLowTable$vaccine = NULL
summaryLowTable$range_3 = NULL
summaryLowTable$range_5 = NULL
summaryLowTable$range_7.5 = NULL
summaryLowTable$range_10 = NULL

summaryLowPrev = list()
summaryLowPrev$outcome = NULL
summaryLowPrev$vaccine = NULL
summaryLowPrev$med_3to10 = NULL
summaryLowPrev$range_3to10 = NULL
summaryLowPrev$med_5to10 = NULL
summaryLowPrev$range_5to10 = NULL

summaryLowPrevTable = list()
summaryLowPrevTable$outcome = NULL
summaryLowPrevTable$vaccine = NULL
summaryLowPrevTable$range_3to10 = NULL
summaryLowPrevTable$range_5to10 = NULL



 # testing
tableName = "propDeathsAverted_U5"
tableName = "propClinicalCasesAverted_U5"
tableName = "propDeathsAverted"
tableName = "deathper100kvaccaverted"
tableName = "severeCasesper100kvaccaverted"
tableName = "clinicalCasesper100kvaccaverted"
tableName = "costperdaly_2"
tableName = "costperdaly_5"
tableName = "costperdaly_10"
tableName = "costperClinicalCase_2"
tableName = "costperClinicalCase_5"
tableName = "costperClinicalCase_10"

tableName = namesOfTable[1]
my_boost ="with_booster"
for (tableName in namesOfTable){
	for (my_boost in c("with_booster", "without_booster")){
	
	
		df = read.csv(paste(pathPlots,"Table_", tableName,"_", my_boost,"_MediansOnly.csv", sep=""))
		estimates_3to65 = c(df$prev3,df$prev10,df$prev30,df$prev50, df$prev65)
		estimates_10to65 = c(df$prev10,df$prev30,df$prev50, df$prev65)
		estimates_10to50 = c(df$prev10,df$prev30,df$prev50)
		estimates_30to50 = c(df$prev30,df$prev50)
		
		
		summary$outcome = c(summary$outcome, tableName)
		summary$vaccine = c(summary$vaccine, my_boost)
		summary$med_3to65 = c(summary$med_3to65, median(sort(estimates_3to65)))
		summary$range_3to65 = c(summary$range_3to65, paste("(",range(sort(estimates_3to65))[1]," to ",range(sort(estimates_3to65))[2],")",sep=""))
		summary$med_10to65 = c(summary$med_10to65, median(sort(estimates_10to65)))
		summary$range_10to65 = c(summary$range_10to65, paste("(",range(sort(estimates_10to65))[1]," to ",range(sort(estimates_10to65))[2],")",sep=""))
		summary$med_10to50 = c(summary$med_10to50, median(sort(estimates_10to50)))
		summary$range_10to50 = c(summary$range_10to50, paste("(",range(sort(estimates_10to50))[1]," to ",range(sort(estimates_10to50))[2],")",sep=""))
		summary$med_30to50 = c(summary$med_30to50, median(sort(estimates_30to50)))
		summary$range_30to50 = c(summary$range_30to50, paste("(",range(sort(estimates_30to50))[1]," to ",range(sort(estimates_30to50))[2],")",sep=""))
		
		summaryTable$outcome = c(summaryTable$outcome, tableName)
		summaryTable$vaccine = c(summaryTable$vaccine, my_boost)
		summaryTable$range_3to65 = c(summaryTable$range_3to65, paste(median(sort(estimates_3to65)), " (", range(sort(estimates_3to65))[1],"-",range(sort(estimates_3to65))[2],")",sep=""))
		summaryTable$range_10to65 = c(summaryTable$range_10to65, paste(median(sort(estimates_10to65)), " (", range(sort(estimates_10to65))[1], "-", range(sort(estimates_10to65))[2],")",sep=""))
		summaryTable$range_10to50 = c(summaryTable$range_10to50, paste(median(sort(estimates_10to50)), " (", range(sort(estimates_10to50))[1], "-", range(sort(estimates_10to50))[2],")",sep=""))
		summaryTable$range_30to50 = c(summaryTable$range_30to50, paste(median(sort(estimates_30to50)), " (", range(sort(estimates_30to50))[1], "-", range(sort(estimates_30to50))[2],")",sep=""))
		
		rm(estimates_3to65, estimates_10to65, estimates_10to50, estimates_30to50)
		
		#-------
		df = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_", my_boost,"_MediansOnly.csv", sep=""))
		estimates_3 = c(df$prev3)
		estimates_5 = c(df$prev5)
		estimates_7.5 = c(df$prev7.5)
		estimates_10 = c(df$prev10)
		
		summaryLow$outcome = c(summaryLow$outcome, tableName)
		summaryLow$vaccine = c(summaryLow$vaccine, my_boost)
		summaryLow$med_3 = c(summaryLow$med_3, median(sort(estimates_3)))
		summaryLow$range_3 = c(summaryLow$range_3 , paste("(",range(sort(estimates_3))[1]," to ",range(sort(estimates_3))[2],")",sep=""))
		
		
		summaryLow$med_5 = c(summaryLow$med_5, median(sort(estimates_5)))
		summaryLow$range_5 = c(summaryLow$range_5 , paste("(",range(sort(estimates_5))[1]," to ",range(sort(estimates_5))[2],")",sep=""))
		
		summaryLow$med_7.5 = c(summaryLow$med_7.5, median(sort(estimates_7.5)))
		summaryLow$range_7.5 = c(summaryLow$range_7.5 , paste("(",range(sort(estimates_7.5))[1]," to ",range(sort(estimates_7.5))[2],")",sep=""))
		
		summaryLow$med_10 = c(summaryLow$med_10, median(sort(estimates_10)))
		summaryLow$range_10 = c(summaryLow$range_10 , paste("(",range(sort(estimates_10))[1]," to ",range(sort(estimates_10))[2],")",sep=""))
		
		
		summaryLowTable$outcome = c(summaryLowTable$outcome, tableName)
		summaryLowTable$vaccine = c(summaryLowTable$vaccine, my_boost)
		summaryLowTable$med_3 = c(summaryLowTable$med_3, median(sort(estimates_3)))
		summaryLowTable$range_3 = c(summaryLowTable$range_3 , paste(median(sort(estimates_3)), " (", range(sort(estimates_3))[1], "-",range(sort(estimates_3))[2],")",sep=""))
		summaryLowTable$range_5 = c(summaryLowTable$range_5 , paste(median(sort(estimates_5)), " (", range(sort(estimates_5))[1], "-",range(sort(estimates_5))[2],")",sep=""))
		summaryLowTable$range_7.5 = c(summaryLowTable$range_7.5 , paste(median(sort(estimates_7.5)), " (", range(sort(estimates_7.5))[1], "-",range(sort(estimates_7.5))[2],")",sep=""))
		summaryLowTable$range_10 = c(summaryLowTable$range_10 , paste(median(sort(estimates_10)), " (", range(sort(estimates_10))[1], "-",range(sort(estimates_10))[2],")",sep=""))
		rm(estimates_3, estimates_5, estimates_7.5, estimates_10)
		
		#-------
		df = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_", my_boost,"_MediansOnly.csv", sep=""))
		estimates_3to10 = c(df$prev3,df$prev5,df$prev7.5,df$prev10 )
		estimates_5to10 = c(df$prev5,df$prev7.5,df$prev10)

		summaryLowPrev$outcome = c(summaryLowPrev$outcome, tableName)
		summaryLowPrev$vaccine = c(summaryLowPrev$vaccine, my_boost)
		summaryLowPrev$med_3to10 = c(summaryLowPrev$med_3to10, median(sort(estimates_3to10)))
		summaryLowPrev$range_3to10 = c(summaryLowPrev$range_3to10,  paste("(",range(sort(estimates_3to10))[1]," to ",range(sort(estimates_3to10))[2],")",sep=""))
		summaryLowPrev$med_5to10 = c(summaryLowPrev$med_5to10, median(sort(estimates_5to10)))
		summaryLowPrev$range_5to10 = c(summaryLowPrev$range_5to10,  paste("(",range(sort(estimates_5to10))[1]," to ",range(sort(estimates_5to10))[2],")",sep=""))
		
		summaryLowPrevTable$outcome = c(summaryLowPrevTable$outcome, tableName)
		summaryLowPrevTable$vaccine = c(summaryLowPrevTable$vaccine, my_boost)
		summaryLowPrevTable$range_3to10 = c(summaryLowPrevTable$range_3to10,  paste(median(sort(estimates_3to10)), " (", range(sort(estimates_3to10))[1], "-",range(sort(estimates_3to10))[2],")",sep=""))
		summaryLowPrevTable$range_5to10 = c(summaryLowPrevTable$range_5to10,  paste(median(sort(estimates_5to10)), " (", range(sort(estimates_5to10))[1], "-",range(sort(estimates_5to10))[2],")",sep=""))
				
		rm(estimates_3to10, estimates_5to10)
	}
	
}

summary = as.data.frame(summary)
summaryLow = as.data.frame(summaryLow)
summaryLowPrev = as.data.frame(summaryLowPrev)
summaryTable = as.data.frame(summaryTable)
summaryLowTable = as.data.frame(summaryLowTable)
summaryLowPrevTable = as.data.frame(summaryLowPrevTable)
write.csv(summary, file = paste(pathPaperSummary_extra,"summaryTransmission.csv", sep=""),row.names=FALSE)
write.csv(summary, file = paste(pathReportMainSummary,"summaryTransmission.csv", sep=""),row.names=FALSE)
write.csv(summaryLow, file = paste(pathPaperSummary_extra,"summaryTransmission_low.csv", sep=""),row.names=FALSE)
write.csv(summaryLowPrev, file = paste(pathPaperSummary_extra,"summaryTransmission_lowPrev.csv", sep=""),row.names=FALSE)

write.csv(summaryTable, file = paste(pathPaperSummary_extra,"summaryTransmissionTable.csv", sep=""),row.names=FALSE)
write.csv(summaryTable, file = paste(pathReportMainSummary,"summaryTransmissionTable.csv", sep=""),row.names=FALSE)
write.csv(summaryLowTable, file = paste(pathPaperSummary_extra,"summaryTransmission_lowTable.csv", sep=""),row.names=FALSE)
write.csv(summaryLowPrevTable, file = paste(pathPaperSummary_extra,"summaryTransmission_lowPrevTable.csv", sep=""),row.names=FALSE)

summaryTable_paper = summaryTable
summaryTable_paper[["range_10"]] = summaryLowTable[["range_10"]]
summaryTable_paper[["range_7.5"]] = summaryLowTable[["range_7.5"]]
summaryTable_paper[["range_5"]] = summaryLowTable[["range_5"]]
write.csv(summaryTable_paper, file = paste(pathTablesMain,"table3_", "summaryTransmissionTable_ranges.csv", sep=""),row.names=FALSE)


################################
# - country tables
produceCountrySumm = FALSE

if(produceCountrySumm){
namesOfTable = c( "deathper100kvaccaverted",  "severeCasesper100kvaccaverted", "clinicalCasesper100kvaccaverted", "costperdaly_2", "costperdaly_5","costperdaly_10")

summaryCountry = list()
summaryCountry$outcome = NULL
summaryCountry$vaccine = NULL
summaryCountry$med_all = NULL
summaryCountry$range_all = NULL
summaryCountry$med_prevGt10 = NULL
summaryCountry$range_prevGt10 = NULL

summaryCountryTable = list()
summaryCountryTable$outcome = NULL
summaryCountryTable$vaccine = NULL
summaryCountryTable$range_all = NULL
summaryCountryTable$range_prevGt10 = NULL

for (tableName in namesOfTable){
	for (my_boost in c("with_booster", "without_booster")){
	
	
		df = read.csv(paste(pathPlotsCountry,"Table_", tableName,"_6countries_", my_boost,"_MediansOnly.csv", sep=""))
		estimates_all = c(df$Country.A, df$Country.B, df$Country.C, df$Country.D, df$Country.E, df$Country.F)
		estimates_prevGt10 = c(  df$Country.D, df$Country.E, df$Country.F)		
		
		summaryCountry$outcome = c(summaryCountry$outcome, tableName)
		summaryCountry$vaccine = c(summaryCountry$vaccine, my_boost)
		summaryCountry$med_all = c(summaryCountry$med_all, median(sort(estimates_all)))
		summaryCountry$range_all = c(summaryCountry$range_all , paste("(",range(sort(estimates_all))[1]," to ",range(sort(estimates_all))[2],")",sep=""))
		summaryCountry$med_prevGt10 = c(summaryCountry$med_prevGt10, median(sort(estimates_prevGt10)))
		summaryCountry$range_prevGt10 = c(summaryCountry$range_prevGt10 , paste("(",range(sort(estimates_prevGt10))[1]," to ",range(sort(estimates_prevGt10))[2],")",sep=""))
		
		summaryCountryTable$outcome = c(summaryCountryTable$outcome, tableName)
		summaryCountryTable$vaccine = c(summaryCountryTable$vaccine, my_boost)
		summaryCountryTable$range_all = c(summaryCountryTable$range_all , paste(median(sort(estimates_all)), " (", range(sort(estimates_all))[1],"-",range(sort(estimates_all))[2],")",sep=""))
		summaryCountryTable$range_prevGt10 = c(summaryCountryTable$range_prevGt10 , paste(median(sort(estimates_prevGt10)), " (", range(sort(estimates_prevGt10))[1],"-",range(sort(estimates_prevGt10))[2],")",sep=""))
		
		
	}
	
}

summaryCountry = as.data.frame(summaryCountry)
write.csv(summaryCountry, file = paste(pathPaperSummary_extra,"summaryCountry.csv", sep=""),row.names=FALSE)

summaryCountryTable = as.data.frame(summaryCountryTable)
write.csv(summaryCountryTable, file = paste(pathPaperSummary_extra,"summaryCountryTable.csv", sep=""),row.names=FALSE)


write.csv(summaryCountryTable, file = paste(pathTablesMain,"table7_", "summaryCountryTable_ranges.csv", sep=""),row.names=FALSE)
}
################################
# - incremental Tables!


namesOfTable = c( "deathper100kvaccaverted", "clinicalCasesper100kvaccaverted")



summaryIncrementalTable = list()
summaryIncrementalTable$outcome = NULL
summaryIncrementalTable$vaccine = NULL
summaryIncrementalTable$group = NULL
summaryIncrementalTable$range_10 = NULL
summaryIncrementalTable$range_30 = NULL
summaryIncrementalTable$range_50 = NULL
summaryIncrementalTable$range_65 = NULL


summaryIncrementalTable_lowPrev= list()
summaryIncrementalTable_lowPrev$outcome = NULL
summaryIncrementalTable_lowPrev$vaccine = NULL
summaryIncrementalTable_lowPrev$group = NULL
summaryIncrementalTable_lowPrev$range_3 = NULL
summaryIncrementalTable_lowPrev$range_5 = NULL
summaryIncrementalTable_lowPrev$range_7.5 = NULL
summaryIncrementalTable_lowPrev$range_10 = NULL
summaryIncrementalTable_lowPrev$range_15 = NULL

tableName = namesOfTable[1]
my_boost ="with_booster"
for (tableName in namesOfTable){
	
	

	df_with_booster = read.csv(paste(pathPlots,"Table_", tableName,"_", "with_booster","_MediansOnly.csv", sep=""))
	df_without_booster = read.csv(paste(pathPlots,"Table_", tableName,"_", "without_booster","_MediansOnly.csv", sep=""))
	summaryIncrementalTable_temp = df_with_booster[,2:7]
	summaryIncrementalTable_temp[,2:6] = 100*(df_with_booster[,3:7]-df_without_booster[,3:7])/df_without_booster[,3:7]
	
	# x
	
	summaryIncrementalTable$outcome = c(summaryIncrementalTable$outcome, rep(tableName,4))
	summaryIncrementalTable$vaccine = c(summaryIncrementalTable$vaccine, rep("incremental as percentage",4))
	summaryIncrementalTable$group = c(summaryIncrementalTable$group, summaryIncrementalTable_temp[,1])
	summaryIncrementalTable$range_3 = c(summaryIncrementalTable$range_3, summaryIncrementalTable_temp[,2])
	summaryIncrementalTable$range_10 = c(summaryIncrementalTable$range_10, summaryIncrementalTable_temp[,3])
	summaryIncrementalTable$range_30 = c(summaryIncrementalTable$range_30, summaryIncrementalTable_temp[,4])
	summaryIncrementalTable$range_50 = c(summaryIncrementalTable$range_50, summaryIncrementalTable_temp[,5])
	summaryIncrementalTable$range_65 = c(summaryIncrementalTable$range_65, summaryIncrementalTable_temp[,6])

	df_with_booster_lowPrev = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_", "with_booster","_MediansOnly.csv", sep=""))
	df_without_booster_lowPrev = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_",  "without_booster","_MediansOnly.csv", sep=""))
	# summaryIncrementalTable_lowPrev = df_with_booster_lowPrev[,2:7]
	# summaryIncrementalTable_lowPrev[,2:6] = 100*(df_with_booster_lowPrev[,3:7]-df_without_booster_lowPrev[,3:7])/df_without_booster_lowPrev[,3:7]
	
	
	summaryIncrementalTable_lowPrev_temp = df_with_booster_lowPrev[,2:7]
	summaryIncrementalTable_lowPrev_temp[,2:6] = 100*(df_with_booster_lowPrev[,3:7]-df_without_booster_lowPrev[,3:7])/df_without_booster_lowPrev[,3:7]
	
	summaryIncrementalTable_lowPrev$outcome = c(summaryIncrementalTable_lowPrev$outcome, rep(tableName,4))
	summaryIncrementalTable_lowPrev$vaccine = c(summaryIncrementalTable_lowPrev$vaccine, rep("incremental as percentage",4))
	summaryIncrementalTable_lowPrev$group = c(summaryIncrementalTable_lowPrev$group, summaryIncrementalTable_lowPrev_temp[,1])
	summaryIncrementalTable_lowPrev$range_3 = c(summaryIncrementalTable_lowPrev$range_3, summaryIncrementalTable_lowPrev_temp[,2])
	summaryIncrementalTable_lowPrev$range_5 = c(summaryIncrementalTable_lowPrev$range_5, summaryIncrementalTable_lowPrev_temp[,3])
	summaryIncrementalTable_lowPrev$range_7.5 = c(summaryIncrementalTable_lowPrev$range_7.5, summaryIncrementalTable_lowPrev_temp[,4])
	summaryIncrementalTable_lowPrev$range_10 = c(summaryIncrementalTable_lowPrev$range_10, summaryIncrementalTable_lowPrev_temp[,5])
	summaryIncrementalTable_lowPrev$range_15 = c(summaryIncrementalTable_lowPrev$range_15, summaryIncrementalTable_lowPrev_temp[,6])

	
	
}


summaryIncrementalTable = as.data.frame(summaryIncrementalTable)

summaryIncrementalTable_lowPrev = as.data.frame(summaryIncrementalTable_lowPrev)


write.csv(summaryIncrementalTable, file = paste(pathPaperSummary_extra,"summaryIncrementalTransmissionTable.csv", sep=""),row.names=FALSE)

write.csv(summaryIncrementalTable_lowPrev, file = paste(pathPaperSummary_extra,"summaryIncrementalTransmission_lowPrevTable.csv", sep=""),row.names=FALSE)



write.csv(summaryIncrementalTable, file = paste(pathTablesMain,"table3_","summaryIncrementalTransmissionTable.csv", sep=""),row.names=FALSE)

write.csv(summaryIncrementalTable_lowPrev, file = paste(pathTablesMain,"table3_","summaryIncrementalTransmission_lowPrevTable.csv", sep=""),row.names=FALSE)




################################################################
################################################################
################################################################
	
namesOfTable = c( "propDeathsAverted_U5", "propDeathsAverted", "propClinicalCasesAverted")



summaryIncrementalTableBasedOnProp = list()
summaryIncrementalTableBasedOnProp$outcome = NULL
summaryIncrementalTableBasedOnProp$vaccine = NULL
summaryIncrementalTableBasedOnProp$group = NULL
summaryIncrementalTableBasedOnProp$range_10 = NULL
summaryIncrementalTableBasedOnProp$range_30 = NULL
summaryIncrementalTableBasedOnProp$range_50 = NULL
summaryIncrementalTableBasedOnProp$range_65 = NULL


summaryIncrementalTableBasedOnProp_lowPrev= list()
summaryIncrementalTableBasedOnProp_lowPrev$outcome = NULL
summaryIncrementalTableBasedOnProp_lowPrev$vaccine = NULL
summaryIncrementalTableBasedOnProp_lowPrev$group = NULL
summaryIncrementalTableBasedOnProp_lowPrev$range_3 = NULL
summaryIncrementalTableBasedOnProp_lowPrev$range_5 = NULL
summaryIncrementalTableBasedOnProp_lowPrev$range_7.5 = NULL
summaryIncrementalTableBasedOnProp_lowPrev$range_10 = NULL
summaryIncrementalTableBasedOnProp_lowPrev$range_15 = NULL

tableName = namesOfTable[1]
my_boost ="with_booster"
for (tableName in namesOfTable){
	
	

	df_with_booster = read.csv(paste(pathPlots,"Table_", tableName,"_", "with_booster","_MediansOnly.csv", sep=""))
	df_without_booster = read.csv(paste(pathPlots,"Table_", tableName,"_", "without_booster","_MediansOnly.csv", sep=""))
	summaryIncrementalTableBasedOnProp_temp = df_with_booster[,2:7]
	summaryIncrementalTableBasedOnProp_temp[,2:6] = df_with_booster[,3:7]-df_without_booster[,3:7]
	
	# x
	
	summaryIncrementalTableBasedOnProp$outcome = c(summaryIncrementalTableBasedOnProp$outcome, rep(tableName,4))
	summaryIncrementalTableBasedOnProp$vaccine = c(summaryIncrementalTableBasedOnProp$vaccine, rep("incremental as percentage",4))
	summaryIncrementalTableBasedOnProp$group = c(summaryIncrementalTableBasedOnProp$group, summaryIncrementalTableBasedOnProp_temp[,1])
	summaryIncrementalTableBasedOnProp$range_3 = c(summaryIncrementalTableBasedOnProp$range_3, summaryIncrementalTableBasedOnProp_temp[,2])
	summaryIncrementalTableBasedOnProp$range_10 = c(summaryIncrementalTableBasedOnProp$range_10, summaryIncrementalTableBasedOnProp_temp[,3])
	summaryIncrementalTableBasedOnProp$range_30 = c(summaryIncrementalTableBasedOnProp$range_30, summaryIncrementalTableBasedOnProp_temp[,4])
	summaryIncrementalTableBasedOnProp$range_50 = c(summaryIncrementalTableBasedOnProp$range_50, summaryIncrementalTableBasedOnProp_temp[,5])
	summaryIncrementalTableBasedOnProp$range_65 = c(summaryIncrementalTableBasedOnProp$range_65, summaryIncrementalTableBasedOnProp_temp[,6])

	df_with_booster_lowPrev = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_", "with_booster","_MediansOnly.csv", sep=""))
	df_without_booster_lowPrev = read.csv(paste(pathPlots,"Table_", tableName,"_lowPrev_",  "without_booster","_MediansOnly.csv", sep=""))
	# summaryIncrementalTableBasedOnProp_lowPrev = df_with_booster_lowPrev[,2:7]
	# summaryIncrementalTableBasedOnProp_lowPrev[,2:6] = 100*(df_with_booster_lowPrev[,3:7]-df_without_booster_lowPrev[,3:7])/df_without_booster_lowPrev[,3:7]
	
	summaryIncrementalTableBasedOnProp_lowPrev_temp = df_with_booster_lowPrev[,2:7]
	summaryIncrementalTableBasedOnProp_lowPrev_temp[,2:6] = df_with_booster_lowPrev[,3:7]-df_without_booster_lowPrev[,3:7]
	
	summaryIncrementalTableBasedOnProp_lowPrev$outcome = c(summaryIncrementalTableBasedOnProp_lowPrev$outcome, rep(tableName,4))
	summaryIncrementalTableBasedOnProp_lowPrev$vaccine = c(summaryIncrementalTableBasedOnProp_lowPrev$vaccine, rep("incremental as percentage",4))
	summaryIncrementalTableBasedOnProp_lowPrev$group = c(summaryIncrementalTableBasedOnProp_lowPrev$group, summaryIncrementalTableBasedOnProp_lowPrev_temp[,1])
	summaryIncrementalTableBasedOnProp_lowPrev$range_3 = c(summaryIncrementalTableBasedOnProp_lowPrev$range_3, summaryIncrementalTableBasedOnProp_lowPrev_temp[,2])
	summaryIncrementalTableBasedOnProp_lowPrev$range_5 = c(summaryIncrementalTableBasedOnProp_lowPrev$range_5, summaryIncrementalTableBasedOnProp_lowPrev_temp[,3])
	summaryIncrementalTableBasedOnProp_lowPrev$range_7.5 = c(summaryIncrementalTableBasedOnProp_lowPrev$range_7.5, summaryIncrementalTableBasedOnProp_lowPrev_temp[,4])
	summaryIncrementalTableBasedOnProp_lowPrev$range_10 = c(summaryIncrementalTableBasedOnProp_lowPrev$range_10, summaryIncrementalTableBasedOnProp_lowPrev_temp[,5])
	summaryIncrementalTableBasedOnProp_lowPrev$range_15 = c(summaryIncrementalTableBasedOnProp_lowPrev$range_15, summaryIncrementalTableBasedOnProp_lowPrev_temp[,6])

	
	
}



summaryIncrementalTableBasedOnProp = as.data.frame(summaryIncrementalTableBasedOnProp)

summaryIncrementalTableBasedOnProp_lowPrev = as.data.frame(summaryIncrementalTableBasedOnProp_lowPrev)


write.csv(summaryIncrementalTableBasedOnProp, file = paste(pathPaperSummary_extra,"summaryIncrementalTransmissionTableBasedOnProp.csv", sep=""),row.names=FALSE)

write.csv(summaryIncrementalTableBasedOnProp_lowPrev, file = paste(pathPaperSummary_extra,"summaryIncrementalTransmissionBasedOnProp_lowPrevTable.csv", sep=""),row.names=FALSE)


################################################################
################################################################
################################################################
# - copy tables to folder for paper 





TableNames_1 = c("Table_propDeathsAverted_U5_without_booster.csv", "Table_propDeathsAverted_U5_with_booster.csv", "Table_deathper100kvaccaverted_without_booster.csv", "Table_deathper100kvaccaverted_with_booster.csv", "Table_clinicalCasesper100kvaccaverted_without_booster.csv","Table_clinicalCasesper100kvaccaverted_with_booster.csv")


TableNames_2 = c("Table_deathper100kvaccaverted_6countries_without_booster.csv", "Table_deathper100kvaccaverted_6countries_with_booster.csv","Table_clinicalCasesper100kvaccaverted_6countries_without_booster.csv", "Table_clinicalCasesper100kvaccaverted_6countries_with_booster.csv")


TableNames_3 = c("Table_costperdaly_5_without_booster.csv", "Table_costperdaly_5_with_booster.csv", "Table_costperdaly_2_without_booster.csv", "Table_costperdaly_2_with_booster.csv", "Table_costperdaly_10_without_booster.csv", "Table_costperdaly_10_with_booster.csv", "Table_costperClinicalCase_5_without_booster.csv", "Table_costperClinicalCase_5_with_booster.csv", "Table_costperClinicalCase_2_without_booster.csv", "Table_costperClinicalCase_2_with_booster.csv", "Table_costperClinicalCase_10_without_booster.csv", "Table_costperClinicalCase_10_with_booster.csv")

TableNames_4 = c("Table_costperdaly_5_6countries_without_booster.csv", "Table_costperdaly_5_6countries_with_booster.csv", "Table_costperdaly_2_6countries_without_booster.csv", "Table_costperdaly_2_6countries_with_booster.csv", "Table_costperdaly_10_6countries_without_booster.csv", "Table_costperdaly_10_6countries_with_booster.csv")

TableNames_Supplement = c( "Table_propDeathsAverted_U5_lowPrev_without_booster.csv", "Table_propDeathsAverted_U5_lowPrev_with_booster.csv", "Table_deathper100kvaccaverted_lowPrev_without_booster.csv", "Table_deathper100kvaccaverted_lowPrev_with_booster.csv", "Table_clinicalCasesper100kvaccaverted_lowPrev_without_booster.csv", "Table_clinicalCasesper100kvaccaverted_lowPrev_with_booster.csv", "Table_costperdaly_5_lowPrev_without_booster.csv", "Table_costperdaly_5_lowPrev_with_booster.csv", "Table_costperdaly_2_lowPrev_without_booster.csv", "Table_costperdaly_2_lowPrev_with_booster.csv", "Table_costperdaly_10_lowPrev_without_booster.csv", "Table_costperdaly_10_lowPrev_with_booster.csv", "Table_clinicalCasesper100kvaccaverted_without_booster.csv", "Table_clinicalCasesper100kvaccaverted_with_booster.csv", "Table_propClinicalCasesAverted_U5_without_booster.csv","Table_propClinicalCasesAverted_U5_with_booster.csv", "Table_costperClinicalCase_5_lowPrev_without_booster.csv", "Table_costperClinicalCase_5_lowPrev_with_booster.csv", "Table_costperClinicalCase_2_lowPrev_without_booster.csv", "Table_costperClinicalCase_2_lowPrev_with_booster.csv", "Table_costperClinicalCase_10_lowPrev_without_booster.csv", "Table_costperClinicalCase_10_lowPrev_with_booster.csv")


# TableNames_Supplement_2 = c("Table_clinicalCasesper100kvaccaverted_6countries_without_booster.csv", "Table_clinicalCasesper100kvaccaverted_6countries_with_booster.csv", "Table_clinicalCasesper100kvaccaverted_6countries_without_booster.csv", "Table_clinicalCasesper100kvaccaverted_6countries_with_booster.csv") 


TablePaperNum = 2
for (fileNam in TableNames_1){
	dif=read.csv(paste(pathPlots, fileNam, sep=""))
	write.csv(dif[,!(names(dif)=="X")], paste(pathTablesMain, "table",TablePaperNum,"_",fileNam, sep=""), row.names=FALSE)
	rm(dif)
}
TablePaperNum = 4

if(produceCountrySumm){
for (fileNam in TableNames_2){

	dif=read.csv(paste(pathPlotsCountry, fileNam, sep=""))
	write.csv(dif[,!(names(dif)=="X")], paste(pathTablesMain, "table",TablePaperNum,"_",fileNam, sep=""), row.names=FALSE)
	rm(dif)
}
}


TablePaperNum = 5
for (fileNam in TableNames_3){

	dif=read.csv(paste(pathPlots, fileNam, sep=""))
	write.csv(dif[,!(names(dif)=="X")], paste(pathTablesMain, "table",TablePaperNum,"_",fileNam, sep=""), row.names=FALSE)
	rm(dif)
}
TablePaperNum = 6

if(produceCountrySumm){
for (fileNam in TableNames_4){

	dif=read.csv(paste(pathPlotsCountry, fileNam, sep=""))
	write.csv(dif[,!(names(dif)=="X")], paste(pathTablesMain, "table",TablePaperNum,"_",fileNam, sep=""), row.names=FALSE)
	rm(dif)
}
}

for (fileNam in TableNames_Supplement){

	dif=read.csv(paste(pathPlots, fileNam, sep=""))
	write.csv(dif[,!(names(dif)=="X")], paste(pathTablesSupplment, fileNam, sep=""), row.names=FALSE)
	rm(dif)
}

# for (fileNam in TableNames_Supplement_2){

	# dif=read.csv(paste(pathPlotsCountry, fileNam, sep=""))
	# write.csv(dif[,!(names(dif)=="X")], paste(pathTablesSupplment, fileNam, sep=""), row.names=FALSE)
	# rm(dif)
# }




