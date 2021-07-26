
# load current data 
# this dummy data has both Imperial and Swiss TPH - use for two model comparison
# Data=read.csv(paste(pathData,"Combined", dirSwitch,"DummyData_2021.csv",sep=""))

# this dummy data has just Swiss TPH - use for one model plots only
Data=read.csv(paste(pathData,"Combined", dirSwitch,"DummyData_2021_SwissTPHonly.csv",sep=""))
Data$Event=gsub("discouted","discounted", Data $Event)

updateSwissTPH = T
updateImperial = F

# ---- indicate final file name - should indicate a name descriptive of sensivitiy analysis or date of runs
#finalFileName = "Data_IVIRAC_5prev_20210719TESTreduced_paper.Rdata"
#finalFileName = "Data_2021_recoverPast.Rdata"
 
# finalFileName = "Data_20210721_WHOreport_cov80.Rdata"
# finalFileName = "Data_20210721_WHOreport_cov90.Rdata"
finalFileName = "Data_20210721_WHOreport_cov50.Rdata"

# ---Data_SwissTPH
if (updateSwissTPH) {
	Data=subset(Data, Group!="SwissTPH")
	Data=subset(Data, Group!="OpenMalaria")
	
	# - which data to load (older files)
	#load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmission_20150518.Rdata", sep=""));
	#load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmission_July2021test.Rdata", sep=""));
	# load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmissionCovSen_20210709.Rdata", sep=""));
	
	# - which data to load (NEWER sensitivity analysis)
	# load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmissionCovSen_20210709_Cov_3d_80_4d_64.Rdata", sep=""));
	# load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmissionCovSen_20210709_Cov_3d_90_4d_72.Rdata", sep=""));
	load(paste(pathData,"SwissTPH", dirSwitch, "SwissTPH_transmissionCovSen_20210709_Cov_3d_50_4d_40.Rdata", sep=""));
	
	Data_SwissTPH$Group = rep("OpenMalaria",length(Data_SwissTPH$Group))
	Data_SwissTPH$Group = factor(Data_SwissTPH$Group, levels="OpenMalaria")
	Data=rbind(Data, Data_SwissTPH[,names(Data)])
	Data$Transmission=gsub("_","",Data$Transmission)
	Data$prev_2_10=gsub("_","",Data$prev_2_10)
}


# ---Data Imperial
if (updateImperial) {
	Data=subset(Data, Group!="Imperial")
	df_Imperial=read.table(paste(pathData,"Imperial", dirSwitch, "RTSSABmodel_Restructured3.txt", sep=""), header=T);
	names(df_Imperial)[names(df_Imperial)=="Prev_2_10"]="prev_2_10"
	names(df_Imperial)[names(df_Imperial)=="Age_granularity"]="age_granulation"
	df_Imperial$Event=gsub("severe","hospitalised",df_Imperial$Event)
	colNames = c('Group','Transmission','prev_2_10','Vaccine','Vaccine_price','Event','Summary','age_granulation','Age_lower','Age_upper',paste('y',1:15,sep=''),'cumy05','cumy10','cumy15')
	df_Imperial = df_Imperial[,colNames]
	timeCols = which(names(df_Imperial)%in%c(paste('y',1:15,sep=''),'cumy05','cumy10','cumy15'))
	df_Imperial[df_Imperial$Event%in%c('prop_clinical_cases_averted','prop_severe_cases_averted','prop_hospitalised_cases_averted','prop_deaths_averted','prop_DALYs_averted','prop_DALYs_discounted_averted'),timeCols] = df_Imperial[df_Imperial$Event%in%c('prop_clinical_cases_averted','prop_severe_cases_averted','prop_hospitalised_cases_averted','prop_deaths_averted','prop_DALYs_averted','prop_DALYs_discounted_averted'),timeCols] * 100
	Data=rbind(Data, df_Imperial[,names(Data)])
}



#  not sure if needed
Data$Transmission[Data$Transmission=="prev7p5"]<-"prev7.5"

# --- tests
# table(Data$Transmission, Data$Group)
# table(Data$Vaccine, Data$Group)
# table(Data$Event, Data$Group)
# table(Data$Vaccine_price, Data$Group)
# table(Data$Summary, Data$Group)
# table(Data$age_granulation, Data$Group)

# save new data

save(Data,file=paste(pathData,"Combined", dirSwitch, finalFileName,sep=""))




