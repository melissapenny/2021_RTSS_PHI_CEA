# combine SwissTPH - only need if combining booster and non-booster PHI with CEA is needed

# recover the 2015 estimates
# phi1 = "SwissTPH_summaryPredictions_20210707_WHOtransmission_5to17mth_boosters_withBaseline"
# phi2 = "SwissTPH_summaryPredictions_20210707_WHOtransmission_5to17mth_withBaseline"
# cea1 = "SwissTPH_transmission_cea_2021-07-07"
# cea2 = "SwissTPH_transmission_incremental_cea_2021-07-07"
# comparionTypeFinalFileNames = "SwissTPH_transmission_July2021test"

# name of the 3dose and 4 dose schedules
nameResultsToUse_phi1 = "with_booster"
nameResultsToUse_phi2 = "without_booster"

produceCountryPredictions = FALSE

# 2021 WHO report - main folder of sensitvities
phi1 = "SwissTPH_summaryPredictions_20210709_WHOtransmissionCovSen_5to17mth_boosters_withBaseline"
phi2 = "SwissTPH_summaryPredictions_20210709_WHOtransmissionCovSen_5to17mth_withBaseline"


#--- names needed for 80% coverage
cea1 = "SwissTPH_transmission_cea_2021-07-09_Cov_3d_80_4d_64"
cea2 = "SwissTPH_transmission_incremental_cea_2021-07-09_Cov_3d_80_4d_64"
comparionTypeFinalFileNames = "SwissTPH_transmissionCovSen_20210709_Cov_3d_80_4d_64"
# "with_booster" is the 80% coverage "with_booster_coverage_50" "with_booster_coverage_90"
nameResultsToUse_phi1 = "with_booster"
nameResultsToUse_phi2 = "without_booster"

#--- names needed for 90% coverage
# cea1 = "SwissTPH_transmission_cea_2021-07-09_Cov_3d_90_4d_72"
# cea2 = "SwissTPH_transmission_incremental_cea_2021-07-09_Cov_3d_90_4d_72"
# comparionTypeFinalFileNames = "SwissTPH_transmissionCovSen_20210709_Cov_3d_90_4d_72"
# # "with_booster" is the 80% coverage "with_booster_coverage_50" "with_booster_coverage_90"
# nameResultsToUse_phi1 = "with_booster_coverage_90"
# nameResultsToUse_phi2 = "without_booster_coverage_90"

#--- names needed for 90% coverage
# cea1 = "SwissTPH_transmission_cea_2021-07-09_Cov_3d_50_4d_40"
# cea2 = "SwissTPH_transmission_incremental_cea_2021-07-09_Cov_3d_50_4d_40"
# comparionTypeFinalFileNames = "SwissTPH_transmissionCovSen_20210709_Cov_3d_50_4d_40"
# # "with_booster" is the 80% coverage "with_booster_coverage_50" "with_booster_coverage_90"
# nameResultsToUse_phi1 = "with_booster_coverage_50"
# nameResultsToUse_phi2 = "without_booster_coverage_50"


# phi 5to17mth
df1=read.csv(paste(pathData,"SwissTPH", dirSwitch, "phi", dirSwitch, phi1, ".csv", sep="")); names(df1)[which(names(df1)=="Age.lower")]="Age_lower"; names(df1)[which(names(df1)=="Age.upper")]="Age_upper"
df1_temp = df1
rm(df1)
df1 = df1_temp[df1_temp$Vaccine == nameResultsToUse_phi1,]
df1$Vaccine[df1$Vaccine == nameResultsToUse_phi1] = "with_booster"
rm(df1_temp)
# phi 5to17mth_boosters
df2=read.csv(paste(pathData,"SwissTPH", dirSwitch, "phi", dirSwitch, phi2, ".csv", sep="")); names(df2)[which(names(df2)=="Age.lower")]="Age_lower"; names(df2)[which(names(df2)=="Age.upper")]="Age_upper"
df2_temp = df2
rm(df2)
df2 = df2_temp[df2_temp$Vaccine == nameResultsToUse_phi2,]
df2$Vaccine[df2$Vaccine == nameResultsToUse_phi2] = "without_booster"
rm(df2_temp)
# cea
df3=read.csv(paste(pathData,"SwissTPH", dirSwitch, "cea", dirSwitch, cea1, ".csv", sep="")); names(df3)[which(names(df3)=="Age.lower")]="Age_lower"; names(df3)[which(names(df3)=="Age.upper")]="Age_upper"; names(df3)[which(names(df3)=="Age_granulation")]="age_granulation"; names(df3)[which(names(df3)=="Prev_2_10")]="prev_2_10"
# 2021 could not create this file :( 

# cea incremental
df4=read.csv(paste(pathData,"SwissTPH", dirSwitch, "cea", dirSwitch, cea2,".csv", sep="")); names(df4)[which(names(df4)=="Age.lower")]="Age_lower"; names(df4)[which(names(df4)=="Age.upper")]="Age_upper"; names(df4)[which(names(df4)=="Age_granulation")]="age_granulation"; names(df4)[which(names(df4)=="Prev_2_10")]="prev_2_10"
#df3=read.csv(paste(pathData,"SwissTPH", dirSwitch, "cea", dirSwitch, cea2, ".csv", sep="")); names(df3)[which(names(df3)=="Age.lower")]="Age_lower"; names(df3)[which(names(df3)=="Age.upper")]="Age_upper"; names(df3)[which(names(df3)=="Age_granulation")]="age_granulation"; names(df3)[which(names(df3)=="Prev_2_10")]="prev_2_10"


# unique(df3$Event)

df3$Event=gsub("cost_per_DALY_averted","cost_per_DALYTEMP_averted", df3$Event)
# TODO we need to check all groups are using undiscounted DALYs
df3$Event=gsub("DALY_averted_undiscounted","DALYs_averted", df3$Event)
df3$Event=gsub("cost_per_DALYTEMP_averted","cost_per_DALY_averted", df3$Event)

df3$Event=gsub("cost_per_death_averted_undiscounted","cost_per_death_averted", df3$Event)
df3$Event=gsub("cost_per_clinical_case_averted_undiscounted","cost_per_clinical_case_averted", df3$Event)

df3$Event=gsub("averted_discounted","averted_(discounted)", df3$Event)
df3$Event=gsub("averted_undiscounted","averted_(undiscounted)", df3$Event)


# df4$Event=gsub("cost_per_DALY_averted","cost_per_DALYTEMP_averted", df4$Event)
# # TODO we need to check all groups are using undiscounted DALYs
# df4$Event=gsub("DALY_averted_undiscounted","DALYs_averted", df4$Event)
# df4$Event=gsub("cost_per_DALYTEMP_averted","cost_per_DALY_averted", df4$Event)
# 
# df4$Event=gsub("cost_per_death_averted_undiscounted","cost_per_death_averted", df4$Event)
# df4$Event=gsub("cost_per_clinical_case_averted_undiscounted","cost_per_clinical_case_averted", df4$Event)
# 
# df4$Event=gsub("averted_discounted","averted_(discounted)", df4$Event)
# df4$Event=gsub("averted_undiscounted","averted_(undiscounted)", df4$Event)

# df3$Event=factor(df3$Event)
# levels(df3$Event)

if(produceCountryPredictions){
	# names(df3)
	# names(df1)
	names(df3)[names(df3)=="Country"] = "Transmission"
	df3[["prev_2_10"]] = df3[["Transmission"]]
}
Data_SwissTPH=rbind(df1[,names(df1)],df2[,names(df1)],df3[,names(df1)])
Data_SwissTPH$Transmission=gsub("_","", Data_SwissTPH$Transmission)
Data_SwissTPH$prev_2_10=gsub("_","", Data_SwissTPH$prev_2_10)

# check Event entries
# unique(df3$Event)
# unique(Data$Event)
# unique(Data_SwissTPH$Event)
# unique(Data_SwissTPH$prev_2_10)
# unique(Data_SwissTPH$Transmission)

if(produceCountryPredictions){
	Prev_by_country = list()
	Prev_by_country$country = c("BurkinaFaso", "Ghana", "Kenya", "Senegal", "Tanzania", "Uganda")
	Prev_by_country$code = c("bfa", "gha", "ken","sen", "tza", "uga")
	
	Prev_by_country$medianPrevalence = c(66.0, 24.0, 3, 4, 7.0, 37.0)
Prev_by_country$meanPrevalence = c(61, 34, 6.8, 8, 17, 41)
	# corrected prevalence
	# Country / Median / Mean
# BFA / 66 / 61
# GHA / 24 / 34
# KEN / 3 / 8
# SEN / 4 / 7
# TZA / 7 / 17
# UGA / 37 / 41

	
	# Prev_by_country$meanPrevalence = c(62.2, 33.7, 6.8, 7.6, 7.2, 39.9)
	# Prev_by_country$medianPrevalence = c(68.0, 24.0, 0.1, 0.1, 4.0, 36.0)
# "BurkinaFaso", "bfa", 62.2, 68.0)
# "Ghana", "gha", 33.7, 24.0)
# "Kenya", "ken", 6.8, 0.1)
# "Senegal", "sen", 7.6, 0.1)
# "Tanzania", "tza", 17.2, 4.0)
# "Uganda", "uga", 39.9, 36.0)
	for(cInd in 1:length(Prev_by_country$country)){
		
		prevToUse = Prev_by_country$meanPrevalence[cInd]
		prevToUse = Prev_by_country$medianPrevalence[cInd]
		
		Data_SwissTPH$prev_2_10[Data_SwissTPH$prev_2_10==Prev_by_country$country[cInd]] = gsub(Prev_by_country$country[cInd], prevToUse, Data_SwissTPH$prev_2_10[Data_SwissTPH$prev_2_10==Prev_by_country$country[cInd]])
	}
	
	
} 
# save SwissTPH data
save(Data_SwissTPH,file=paste(pathData,"SwissTPH", dirSwitch, comparionTypeFinalFileNames,".Rdata",sep=""))
# write.csv(Data_SwissTPH, file=paste(pathData,"SwissTPH", dirSwitch, comparionTypeFinalFileNames,".csv",sep=""), row.names = TRUE)

