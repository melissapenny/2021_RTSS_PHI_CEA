  
  # load packages and data
  library("ggplot2")
  library("plyr")
  library("dplyr")
  library("tidyr")
  library("RColorBrewer")
  
  #########################
  # load data	
  
  #-- test 2021and recover past with new simulations
  #load(paste(pathData, "Data_2021_recoverPast.Rdata", sep=""))

  # --- new predictions from SwissTPH 2021
  # doesn't work load(paste(pathData, "Data_20210721_WHOreport.Rdata", sep=""))
  load(paste(pathData, "Data_20210721_WHOreport_Cov80.Rdata", sep=""))
  # load(paste(pathData, "Data_20210721_WHOreport_Cov90.Rdata", sep=""))
  # load(paste(pathData, "Data_20210721_WHOreport_Cov50.Rdata", sep=""))
  
  # highest prevalence to plot
  prevHighNum = 65
  prevHighName = "prev_65"
  maxPrevLimitPlots = 75
  maxPrevLimitPlots = 65
  #(required for combined plots)
  
  table(Data$Event,Data$Group)
  
  DataAll = Data
  
  # remove Data from OpenMalaria that is greater than prev 70
  DataO <- Data[!(Data$Group=="OpenMalaria"),]
  # unique(DataTemp$prev_2_10)
  DataS <- Data[Data$Group=="OpenMalaria",]
  DataS1 <-  DataS[DataS$prev_2_10 < 70,] 
  Data<- rbind(DataO, DataS1)
  DataAll = Data
  
  
  #----
  # should plots be saved?
  save_plots=T
  
  # resolution of the plots
  resolution=300
  # lower resolution for mac
  #resolution=100
  # # 
    
  # at what time
  my_time="cumy15"
  #my_time="cumy05"
  update_tables=T
  
  # define colors
  gg_color_hue <- function(n) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=65, c=100)[1:n]}
  # # my_col=gg_color_hue(4)
  # # names(my_col)=c("GSK","EMOD DTK","Imperial","OpenMalaria")
  # my_col=c(gg_color_hue(4),"#7CAE00")
  # names(my_col)=c("GSK","EMOD DTK","Imperial","OpenMalaria","EMOD~DTK")
  
  #2015 # define colourBlind safe colors
  # recomCol = c("#D55E00", "#009E73", "#0072A7","#CC79A7") # slightly more blue pink/purple #C879C8 to replace pink#CC79A7
  # my_recomCol = c("#D55E00", "#009E73", "#0072A7","#C879C8")
  # my_col=c(my_recomCol,"#009E73")
  # names(my_col)=c("GSK","EMOD DTK","Imperial","OpenMalaria","EMOD~DTK")
  
  
  # added 2021 number of groups to plot
  # NumGroupsToPlot = 4
  NumGroupsToPlot = 2
  NumGroupsToPlot = 1
  
  # 2021 define colourBlind safe colors
  # if (NumGroupsToPlot==2){
    my_recomCol = c("#0072A7","#C879C8")
    my_col=c(my_recomCol) #,"#009E73")
    names(my_col)=c("Imperial","OpenMalaria")
    GROUPStoSelect = c( "Imperial", "OpenMalaria")
  # }
  if (NumGroupsToPlot==1){
    my_recomCol = c("#C879C8")
    my_col=c(my_recomCol) #,"#009E73")
    names(my_col)=c("OpenMalaria")
    GROUPStoSelect = c("OpenMalaria")
  }
  
  # ##############################################
  # # plot efficacy against infection
  # ##############################################
  # # load data
  # load(paste(pathData, "Data_IVIRAC_EfficacyAgainstInfection_20150518_paper.Rdata", sep=""))
  # DataEffAll = DataEff
  # DataEffAll$Group <- factor(DataEffAll$Group, levels = c("EMOD DTK", "GSK",  "Imperial", "OpenMalaria"))
  # load(paste(pathData, "Data_IVIRAC_EfficacyAgainstInfection_20150518_paper_imperialAntibody.Rdata", sep=""))
  # DataEffAll_antibody = DataEff
  # DataEffAll_antibody$Group <- factor(DataEffAll_antibody$Group, levels = c("EMOD DTK", "GSK",  "Imperial", "OpenMalaria"))
  # source("Figure 00 EfficacyAgainstInfection.r")
  
  ##############################################
  # effectiveness Plots
  ##############################################
  Data = DataAll
  # get levels in correct order
  # Data$Group <- factor(Data$Group, levels = c("EMOD DTK", "GSK",  "Imperial", "OpenMalaria"))
  # Data$Event <- factor(Data$Event, levels = c("prop_clinical_cases_averted","prop_severe_cases_averted","prop_hospitalised_cases_averted","prop_deaths_averted","prop_DALYs_averted"))
  # 
  Data$Group <- factor(Data$Group, levels = GROUPStoSelect)
  Data$Event <- factor(Data$Event, levels = c("prop_clinical_cases_averted","prop_severe_cases_averted","prop_hospitalised_cases_averted","prop_deaths_averted","prop_DALYs_averted"))
  
  
  # create all figures
  # my_boost = "without_booster"
  # my_boost = "with_booster"
  for (my_boost in c("without_booster","with_booster")){ #,"with_booster_incremental")){
  	source("Functions proportion events averted.r")
  	source("Figure 01-05 proportion events averted.r")
  	source("Figure 06 proportion events averted.r")
  }
  source("Functions_bothVac proportion events averted.r")
  source("Figure 01-05_bothVac proportion events averted.r")
  
  ##############################################
  # events averted per 100,000 fully vaccinated
  ##############################################
  Data = DataAll
  # get levels in correct order
  # Data$Group <- factor(Data$Group, levels = c("EMOD DTK", "GSK", "Imperial", "OpenMalaria"))
  # Data$Event <- factor(Data$Event, levels = c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac","cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)","vaccinees"))
  
  Data$Group <- factor(Data$Group, levels = GROUPStoSelect)
  Data$Event <- factor(Data$Event, levels = c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac","cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)","vaccinees"))
  
  
  # create all figures
  source("Figure 00 Vaccinees.r")
  # my_boost = "without_booster"
  # my_boost = "with_booster"
  for (my_boost in c("without_booster","with_booster")){ #,"with_booster_incremental")){
  	source("Functions per 100000 FV.r")
  	source("Figure 01-05 events averted per 100000 FV.r")
  	source("Figure 06 Events averted per 100000 FV.r")
  	source("Figure 06 Events averted per 100000 FV - Combined.r")
  	source("Figure 07 costs per Event averted.r")
  }
  source("Functions_bothVac per 100000 FV.r")
  source("Figure 01-05_bothVac events averted per 100000 FV.r")
  source("Figure 07b costs per Event averted_boostNoBoost_DALYs - combined.r")
  
  
  #
  # Data <- tmpData
  
  DataO <- Data[!(Data$Group=="OpenMalaria"),]
  # unique(DataTemp$prev_2_10)
  DataS <- Data[Data$Group=="OpenMalaria",]
  DataS1 <- DataS[!(DataS$Event=="cost_per_clinical_case_averted"),]
  DataS2 <-  DataS[DataS$Event=="cost_per_clinical_case_averted"& DataS$prev_2_10 %in% c("3","5","7.5","10","15","20","25","30","35","40","45","50","55","60"),]#,"65"),] 
  Data<- rbind(DataO, DataS1,DataS2)
  update_tables=F
  # reduce prevalences to limit yscales
  for (my_boost in c("without_booster","with_booster")){ #,"with_booster_incremental")){
  	source("Figure 07 costs per Event averted.r")
  }
  source("Figure 07a costs per Event averted_boostNoBoost_DALYs.r")
  source("Figure 07alancet costs per Event averted_boostNoBoost_DALYsClinical.r")
  update_tables=T
  
  Data = DataAll	
  source("Figure 08 costs effectiveness planes.r")
  # source("Figure 09 incremental cost effectiveness.r")
  
  
  
  ##############################################
  # events averted in 100,000 population
  ##############################################
  createPlotsPer100000Pop <- FALSE
  if (createPlotsPer100000Pop){
  	Data = DataAll
  	
  	# get levels in correct order
  	# Data$Group <- factor(Data$Group, levels = c("EMOD DTK", "GSK",  "Imperial", "OpenMalaria"))
  	Data$Group <- factor(Data$Group, levels = GROUPStoSelect)
  	Data$Event <- factor(Data$Event, levels = c("clinical_cases_averted","severe_cases_averted","hospitalised_cases_averted","deaths_averted","DALYs_averted","cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)","vaccinees"))
  	
  	
  			
  	# create all figures
  	
  	for (my_boost in c("without_booster","with_booster")){ #,"with_booster_incremental")){
  		source("Functions.r")
  		source("Figure 01-05 events averted.r")
  		source("Figure 06 Events averted.r")
  		
  	}
  
  }
  
  
  
  
