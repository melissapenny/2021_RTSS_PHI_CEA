  
  # load packages and data
  library("ggplot2")
  library("plyr")
  library("dplyr")
  library("tidyr")
  library("reshape2")
  
  pathPlots = pathPlotsCountry
  #########################
  # load data	
  # load(paste(pathData, "Data_IVIRAC_country_20150216_IVIRAC_final.Rdata", sep=""))
  load(paste(pathData, "Data_IVIRAC_country_20150630_paper.Rdata", sep=""))

  DataAll = Data
  # should plots be saved?
  save_plots=T
  
  # resolution of the plots
  resolution=300
  # lower resolution for mac
	resolution=100
  
  # at what time
  my_time="cumy15"
  
  # define colors
  gg_color_hue <- function(n) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=65, c=100)[1:n]}
  my_col=gg_color_hue(4)
  names(my_col)=c("GSK","EMOD DTK","Imperial","OpenMalaria")
  
  # define colourBlind safe colors
recomCol = c("#D55E00", "#009E73", "#0072A7","#CC79A7") # slightly more blue pink/purple #C879C8 to replace pink#CC79A7
my_recomCol = c("#D55E00", "#009E73", "#0072A7","#C879C8")
my_col=c(my_recomCol,"#009E73")
names(my_col)=c("GSK","EMOD DTK","Imperial","OpenMalaria","EMOD~DTK")
  
  ##############################################
  # events averted per 100,000 fully vaccinated
  ##############################################
  # get levels in correct order
  Data = DataAll
  
  # for testing only
  Data$Group <- factor(Data$Group, levels = c("EMOD DTK", "GSK", "Imperial", "OpenMalaria"))
  subData = Data[Data$Event %in% c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac","cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)","vaccinees"),]
  Data = subData
  Data$Event <- factor(Data$Event, levels = c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac","cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)","vaccinees"))

  
  # create all figures
  source("FigureCountryPred 00 Vaccinees.r")
  for (my_boost in c("without_booster","with_booster")){ #,"with_booster_incremental")){
  	source("FunctionsCountryPred per 100000 FV.r")
  	source("FigureCountryPred 01-05 events averted per 100000 FV.r")
  	source("FigureCountryPred 06 Events averted per 100000 FV.r")
  	source("FigureCountryPred 07 costs per Event averted.r")
  }
  
  
  
  
  
