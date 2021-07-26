#### set paths


user = "Melissa"


# ---- Melissa
if(user == "Melissa"){
  # Path to read scripts
  setwd('~/Documents/SwissTPH/Malaria/RTSS/codeScripts_RTSS/MVI_RTSS/scripts_2021_WHO/scripts_SummaryAndPlots/WHO_RTSScomparison/MainPlots')
  
  
  # # Path for Data and for saving data
  # # 2021 revover of 2015  test data
  # pathData = "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_predictions/Data_testCode2021/Combined/"
  # # Path for saving plots to test 2015 in 3032
  # mainPath_results =  "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_Figures/testCode2021/"


  # -----2021 test data
  pathData = "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_predictions/Data_July2021/Combined/"
  
  # ----------Path for saving plots --- need to specify right folder for new sensitivites
  mainPath_results =  "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_Figures/report_July2021_Cov80/"
  # mainPath_results =  "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_Figures/report_July2021_Cov90/"
  # mainPath_results =  "~/Dropbox (Personal)/shared_RTSScomparison2021_MELISSA/2021_Figures/report_July2021_Cov50/"
  dir.create(mainPath_results, showWarnings = FALSE)
  
  pathPlots = paste( mainPath_results,"Figures/",sep="")
  dir.create(pathPlots, showWarnings = FALSE)
  
  pathPaperSummary = paste( mainPath_results,"summariesForPaper/",sep="")
  dir.create(pathPaperSummary, showWarnings = FALSE)
  
  pathReportMainSummary = paste( mainPath_results,"summariesForReport2021/",sep="")
  dir.create(pathReportMainSummary, showWarnings = FALSE)
  
  pathPlots_limitedPlots = paste( mainPath_results,"limitedFigure/",sep="")
  dir.create(pathPlots_limitedPlots, showWarnings = FALSE)
  
  # country results not needed 2021
  # pathPlotsCountry = paste( mainPath_results,"Figures_country/",sep="")
  # dir.create(pathPlotsCountry, showWarnings = FALSE)
  
  # switch for mac
  dirSwitch = "/"
}


