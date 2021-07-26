
# load data if needed
	# Data = read.csv(file.choose(),header=TRUE) #load from .../Data
	# source("R scripts/Functions.r")

curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="prop_clinical_cases_averted", my_FigureNumber=1, my_time=my_time , save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="prop_hospitalised_cases_averted", my_FigureNumber=3, my_time=my_time, save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="prop_deaths_averted", my_FigureNumber=4, my_time=my_time, save_plots=save_plots )
# # curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="prop_DALYs_averted", my_FigureNumber=5, my_time=my_time, save_plots=save_plots )

# only for severe plots
NumGroupsToPlot_temp = NumGroupsToPlot
if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp-1
}
# don't plot Imperial severe
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="prop_severe_cases_averted", my_FigureNumber=2, my_time=my_time, save_plots=save_plots )

if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp
}
rm(NumGroupsToPlot_temp)






