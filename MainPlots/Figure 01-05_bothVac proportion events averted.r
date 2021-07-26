
# load data if needed
	# Data = read.csv(file.choose(),header=TRUE) #load from .../Data
	# source("R scripts/Functions.r")

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="prop_clinical_cases_averted", my_FigureNumber=11, my_time=my_time, xlab='age', ylab='percentage clinical cases averted', main='percentage clinical cases averted by age', save_plots=save_plots )

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="prop_hospitalised_cases_averted", my_FigureNumber=13, my_time=my_time, xlab='age', ylab='percentage hospitalised cases averted', main='percentage hospitalised cases averted by age', save_plots=save_plots )

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="prop_deaths_averted", my_FigureNumber=14, my_time=my_time, xlab='age', ylab='percentage deaths averted', main='percentage deaths averted by age', save_plots=save_plots )

# curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="DALYs_averted", my_FigureNumber=15, my_time=my_time, xlab='age', ylab='percentage DALYs averted', main='percentage DALYs averted by age', save_plots=save_plots )

# only for severe plots
NumGroupsToPlot_temp = NumGroupsToPlot
if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp-1
}
# don't plot Imperial severe
curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="prop_severe_cases_averted", my_FigureNumber=12, my_time=my_time, xlab='age', ylab='percentage severe cases averted', main='percentage severe cases averted by age', save_plots=save_plots )
if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp
}
rm(NumGroupsToPlot_temp)
