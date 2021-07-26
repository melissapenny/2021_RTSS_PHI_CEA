
# load data if needed
	# Data = read.csv(file.choose(),header=TRUE) #load from .../Data
	# source("R scripts/Functions.r")

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="clinical_cases_averted_per_100000_vac", my_FigureNumber=11, my_time=my_time, xlab='age', ylab='clinical cases averted per 100000 fully vaccinated', main='clinical cases averted by age', save_plots=save_plots )


curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="hospitalised_cases_averted_per_100000_vac", my_FigureNumber=13, my_time=my_time, xlab='age', ylab='hospitalised cases averted per 100000 fully vaccinated', main='hospitalised cases averted by age', save_plots=save_plots )

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="deaths_averted_per_100000_vac", my_FigureNumber=14, my_time=my_time, xlab='age', ylab='deaths averted per 100000 fully vaccinated', main='deaths averted by age', save_plots=save_plots )

curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="DALYs_averted_per_100000_vac", my_FigureNumber=15, my_time=my_time, xlab='age', ylab='DALYs averted per 100000 fully vaccinated', main='DALYs averted by age', save_plots=save_plots )


# only for severe plots
NumGroupsToPlot_temp = NumGroupsToPlot
if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp-1
}
# don't plot Imperial severe
curr_plot = EventsAvertedPlot_bothVac(Data, my_Event="severe_cases_averted_per_100000_vac", my_FigureNumber=12, my_time=my_time, xlab='age', ylab='severe cases averted per 100000 fully vaccinated', main='severe cases averted by age', save_plots=save_plots )

if (NumGroupsToPlot_temp > 1){
  NumGroupsToPlot = NumGroupsToPlot_temp
}
rm(NumGroupsToPlot_temp)


