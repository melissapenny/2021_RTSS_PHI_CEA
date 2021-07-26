
# load data if needed
	# Data = read.csv(file.choose(),header=TRUE) #load from .../Data
	# source("R scripts/Functions.r")

curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="clinical_cases_averted_per_100000_vac", my_FigureNumber=1, my_time=my_time , save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="severe_cases_averted_per_100000_vac", my_FigureNumber=2, my_time=my_time, save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="hospitalised_cases_averted_per_100000_vac", my_FigureNumber=3, my_time=my_time, save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="deaths_averted_per_100000_vac", my_FigureNumber=4, my_time=my_time, save_plots=save_plots )
curr_plot = EventsAvertedPlot(Data, my_boost=my_boost, my_Event="DALYs_averted_per_100000_vac", my_FigureNumber=5, my_time=my_time, save_plots=save_plots )



