

#function to plot events averted

EventsAvertedPlot <- function(Data, my_boost, my_Event, my_FigureNumber, my_time, save_plots=T ){
	# create temporary copy of the working data
		Data_tmp=Data
    
    
    CountryList = c("BurkinaFaso", "Ghana", "Kenya", "Senegal", "Tanzania", "Uganda")
	# subset to the data needed
    Data_tmp=subset(Data_tmp, age_granulation=="fine")
    Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event==my_Event))
	  Data_tmp=subset(Data_tmp, Transmission %in% CountryList)
	  Data_tmp$Transmission <- factor(Data_tmp$Transmission, levels = CountryList)
	  no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	  no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	  Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  	
	  
  # create age group size information for barplot
	  Data_tmp$barWidths = Data_tmp$Age_upper - Data_tmp$Age_lower
	  Data_tmp$barMids = Data_tmp$Age_lower + Data_tmp$barWidths/2
  
  # get confidence intervals in shape
  	Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
  
	# get country labels in order
    Data_tmp_CI$Transmission <- factor(Data_tmp_CI$Transmission, levels = c("Kenya","Senegal","Tanzania","Ghana","Uganda","BurkinaFaso"))  
	  Data_tmp_CI$Transmission <- revalue(Data_tmp_CI$Transmission, c("Kenya"="Country A", "Senegal"="Country B", "Tanzania"="Country C", "Ghana"="Country D", "Uganda"="Country E", "BurkinaFaso"="Country F"))  
  
  	
	#plot
		my_plot = ggplot(data=Data_tmp_CI, aes(x=barMids,y=median ,width=barWidths, ymin=Q0.025, ymax=Q0.975, fill=Group)) +
					geom_bar(stat="identity", colour="white") + 
					geom_errorbar(width=.01, size=.25, color="black") +
					facet_grid(Transmission ~ Group, scales="free_y") + 
					xlab('Age') + ylab(paste(gsub("_"," ",Data_tmp_CI$Event[1]),'cinated', sep="")) + 
					xlim(c(0,20)) + theme_bw() + scale_fill_manual(name="Groups", values=my_col, guide=F)

	# save file
		if(save_plots){
			tiff(paste(pathPlots,"FigureCountryPred ",my_FigureNumber," ",my_boost,"_per100000FV.tif",sep=""), width=18, height=15, units="cm", pointsize=8, compression="lzw", res=resolution)
			print(my_plot)
			dev.off()
		}
		return(my_plot)
}


