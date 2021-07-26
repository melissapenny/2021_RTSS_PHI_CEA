
# create temporary copy of the working data
	Data_tmp=Data
		
# subset to the data needed
	Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
	Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event %in% c("clinical_cases_averted","severe_cases_averted","hospitalised_cases_averted","deaths_averted","DALYs_averted")))
	no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  
  
# get confidence intervals in shape
	Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
  
# prepare plot
	Data_tmp_CI$prevalence=as.integer(substr(Data_tmp_CI$Transmission,5,10))
	Data_tmp_CI$Event_lab=gsub("........$","",Data_tmp_CI$Event)
	Data_tmp_CI$Event_lab <- factor(Data_tmp_CI$Event_lab, levels = c("clinical_cases","severe_cases","hospitalised_cases","deaths","DALYs"))
	
#plot
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group, fill=Group)) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Event_lab ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Events averted (aggregated by age)") + 
				xlim(c(0,maxPrevLimitPlots)) + theme_bw()+ 
        scale_color_manual(name="Groups",values=my_col, guide=FALSE) + 
	      scale_fill_manual(name="Groups",values=my_col, guide=FALSE)
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
	
	
	if(NumGroupsToPlot == 4){
	  plot_width = 18
	  plot_height = 15
	}
	if(NumGroupsToPlot == 2){
	  plot_width = 15
	  plot_height = 15
	}
	if(NumGroupsToPlot == 1){
	  plot_width = 10
	  plot_height = 15
	}
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 6 ",my_boost,".tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}