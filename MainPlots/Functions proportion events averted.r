
#function to plot events averted

EventsAvertedPlot <- function(Data, my_boost, my_Event, my_FigureNumber, my_time, save_plots=T ){
	# create temporary copy of the working data
		Data_tmp=Data
    
	# subset to the data needed
    Data_tmp=subset(Data_tmp, age_granulation=="fine")
    Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event==my_Event))
	  Data_tmp=subset(Data_tmp, Transmission %in% paste("prev",c(3,10,30,50,prevHighNum),sep=""))
	  Data_tmp$Transmission <- factor(Data_tmp$Transmission, levels = paste("prev",c(3,10,30,50,prevHighNum),sep=""))
	  
	  ##-----
	  Data_tmp$Transmission <- gsub("prev", "italic('Pf')*PR[2-10] == ",Data_tmp$Transmission)
	  Data_tmp$Transmission <- factor(Data_tmp$Transmission, levels = paste("italic('Pf')*PR[2-10] == ",c(3,10,30,50,prevHighNum),sep=""))
	  Data_tmp$Group <- gsub("EMOD DTK", "EMOD~DTK",Data_tmp$Group)
	  Data_tmp$Group <- factor(Data_tmp$Group, levels = c("EMOD~DTK", "GSK",  "Imperial", "OpenMalaria"))
	  ##-----	  
	  
	  no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	  no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	  Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  	
	  
  # create age group size information for barplot
	  Data_tmp$barWidths = Data_tmp$Age_upper - Data_tmp$Age_lower
	  Data_tmp$barMids = Data_tmp$Age_lower + Data_tmp$barWidths/2
  
  # get confidence intervals in shape
  	Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
  	
 	#plot
		my_plot = ggplot(data=Data_tmp_CI, aes(x=barMids,y=median ,width=barWidths, ymin=Q0.025, ymax=Q0.975, fill=Group)) +
					geom_bar(stat="identity", colour="white") + 
					geom_errorbar(width=.01, size=.25, color="black") +
					facet_grid(Transmission ~ Group, labeller=label_parsed, scales="free_y") + 
					xlab('Age') + ylab(paste(gsub("_"," ",Data_tmp_CI$Event[1]),'cinated', sep="")) + 
					xlim(c(0,20)) + theme_bw() + scale_fill_manual(name="Groups", values=my_col, guide=F) +
		      ylim(c(-50,90))

		if(NumGroupsToPlot == 4){
		  plot_width = 18
		  plot_height = 15
		}
		if(NumGroupsToPlot == 2){
		  plot_width = 11
		  plot_height = 15
		}
		if(NumGroupsToPlot == 1){
		  plot_width = 6
		  plot_height = 15
		}
	# save file
		if(save_plots){
		  tiff(paste(pathPlots,"Figure ",my_FigureNumber," ",my_boost,"_propAverted.tif",sep=""),  width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
			print(my_plot)
			dev.off()
		}
		return(my_plot)
}


