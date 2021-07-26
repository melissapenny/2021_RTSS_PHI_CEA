
# create temporary copy of the working data
  Data_sav=Data
  Data_tmp=Data
		
# subset to the data needed
  Data_tmp=subset(Data_tmp, Vaccine %in% c("with_booster", "without_booster"))
  
  
	levels(Data_tmp$Vaccine)
	Data_tmp$Vaccine <- gsub("without_booster", "without booster", Data_tmp$Vaccine)
	Data_tmp$Vaccine <- gsub("with_booster", "with booster", Data_tmp$Vaccine)
	Data_tmp$Vaccine <- factor(Data_tmp$Vaccine, levels = c("with booster", "without booster","baseline"))

  Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
	Data_tmp=subset(Data_tmp,  (Event %in% c("cost_per_DALY_averted_(undiscounted)")))
	Data_tmp=subset(Data_tmp, Vaccine_price =="5")
	no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  
  
# get confidence intervals in shape
	Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
  
# prepare plot
	Data_tmp_CI$prevalence=as.integer(substr(Data_tmp_CI$Transmission,5,10))
	Data_tmp_CI$Event_lab=substr(Data_tmp_CI$Event,6,100)
	Data_tmp_CI$Event_lab=gsub("_"," ",Data_tmp_CI$Event_lab)
	Data_tmp_CI$Event_lab=gsub("per ","",Data_tmp_CI$Event_lab)
	Data_tmp_CI$Event_lab=gsub(" averted","",Data_tmp_CI$Event_lab)

 # plot
	my_plot = ggplot(data= Data_tmp_CI, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group, fill=Group)) +
      geom_hline(yintercept=100, color="darkgrey") +
      geom_hline(yintercept=200, color="darkgrey", linetype="dashed") +
      geom_hline(yintercept=300, color="darkgrey", linetype="dotted") +
        geom_line(stat="identity") +
	      geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Vaccine ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + 
				coord_cartesian(xlim=c(0,maxPrevLimitPlots),ylim=c(-70,500))   + theme_bw() +
        scale_fill_manual(name="Groups", values=my_col, guide=F) +
        scale_color_manual(name="Groups", values=my_col, guide=F) 
        
    my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))    
        
    if(NumGroupsToPlot == 4){
      plot_width = 18
      plot_height = 11
    }
    if(NumGroupsToPlot == 2){
      plot_width = 15
      plot_height = 11
    }
    if(NumGroupsToPlot == 1){
      plot_width = 8
      plot_height = 11
    }		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7b_CEA_DALYs_witherror_compare_nocountry.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	# plot
	my_plot_res = ggplot(data= Data_tmp_CI, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group, fill=Group)) +
      geom_hline(yintercept=100, color="darkgrey") +
      geom_hline(yintercept=200, color="darkgrey", linetype="dashed") +
      geom_hline(yintercept=300, color="darkgrey", linetype="dotted") +
        geom_line(stat="identity") +
	      geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Vaccine ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + 
				coord_cartesian(xlim=c(0,25),ylim=c(-20,300))   + theme_bw() +
        scale_fill_manual(name="Groups", values=my_col, guide=F) +
        scale_color_manual(name="Groups", values=my_col, guide=F) 
        
	my_plot_res = my_plot_res + xlab(expression(italic(Pf)*PR[2-10]))  	
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7b_CEA_DALYs_witherror_compare_nocountry_restrictPrev.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot_res)
		dev.off()
	}

	

  
  
#revert overwritten Data from the country specific dataset
  Data=Data_sav
  
	
