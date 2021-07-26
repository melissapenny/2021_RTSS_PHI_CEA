
# create temporary copy of the working data
	Data_tmp1=Data
		
# subset to the data needed
Data_tmp_CI_temp = list()
for (my_boost in c("without_booster","with_booster")){
	Data_tmp=subset(Data_tmp1, age_granulation=="all_ages")
	Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event %in% c("cost_per_clinical_case_averted","cost_per_death_averted","cost_per_DALY_averted_(undiscounted)","cost_per_DALY_averted_(discounted)")))
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
	Data_tmp_CI$Event_lab <- factor(Data_tmp_CI$Event_lab, levels = c("clinical case","death","DALY (undiscounted)","DALY (discounted)"))
	
# restrict uncertainty
	#hist(Data_tmp_CI[Data_tmp_CI$Event_lab=="clinical case","Q0.975"])
  Data_tmp_CI[(Data_tmp_CI$Event_lab=="clinical case") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="clinical case") & (Data_tmp_CI$Q0.975 >  600),"Q0.975"]=600
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="death") & (Data_tmp_CI$Q0.025 < -500),"Q0.025"]=-500
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="death") & (Data_tmp_CI$Q0.975 >  40000),"Q0.975"]= 40000
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="DALY (undiscounted)") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="DALY (undiscounted)") & (Data_tmp_CI$Q0.975 >  1500),"Q0.975"]= 1500
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="DALY (discounted)") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_lab=="DALY (discounted)") & (Data_tmp_CI$Q0.975 >  1500),"Q0.975"]= 1500
  
  	Data_tmp_CI_temp[[my_boost]] = Data_tmp_CI
  	rm(Data_tmp_CI)
  	
}  	
Data_tmp_CI_all = rbind(Data_tmp_CI_temp[["without_booster"]],Data_tmp_CI_temp[["with_booster"]])
levels(Data_tmp_CI_all$Vaccine)
Data_tmp_CI_all$Vaccine <- gsub("without_booster", "without booster", Data_tmp_CI_all$Vaccine)
Data_tmp_CI_all$Vaccine <- gsub("with_booster", "with booster", Data_tmp_CI_all$Vaccine)
Data_tmp_CI_all$Vaccine <- factor(Data_tmp_CI_all$Vaccine, levels = c("with booster", "without booster","baseline"))
	  
Data_tmp_CI=subset(Data_tmp_CI_all, Event =="cost_per_DALY_averted_(undiscounted)")
Data_tmp_CI_5=subset(Data_tmp_CI, Vaccine_price =="5")
# plot
	my_plot = ggplot(data= Data_tmp_CI_5, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, group=factor(Vaccine_price), colour=Group)) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				geom_errorbar(stat="identity") +
				facet_grid(Vaccine ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				xlim(c(0,maxPrevLimitPlots)) +ylim(c(-70,500))  + theme_bw() + scale_fill_discrete(name="Vaccine price in USD") +
	      scale_color_manual(name="Groups", values=my_col, guide=F)
	      
	   my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))   

	   
	   if(NumGroupsToPlot == 4){
	     plot_width = 18
	     plot_height = 9
	   }
	   if(NumGroupsToPlot == 2){
	     plot_width = 12
	     plot_height = 9
	   }
	   if(NumGroupsToPlot == 1){
	     plot_width = 9
	     plot_height = 9
	   }					
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a_CEA_DALYs_errorbars.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
	#plot
	Data_tmp_CI$Vaccine_price=as.factor(Data_tmp_CI$Vaccine_price)
	
				
my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour= Group, shape= Vaccine_price)) +
				geom_line(stat="identity", aes(group=interaction(Vaccine, Vaccine_price), linetype=Vaccine)) +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + 
				xlim(c(0,maxPrevLimitPlots))  + theme_bw() + scale_shape_discrete(name="Vaccine price in USD")+
    	  scale_color_manual(name="Groups", values=my_col, guide=F)		
    	  
    my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))	  		

    
    if(NumGroupsToPlot == 4){
      plot_width = 18
      plot_height = 9
    }
    if(NumGroupsToPlot == 2){
      plot_width = 18
      plot_height = 9
    }
    if(NumGroupsToPlot == 1){
      plot_width = 18
      plot_height = 9
    }					
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a_CEA_DALYs_noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
		
my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour= Group, shape= Vaccine_price)) +
	  geom_hline(yintercept=100, color="darkgrey") +
	  geom_hline(yintercept=200, color="darkgrey", linetype="dashed") +
	  geom_hline(yintercept=300, color="darkgrey", linetype="dotted") +		
    geom_line(stat="identity", aes(group=interaction(Vaccine, Group), linetype=Vaccine)) +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(~ Vaccine_price, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				coord_cartesian(xlim=c(0,maxPrevLimitPlots), ylim=c(-70,1000)) + theme_bw() +
    	  scale_color_manual(name="Groups", values=my_col)				
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a1_CEA_DALYs_noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour= Group, shape= Vaccine_price)) +
	  geom_hline(yintercept=100, color="darkgrey") +
	  geom_hline(yintercept=200, color="darkgrey", linetype="dashed") +
	  geom_hline(yintercept=300, color="darkgrey", linetype="dotted") +		
    geom_line(stat="identity", aes(group=interaction(Vaccine, Group), linetype=Vaccine)) +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(~ Vaccine_price, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				coord_cartesian(xlim=c(0,60), ylim=c(-10,300)) + theme_bw() +
    	  scale_color_manual(name="Groups", values=my_col)				
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
	

	# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a1restrictLimits_CEA_DALYs_noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour= Group, shape= Vaccine_price)) +
	  geom_hline(yintercept=100, color="darkgrey") +
	  geom_hline(yintercept=200, color="darkgrey", linetype="dashed") +
	  geom_hline(yintercept=300, color="darkgrey", linetype="dotted") +		
    geom_line(stat="identity", aes(group=interaction(Vaccine, Group), linetype=Vaccine)) +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(~ Vaccine_price, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				coord_cartesian(xlim=c(0,25), ylim=c(-10,300)) + theme_bw() +
    	  scale_color_manual(name="Groups", values=my_col)				
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a1restrictLimitsPrev_CEA_DALYs_noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}

	
	Data_tmp_CI$Vaccine_price=as.factor(Data_tmp_CI$Vaccine_price)
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour=Group, shape= Vaccine_price)) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(Vaccine ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per DALY averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				xlim(c(0,maxPrevLimitPlots))  + ylim(c(-70,500))+theme_bw() +
	      scale_color_manual(name="Groups", values=my_col)
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7a2_CEA_DALYs_noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
	
	
	