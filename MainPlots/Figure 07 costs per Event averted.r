
# create temporary copy of the working data
	Data_tmp=Data
		
# subset to the data needed
	Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
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
	Data_tmp_CI_sav=Data_tmp_CI
  
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
  
   Data_tmp_CI[["Vaccine price"]] =  Data_tmp_CI[["Vaccine_price"]]
   Data_tmp_CI[["Vaccine price"]] = factor(Data_tmp_CI[["Vaccine price"]])
#plot
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, group=factor(Vaccine_price), color=Group, shape=factor(Vaccine_price))) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Event_lab ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per events averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				xlim(c(0,maxPrevLimitPlots))  + theme_bw() + scale_fill_discrete(name="Vaccine price in USD") + 
	      ylim(c(-10,NA) )+   #scale_shape_discrete(name="Vaccine price in USD")+
    	  scale_color_manual(name="Groups", values=my_col)
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
	
	
	if(NumGroupsToPlot == 4){
	  plot_width = 23
	  plot_height = 18
	}
	if(NumGroupsToPlot == 2){
	  plot_width = 18
	  plot_height = 18
	}
	if(NumGroupsToPlot == 1){
	  plot_width = 10
	  plot_height = 18
	}		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7 ",my_boost,".tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
	
	
	#plot
	Data_tmp_CI$Vaccine_price=as.factor(Data_tmp_CI$Vaccine_price)
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, group=factor(Vaccine_price), colour= Group, shape=factor(Vaccine_price))) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				# geom_ribbon(alpha=0.3, colour="white") +
				facet_grid(Event_lab ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Cost in USD per events averted") + scale_shape_discrete(name="Vaccine price in USD")+ 
				xlim(c(0,maxPrevLimitPlots))  + theme_bw() +
	      ylim(c(-10,NA) )+
	      scale_color_manual(name="Groups", values=my_col)
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))	
	
	if(NumGroupsToPlot == 4){
	  plot_width = 23
	  plot_height = 18
	}
	if(NumGroupsToPlot == 2){
	  plot_width = 18
	  plot_height = 18
	}
	if(NumGroupsToPlot == 1){
	  plot_width = 10
	  plot_height = 18
	}		
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 7 ",my_boost," noCI.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}


for(VacPrice in c(2,5,10)){
	# table for qualys
		Data_tmp_CI=Data_tmp_CI_sav
		if(update_tables){
		  Data_tmp_CI=subset(Data_tmp_CI, Event_lab %in% c("DALY (undiscounted)"))
		  Data_tmp_CI=subset(Data_tmp_CI, prev_2_10 %in% c(3,10,30,50,prevHighNum))
		  Data_tmp_CI_tab=subset(Data_tmp_CI, Vaccine_price == VacPrice)
		  Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","Transmission","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
		  res=Data_tmp_CI_tab %>%
		    gather(Var, val, median:Q0.975) %>% 
		    unite_("Transmission_range", c("Transmission","Var")) %>% 
		    spread(Transmission_range, val)
		    
		  # res=Data_tmp_CI_tab %>%
		    # gather(Var, val, median:Q0.975) %>% 
		    # unite_("Transmission_range", c("Transmission","variable")) %>% 
		    # spread(Transmission_range, value)  
		  res[,2:16]=round(res[,2:16])
		  res=res[,c(0,4,5,6,1,2,3,7:15)+1]
		  df=data.frame(group=res[,1],
		                prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
		                prev10=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
		                prev30=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
		                prev50=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
		                tempHighPrev=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
		names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
	    write.csv(df,paste(pathPlots,"Table_costperdaly_", VacPrice,"_",my_boost,".csv",sep=""))
	    df=data.frame(group=res[,1],
		                prev3=paste(res[,2], sep=""),
		                prev10=paste(res[,5], sep=""),
		                prev30=paste(res[,8], sep=""),
		                prev50=paste(res[,11], sep=""),
		                tempHighPrev=paste(res[,14], sep=""))
		names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
	    write.csv(df,paste(pathPlots,"Table_costperdaly_", VacPrice,"_",my_boost,"_MediansOnly.csv",sep=""))
		
		}
		
		
		# table for qualys
		Data_tmp_CI=Data_tmp_CI_sav
		if(update_tables){
		  Data_tmp_CI=subset(Data_tmp_CI, Event_lab %in% c("DALY (undiscounted)"))
		  Data_tmp_CI=subset(Data_tmp_CI, prev_2_10 %in% c(3,5,7.5,10,15))
		  Data_tmp_CI_tab=subset(Data_tmp_CI, Vaccine_price == VacPrice)
		  Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","Transmission","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
		  res=Data_tmp_CI_tab %>%
		    gather(Var, val, median:Q0.975) %>% 
		    unite_("Transmission_range", c("Transmission","Var")) %>% 
		    spread(Transmission_range, val)
		    
		  # res=Data_tmp_CI_tab %>%
		    # gather(Var, val, median:Q0.975) %>% 
		    # unite_("Transmission_range", c("Transmission","variable")) %>% 
		    # spread(Transmission_range, value)  
		  res[,2:16]=round(res[,2:16])
		  res=res[,c(0,7:15,1,2,3,4,5,6)+1]
		  df=data.frame(group=res[,1],
		                prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
		                prev5=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
		                prev7.5=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
		                prev10=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
		                prev15=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
		  
	    write.csv(df,paste(pathPlots,"Table_costperdaly_", VacPrice,"_lowPrev_",my_boost,".csv",sep=""))
	    
	    df=data.frame(group=res[,1],
		                prev3=paste(res[,2], sep=""),
		                prev5=paste(res[,5], sep=""),
		                prev7.5=paste(res[,8], sep=""),
		                prev10=paste(res[,11], sep=""),
		                prev15=paste(res[,14], sep=""))
		  
	    write.csv(df,paste(pathPlots,"Table_costperdaly_", VacPrice,"_lowPrev_",my_boost,"_MediansOnly.csv",sep=""))

		}
	
}	



for(VacPrice in c(2,5,10)){
	# table for qualys
		Data_tmp_CI=Data_tmp_CI_sav
		if(update_tables){
		  Data_tmp_CI=subset(Data_tmp_CI, Event_lab %in% c("clinical case"))
		  Data_tmp_CI=subset(Data_tmp_CI, prev_2_10 %in% c(3,10,30,50,prevHighNum))
		  Data_tmp_CI_tab=subset(Data_tmp_CI, Vaccine_price == VacPrice)
		  Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","Transmission","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
		  res=Data_tmp_CI_tab %>%
		    gather(Var, val, median:Q0.975) %>% 
		    unite_("Transmission_range", c("Transmission","Var")) %>% 
		    spread(Transmission_range, val)
		    
		  # res=Data_tmp_CI_tab %>%
		    # gather(Var, val, median:Q0.975) %>% 
		    # unite_("Transmission_range", c("Transmission","variable")) %>% 
		    # spread(Transmission_range, value)  
		  res[,2:16]=round(res[,2:16])
		  res=res[,c(0,4,5,6,1,2,3,7:15)+1]
		  df=data.frame(group=res[,1],
		                prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
		                prev10=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
		                prev30=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
		                prev50=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
		                tempHighPrev=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
		names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
	    write.csv(df,paste(pathPlots,"Table_costperClinicalCase_", VacPrice,"_",my_boost,".csv",sep=""))
	    df=data.frame(group=res[,1],
		                prev3=paste(res[,2], sep=""),
		                prev10=paste(res[,5], sep=""),
		                prev30=paste(res[,8], sep=""),
		                prev50=paste(res[,11], sep=""),
		                tempHighPrev=paste(res[,14], sep=""))
		names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
	    write.csv(df,paste(pathPlots,"Table_costperClinicalCase_", VacPrice,"_",my_boost,"_MediansOnly.csv",sep=""))
		
		}
		
		
		# table for qualys
		Data_tmp_CI=Data_tmp_CI_sav
		if(update_tables){
		  Data_tmp_CI=subset(Data_tmp_CI, Event_lab %in% c("clinical case"))
		  Data_tmp_CI=subset(Data_tmp_CI, prev_2_10 %in% c(3,5,7.5,10,15))
		  Data_tmp_CI_tab=subset(Data_tmp_CI, Vaccine_price == VacPrice)
		  Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","Transmission","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
		  res=Data_tmp_CI_tab %>%
		    gather(Var, val, median:Q0.975) %>% 
		    unite_("Transmission_range", c("Transmission","Var")) %>% 
		    spread(Transmission_range, val)
		    
		  # res=Data_tmp_CI_tab %>%
		    # gather(Var, val, median:Q0.975) %>% 
		    # unite_("Transmission_range", c("Transmission","variable")) %>% 
		    # spread(Transmission_range, value)  
		  res[,2:16]=round(res[,2:16])
		  res=res[,c(0,7:15,1,2,3,4,5,6)+1]
		  df=data.frame(group=res[,1],
		                prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
		                prev5=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
		                prev7.5=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
		                prev10=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
		                prev15=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
		  
	    write.csv(df,paste(pathPlots,"Table_costperClinicalCase_", VacPrice,"_lowPrev_",my_boost,".csv",sep=""))
	    
	    df=data.frame(group=res[,1],
		                prev3=paste(res[,2], sep=""),
		                prev5=paste(res[,5], sep=""),
		                prev7.5=paste(res[,8], sep=""),
		                prev10=paste(res[,11], sep=""),
		                prev15=paste(res[,14], sep=""))
		  
	    write.csv(df,paste(pathPlots,"Table_costperClinicalCase_", VacPrice,"_lowPrev_",my_boost,"_MediansOnly.csv",sep=""))

		}
	
}	
	