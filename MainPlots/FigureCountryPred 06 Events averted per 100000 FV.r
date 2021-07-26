
# create temporary copy of the working data
	Data_tmp=Data
		
# subset to the data needed
	Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
	Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event %in% c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac")))
	no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  
  
# get confidence intervals in shape
	Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
  
# prepare plot
	Data_tmp_CI$prevalence=as.integer(Data_tmp_CI$prev_2_10)
	Data_tmp_CI$Event_clean=gsub("_"," ", Data_tmp_CI$Event)
	Data_tmp_CI$Event_clean=gsub(" averted per 100000 vac","", Data_tmp_CI$Event_clean)
	Data_tmp_CI$Event_clean <- factor(Data_tmp_CI$Event_clean, levels = c("clinical cases","severe cases","hospitalised cases","deaths","DALYs"))
	Data_tmp_CI_tab=Data_tmp_CI
  
	Data_tmp_CI$Transmission <- revalue(Data_tmp_CI$Transmission, c("Kenya"="Country A", "Senegal"="Country B", "Tanzania"="Country C", "Ghana"="Country D", "Uganda"="Country E", "BurkinaFaso"="Country F"))  
	
  
#plot  
  my_plot = ggplot(data=Data_tmp_CI, aes(x=Transmission ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group)) +
	  geom_point(stat="identity", size=2) + 
	  geom_errorbar(width=0.2, size=1)+
	  facet_grid(Event_clean ~ Group, scales="free_y") + 
	  xlab('') + ylab("Events averted per 100,000 vaccinated") + 
	  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	  scale_color_manual(name="Groups", values=my_col)
			
# save file
	if(save_plots){
		tiff(paste(pathPlots,"FigureCountryPred 6 ",my_boost,"_per100000FV.tif",sep=""), width=23, height=18, units="cm", pointsize=7, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
  
# make table for events averted 
Data_tmp_CI_tab_beforeSubset = Data_tmp_CI_tab

tableEvents = c("clinical cases", "severe cases", "hospitalised cases", "deaths")
nameTable = list(deaths ="death")
nameTable[["clinical cases"]]= "clinicalCases"
nameTable[["severe cases"]]= "severeCases"
nameTable[["hospitalised cases"]]= "hospitalisedCases"

for(evTyp in tableEvents){
	  Data_tmp_CI_tab=subset(Data_tmp_CI_tab_beforeSubset, Event_clean== evTyp)
	  Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","Transmission","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
	  res=Data_tmp_CI_tab %>%
	    gather(Var, val, median:Q0.975) %>% 
	    unite_("Transmission_range", c("Transmission","Var")) %>% 
	    spread(Transmission_range, val)
	    
	    # res=Data_tmp_CI_tab %>%
	    # gather(Var, val, median:Q0.975) %>% 
	    # unite_("Transmission_range", c("Transmission","variable")) %>% 
	    # spread(Transmission_range, value)
    res=cbind(res[,1],round(res[,c(8:16,5:7,17:19,2:4)]))
	  df=data.frame(group=res[,1],
	                Kenya=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
	                Senegal=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
	                Tanzania=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
	                Ghana=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
	                Uganda=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""),
	                BurkinaFaso=paste(res[,17]," (",res[,18]," to ",res[,19],")", sep=""))
	  names(df)=c("Group",paste("Country", c("A","B","C","D","E","F")))
	  
	  write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_6countries_",my_boost,".csv",sep=""))  
	  
	   df=data.frame(group=res[,1],
                  Kenya=paste(res[,2], sep=""),
                  Senegal=paste(res[,5], sep=""),
                  Tanzania=paste(res[,8], sep=""),
                  Ghana=paste(res[,11], sep=""),
                  Uganda=paste(res[,14], sep=""),
                  BurkinaFaso=paste(res[,17], sep=""))
	 names(df)=c("Group",paste("Country", c("A","B","C","D","E","F")))	  
	  write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_6countries_",my_boost,"_MediansOnly.csv",sep=""))  
}	
	