
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
	Data_tmp_CI$prevalence=as.integer(substr(Data_tmp_CI$Transmission,5,10))
	Data_tmp_CI$Event_clean=gsub("_"," ", Data_tmp_CI$Event)
	Data_tmp_CI$Event_clean=gsub(" averted per 100000 vac","", Data_tmp_CI$Event_clean)
	Data_tmp_CI$Event_clean <- factor(Data_tmp_CI$Event_clean, levels = c("clinical cases","severe cases","hospitalised cases","deaths","DALYs"))
	Data_tmp_CI_tab=Data_tmp_CI
  
# restrict uncertainty
	#hist(Data_tmp_CI[Data_tmp_CI$Event_lab=="clinical case","Q0.975"])
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.975 >  400000),"Q0.975"]=400000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.025 < -500),"Q0.025"]=-500
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.975 >  10000),"Q0.975"]= 10000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.975 >  5000),"Q0.975"]= 5000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.975 >  2000),"Q0.975"]= 2000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.975 >  150000),"Q0.975"]= 150000
	
  
#plot
	my_plot = ggplot(data=Data_tmp_CI, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group, fill=Group)) +
				geom_line(stat="identity") +
				geom_point(stat="identity") + 
				geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Event_clean ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Events averted per 100,000 vaccinated ") + 
				xlim(c(0,maxPrevLimitPlots)) + theme_bw()+ 
        scale_color_manual(name="Groups",values=my_col, guide=FALSE) + 
	      scale_fill_manual(name="Groups",values=my_col, guide=FALSE)
	my_plot = my_plot + xlab(expression(italic(Pf)*PR[2-10]))		
	
	
	
	if(NumGroupsToPlot == 4){
	  plot_width = 23
	  plot_height = 18
	}
	if(NumGroupsToPlot == 2){
	  plot_width = 13
	  plot_height = 18
	}
	if(NumGroupsToPlot == 1){
	  plot_width = 7
	  plot_height = 18
	}
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 6 ",my_boost,"_per100000FV.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
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
	  Data_tmp_CI_tab=subset(Data_tmp_CI_tab, prev_2_10 %in% c(3,10,30,50,prevHighNum))
    Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","prev_2_10","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
    res=Data_tmp_CI_tab %>%
      gather(Var, val, median:Q0.975) %>% 
      unite_("prev_2_10_range", c("prev_2_10","Var")) %>% 
      spread(prev_2_10_range, val)
	# res=Data_tmp_CI_tab %>%
      # gather(Var, val, median:Q0.975) %>% 
      # unite_("prev_2_10_range", c("prev_2_10","variable")) %>% 
      # spread(prev_2_10_range, value)

    res=res[,c(0,4,5,6,1,2,3,7:15)+1]
    res[,2:16]=round(res[,2:16])
    df=data.frame(group=res[,1],
                  prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
                  prev10=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
                  prev30=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
                  prev50=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
                  tempHighPrev=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
          names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
    write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_",my_boost,".csv",sep=""))  
    
    df=data.frame(group=res[,1],
                  prev3=paste(res[,2], sep=""),
                  prev10=paste(res[,5], sep=""),
                  prev30=paste(res[,8], sep=""),
                  prev50=paste(res[,11], sep=""),
                  tempHighPrev=paste(res[,14], sep=""))
          names(df)[names(df)=="tempHighPrev"]=paste("prev", prevHighNum,sep="")
    write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_",my_boost,"_MediansOnly.csv",sep=""))  
    

    
     # make table
	  Data_tmp_CI_tab=subset(Data_tmp_CI_tab_beforeSubset, Event_clean== evTyp)
	  Data_tmp_CI_tab=subset(Data_tmp_CI_tab, prev_2_10 %in% c(3,5,7.5,10,15))
    Data_tmp_CI_tab=Data_tmp_CI_tab[,c("Group","prev_2_10","median","Q0.025","Q0.975")] #this can be easily changed to have the CIs
    res=Data_tmp_CI_tab %>%
      gather(Var, val, median:Q0.975) %>% 
      unite_("prev_2_10_range", c("prev_2_10","Var")) %>% 
      spread(prev_2_10_range, val)
	# res=Data_tmp_CI_tab %>%
      # gather(Var, val, median:Q0.975) %>% 
      # unite_("prev_2_10_range", c("prev_2_10","variable")) %>% 
      # spread(prev_2_10_range, value)

    res=res[,c(0,7:15,1,2,3,4,5,6)+1]
    res[,2:16]=round(res[,2:16])
    df=data.frame(group=res[,1],
                  prev3=paste(res[,2]," (",res[,3]," to ",res[,4],")", sep=""),
                  prev5=paste(res[,5]," (",res[,6]," to ",res[,7],")", sep=""),
                  prev7.5=paste(res[,8]," (",res[,9]," to ",res[,10],")", sep=""),
                  prev10=paste(res[,11]," (",res[,12]," to ",res[,13],")", sep=""),
                  prev15=paste(res[,14]," (",res[,15]," to ",res[,16],")", sep=""))
    write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_lowPrev_",my_boost,".csv",sep=""))  
	df=data.frame(group=res[,1],
                  prev3=paste(res[,2], sep=""),
                  prev5=paste(res[,5], sep=""),
                  prev7.5=paste(res[,8], sep=""),
                  prev10=paste(res[,11], sep=""),
                  prev15=paste(res[,14], sep=""))
    write.csv( df, paste(pathPlots,"Table_",nameTable[[evTyp]],"per100kvaccaverted_lowPrev_",my_boost,"_MediansOnly.csv",sep=""))  


}
  