
# create temporary copy of the working data
	Data_sav=Data
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
	
# restrict uncertainty
	#hist(Data_tmp_CI[Data_tmp_CI$Event_lab=="clinical case","Q0.975"])
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.975 >  400000),"Q0.975"]=400000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.025 < -500),"Q0.025"]=-500
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.975 >  10000),"Q0.975"]= 10000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.975 >  5000),"Q0.975"]= 5000
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.975 >  1500),"Q0.975"]= 1500
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.975 >  100000),"Q0.975"]= 100000
	Data_tmp_CI_5prev=Data_tmp_CI
	
######## get the cuntry specific data
	# load(paste(pathData, nameOfCountryDataFrame, sep=""))
	# # create temporary copy of the working data
	# Data_tmp=Data
	# # subset to the data needed
	# Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
	# Data_tmp=subset(Data_tmp, (Vaccine==my_boost) & (Event %in% c("clinical_cases_averted_per_100000_vac","severe_cases_averted_per_100000_vac","hospitalised_cases_averted_per_100000_vac","deaths_averted_per_100000_vac","DALYs_averted_per_100000_vac")))
	# no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
	# no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
	# Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]  
	# # get confidence intervals in shape
	# Data_tmp_CI = spread_(Data_tmp, "Summary", my_time)
	# # prepare plot
	# Data_tmp_CI$prevalence=as.integer(Data_tmp_CI$prev_2_10)
	# Data_tmp_CI$Event_clean=gsub("_"," ", Data_tmp_CI$Event)
	# Data_tmp_CI$Event_clean=gsub(" averted per 100000 vac","", Data_tmp_CI$Event_clean)
	# Data_tmp_CI$Event_clean <- factor(Data_tmp_CI$Event_clean, levels = c("clinical cases","severe cases","hospitalised cases","deaths","DALYs"))
	# Data_tmp_CI$Group=gsub("_dummy","", Data_tmp_CI$Group)
	# Data_tmp_CI$Transmission <- revalue(Data_tmp_CI$Transmission, c("Kenya"="A", "Senegal"="B", "Tanzania"="c", "Ghana"="D", "Uganda"="E", "BurkinaFaso"="F"))  
	
# # restrict uncertainty
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="clinical cases") & (Data_tmp_CI$Q0.975 >  400000),"Q0.975"]=400000
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.025 < -500),"Q0.025"]=-500
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="severe cases") & (Data_tmp_CI$Q0.975 >  10000),"Q0.975"]= 10000
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="hospitalised cases") & (Data_tmp_CI$Q0.975 >  5000),"Q0.975"]= 5000
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="deaths") & (Data_tmp_CI$Q0.975 >  1500),"Q0.975"]= 1500
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.025 < -200),"Q0.025"]=-200
# 	Data_tmp_CI[(Data_tmp_CI$Event_clean=="DALYs") & (Data_tmp_CI$Q0.975 >  100000),"Q0.975"]= 100000

	
 
######  restrict data/ plots to a single outcome - 
	#Data_tmp_CI_5prev=subset(Data_tmp_CI_5prev, Event_clean=="severe cases")
	#Data_tmp_CI=subset(Data_tmp_CI, Event_clean=="severe cases")
  
#plot
	my_plot = ggplot(data=Data_tmp_CI_5prev, aes(x=prevalence ,y=median, ymin=Q0.025, ymax=Q0.975, colour=Group, fill=Group)) +
				geom_line(stat="identity") +
				#geom_point(stat="identity") + 
				geom_ribbon(alpha=0.3, colour=NA) +
				facet_grid(Event_clean ~ Group, scales="free_y") + 
				xlab('Prevalence') + ylab("Events averted per 100,000 vaccinated ") + 
				xlim(c(0,maxPrevLimitPlots)) + theme_bw() + 
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
	    tiff(paste(pathPlots,"Figure 6 combined_nocountrydata ",my_boost,"_per100000FV.tif",sep=""), width=plot_width, height=plot_height, units="cm", pointsize=8, compression="lzw", res=resolution)
	    print(my_plot)
	    dev.off()
	  }
	

  
#revert overwritten Data from the country specific dataset
  Data=Data_sav
  