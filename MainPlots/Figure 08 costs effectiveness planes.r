
# create temporary copy of the working data
	Data_tmp=Data
		
# subset to the data needed
	Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
	Data_tmp=subset(Data_tmp, Event %in% c("cost_per_DALY_averted_(undiscounted)","DALYs_averted"))
  Data_tmp=subset(Data_tmp, Vaccine %in% c("without_booster","with_booster"))
  Data_tmp=subset(Data_tmp, Summary == "median")
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
  
# assuming that the data for DALYs averted is provided once for the three vaccine price scenarios:
  Data_tmp$Vaccine_price[Data_tmp$Event == "DALYs_averted"]=2
	tmp2=Data_tmp[Data_tmp$Event == "DALYs_averted",]
	tmp5=tmp2; tmp5$Vaccine_price=5
	tmp10=tmp2; tmp10$Vaccine_price=10
  Data_tmp=rbind(Data_tmp,tmp5,tmp10)
  
# create a no vaccination data point
  tmpNovacc=Data_tmp[Data_tmp$Vaccine =="with_booster",]
  tmpNovacc$cumy15=0
  tmpNovacc$Vaccine="no_vaccine"
  Data_tmp=rbind(Data_tmp,tmpNovacc)
  
# change names to plot
	Data_tmp$Event=gsub("cost_per_DALY_averted_.undiscounted.","Cost_per_DALY_averted", Data_tmp$Event)
   
# get data in shape for scatter plot : problem: single value for DALYs_averted and 3 for costs_per_...
	Data_tmp_spread=spread_(Data_tmp, "Event", my_time)
	Data_tmp_spread$Costs=Data_tmp_spread$DALYs_averted * Data_tmp_spread$Cost_per_DALY_averted
	Data_tmp_spread$Vaccine_prev=Data_tmp_spread$Vaccine
	Data_tmp_spread$Vaccine=gsub("_"," ",Data_tmp_spread$Vaccine)
  
#plot subset by group, vaccine price, prevalence and booster
	my_plot = ggplot(data=Data_tmp_spread, aes(x=DALYs_averted ,y=Costs, shape=Vaccine, group=Group, colour=Group)) +
				geom_point(stat="identity") + 
        geom_line(stat="identity") +
				facet_grid(Transmission ~ Vaccine_price, labeller=label_parsed) + 
				xlab('DALYs averted') + ylab("Cost in USD") + 
				theme_bw()  + geom_abline(intercept = 0, slope = 100, color="grey") +
				geom_abline(intercept = 0, slope = 200, color="grey", linetype = "dashed") +
				geom_abline(intercept = 0, slope = 300, color="grey", linetype = "dotted") +
	      scale_color_manual(name="Groups", values=my_col, guide=F)
			
# save file
	if(save_plots){
		tiff(paste(pathPlots,"Figure 8.tif",sep=""), width=18, height=15, units="cm", pointsize=8, compression="lzw", res=resolution)
		print(my_plot)
		dev.off()
	}
rm(my_plot)
# create incremental plot
	Data_tmp_spread_costs=Data_tmp_spread[,c("Group","Transmission","Vaccine","Vaccine_price","Costs")]
	tmp=Data_tmp_spread_costs %>% spread(Vaccine, Costs)
  tmp[,"with booster"]=tmp[,"with booster"]-tmp[,"without booster"]
	Data_tmp_spread_costs=tmp %>% gather(Vaccine,Costs,4:6)
	
  Data_tmp_spread_dalys=Data_tmp_spread[,c("Group","Transmission","Vaccine","Vaccine_price","DALYs_averted")]
	tmp=Data_tmp_spread_dalys %>% spread(Vaccine, DALYs_averted)
	tmp[,"with booster"]=tmp[,"with booster"]-tmp[,"without booster"]
	Data_tmp_spread_dalys=tmp %>% gather(Vaccine,DALYs_averted,4:6)
	
	Data_tmp_spread_incremental=merge(Data_tmp_spread_dalys, Data_tmp_spread_costs)
  
	#plot subset by group, vaccine price, prevalence and booster
	my_plot = ggplot(data=Data_tmp_spread_incremental, aes(x=DALYs_averted ,y=Costs, shape=Vaccine)) +
	  geom_point(stat="identity", aes(color=Group)) + 
	  geom_line(data=subset(Data_tmp_spread_incremental, Vaccine!="with booster"), aes(x=DALYs_averted ,y=Costs, group=Group, colour=Group), stat="identity") +
	  geom_line(data=subset(Data_tmp_spread_incremental, Vaccine!="without booster"), aes(x=DALYs_averted ,y=Costs, group=Group, colour=Group), stat="identity") +
	  facet_grid(Transmission ~ Vaccine_price, labeller=label_parsed) + 
	  xlab('Incremental DALYs averted') + ylab("Incremental cost in USD") + 
	  theme_bw()  + 
    geom_abline(intercept = 0, slope = 100, color="grey") + 
    geom_abline(intercept = 0, slope = 200, color="grey", linetype = "dashed") +
    geom_abline(intercept = 0, slope = 300, color="grey", linetype = "dotted") +
	  scale_color_manual(name="Groups", values=my_col, guide=F)  
  
	# save file
	if(save_plots){
	  tiff(paste(pathPlots,"Figure 8incr.tif",sep=""), width=18, height=15, units="cm", pointsize=8, compression="lzw", res=resolution)
	  print(my_plot)
	  dev.off()
	}
	
# produce TABLE of results for PfPr=30
	
	write.csv(Data_tmp_spread_incremental[Data_tmp_spread_incremental$Transmission=="italic('Pf')*PR[2-10] == 30",], paste(pathPlots,"Figure 8incr TABLE pf30.csv",sep=""))
	
  
	write.csv(Data_tmp_spread_incremental, paste(pathPlots,"Figure 8incr TABLE pfall.csv",sep=""))