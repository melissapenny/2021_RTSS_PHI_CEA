# Inserted by CB in case not running the master script
# library(dplyr)
# library(tidyr)
library(reshape2)
# library(ggplot2)

# create temporary copy of the working data
Data_tmp=Data

# Inserted by CB in case not running the master script
# my_time = "cumy15"

# Define units of DALYs averted
daly_name <- "DALYs_averted_per_100000_vac"

# subset to the data needed
Data_tmp=subset(Data_tmp, age_granulation=="all_ages")
Data_tmp=subset(Data_tmp, Event %in% c("cost_per_DALY_averted_(undiscounted)",
                                       daly_name))
Data_tmp=subset(Data_tmp, Vaccine %in% c("without_booster","with_booster"))
Data_tmp=subset(Data_tmp, Summary %in% c("median","Q0.025","Q0.975"))
Data_tmp=subset(Data_tmp, Transmission %in% paste("prev",c(3,10,30,50,prevHighNum),sep=""))
Data_tmp$Transmission <- factor(Data_tmp$Transmission, levels = paste("prev",c(3,10,30,50,prevHighNum),sep=""))
no_need_value_label=c(paste("y",0:15,sep=""),"cumy05","cumy10","cumy15")
no_need_value_label=no_need_value_label[-which(no_need_value_label==my_time)]
Data_tmp = Data_tmp[, - which(names(Data_tmp) %in% no_need_value_label)]

# assuming that the data for DALYs averted is provided once for the three vaccine price scenarios:
Data_tmp$Vaccine_price[Data_tmp$Event == daly_name]=2
tmp2=Data_tmp[Data_tmp$Event == daly_name,]
tmp5=tmp2; tmp5$Vaccine_price<-5
tmp10=tmp2; tmp10$Vaccine_price=10
Data_tmp=rbind(Data_tmp,tmp5,tmp10)

# create a no vaccination data point
tmpNovacc=Data_tmp[Data_tmp$Vaccine =="with_booster",]
tmpNovacc$cumy15=0
tmpNovacc$Vaccine="no_vaccine"
Data_tmp=rbind(Data_tmp,tmpNovacc)

# change names to plot
Data_tmp$Event=gsub(daly_name,
                    "DALYs_averted", Data_tmp$Event)
Data_tmp$Event=gsub("cost_per_DALY_averted_.undiscounted.",
                    "Cost_per_DALY_averted", Data_tmp$Event)

# get data in shape for scatter plot : problem: single value for DALYs_averted and 3 for costs_per_...
Data_tmp_spread=spread_(Data_tmp, "Event", my_time)
Data_tmp_spread$Costs=Data_tmp_spread$DALYs_averted * Data_tmp_spread$Cost_per_DALY_averted

# added by CB to compute incremental cost effectiveness
costDF <- Data_tmp_spread[,c("Group","Transmission","Vaccine","Vaccine_price",
                                 "Summary","Costs")]
dalyDF <- Data_tmp_spread[,c("Group","Transmission","Vaccine","Vaccine_price",
                                  "Summary","DALYs_averted")]

costDF <- dcast(costDF,Group+Transmission+Vaccine_price+Summary~Vaccine)
costDF$without_booster <- costDF$without_booster - costDF$no_vaccine
costDF$with_booster <- costDF$with_booster - costDF$without_booster
costDF <- costDF[,setdiff(names(costDF),"no_vaccine")]
costDF <- melt(costDF,measure.vars=c("with_booster","without_booster"),variable.name="Vaccine",value.name="Costs")

dalyDF <- dcast(dalyDF,Group+Transmission+Vaccine_price+Summary~Vaccine)
dalyDF$without_booster <- dalyDF$without_booster- dalyDF$no_vaccine
dalyDF$with_booster <- dalyDF$with_booster - dalyDF$without_booster
dalyDF <- dalyDF[,setdiff(names(dalyDF),"no_vaccine")]
dalyDF <- melt(dalyDF,measure.vars=c("with_booster","without_booster"),variable.name="Vaccine",value.name="DALYs_averted")

names(dalyDF)[names(dalyDF)=="variable"]="Vaccine"
names(dalyDF)[names(dalyDF)=="value"]="DALYs_averted"
names(costDF)[names(costDF)=="variable"]="Vaccine"
names(costDF)[names(costDF)=="value"]="Costs"
allDF <- merge(costDF,dalyDF)

allDF$slope <- allDF$Costs/allDF$DALYs_averted

my_plot <- ggplot(data=subset(allDF,Summary=="median"), aes(x=as.numeric(gsub('prev','',Transmission)) ,
                                                 y=slope, 
                                                 shape=Vaccine, group=Group, 
                                                 colour=Group)) +
  geom_point(stat="identity",size=3) +
  geom_line(stat="identity")+
  facet_grid(Vaccine~Vaccine_price)+
  xlab('Prevalence') + ylab("Delta cost/delta DALYs averted per 100000 vaccinated") +
  theme_bw() +
  scale_color_manual(name="Groups", values=my_col)  


# save file
if(save_plots){
  tiff(paste(pathPlots,"Figure 9.tif",sep=""), width=18, height=15, units="cm", pointsize=8, compression="lzw", res=resolution)
  print(my_plot)
  dev.off()
}
