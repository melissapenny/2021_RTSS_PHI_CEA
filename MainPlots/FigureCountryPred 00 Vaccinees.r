
# this is still a test
# in particular I will need to look into the subsetting of the data to find the appropiate data on vaccinees for each group

# subset data
subData = subset(Data, Event=='vaccinees')
#table(subData$Group, subData$Event)
subData = subset(subData, Vaccine=="without_booster")
#table(subData$Group, subData$Event)

#table(subData$Group, subData$Event)
subData = subset(subData, Summary=="median")
#table(subData$Group, subData$Event)

# might need later
# subData$age_granulation[which(subData$Group=="GSK")]="all_ages"
# subData$Age_upper[which(subData$Group=="GSK")]=100
subData = subset(subData, age_granulation=='all_ages')
#table(subData$Group, subData$Event)
# subData [subData$Group%in%c("Imperial","OpenMalaria"),]

# group specific corrections
# Swiss TPH: the correct cumulative values  are in the specific year columns already
#subData[subData$Group=="OpenMalaria",c("cumy05","cumy10","cumy15")]=subData[subData$Group=="OpenMalaria",c("y5","y10","y15")]

# aditional data manipulation
# subData$cumy00=0
subData=subData[,names(subData) %in% c("Group","Transmission","cumy00","cumy05","cumy10","cumy15")]
# df=gather(subData, time, vaccinees,~Group)
df=melt(subData)
names(df)[names(df)=="value"]="vaccinees"
names(df)[names(df)=="variable"]="time"
df$time_num=as.numeric(substr(df$time,5,6))
df = as.data.frame(df)
df$Transmission <- factor(df$Transmission, levels = c("Kenya","Senegal","Tanzania","Ghana","Uganda","BurkinaFaso"))  
df$Transmission <- revalue(df$Transmission, c("Kenya"="Country A", "Senegal"="Country B", "Tanzania"="Country C", "Ghana"="Country D", "Uganda"="Country E", "BurkinaFaso"="Country F"))  


p2<-ggplot(data=df) + geom_point(aes(x=time_num, y=vaccinees, colour=Group)) + facet_wrap(~Transmission)  + 
  geom_line(aes(x=time_num, y=vaccinees, colour=Group)) +  scale_x_continuous("time since the introduction of RTS,S") + 
  scale_y_continuous("number of fully vaccinated children")+ theme_bw() +
  scale_color_manual(name="Groups", values=my_col)


# save file
if(save_plots){
  tiff(paste(pathPlots,"FigureCountryPred 0 ",".tif",sep=""), width=12, height=8, units="cm", pointsize=8, compression="lzw", res=resolution)
  print(p2)
  dev.off()
}
