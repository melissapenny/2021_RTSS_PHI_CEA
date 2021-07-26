
# this is still a test
# in particular I will need to look into the subsetting of the data to find the appropiate data on vaccinees for each group

# subset data
subData = subset(Data, Event=='vaccinees')
#table(subData$Group, subData$Event)
subData = subset(subData, Vaccine=="without_booster")
#table(subData$Group, subData$Event)
subData = subset(subData, Transmission=="prev10")
#table(subData$Group, subData$Event)
subData = subset(subData, Summary=="median")
#table(subData$Group, subData$Event)
subData$age_granulation[which(subData$Group=="GSK")]="all_ages"
subData$Age_upper[which(subData$Group=="GSK")]=100
subData = subset(subData, age_granulation=='all_ages')
#table(subData$Group, subData$Event)

# group specific corrections
# Swiss TPH: the correct cumulative values  are in the specific year columns already
subData[subData$Group=="OpenMalaria",c("cumy05","cumy10","cumy15")]=subData[subData$Group=="OpenMalaria",c("y5","y10","y15")]

# aditional data manipulation
subData$cumy00=0
subData=subData[,names(subData) %in% c("Group","cumy00","cumy05","cumy10","cumy15")]
df=gather(subData, time, vaccinees, -Group)
names(df)=c("Group","time","vaccinees")
df$time_num=as.numeric(substr(df$time,5,6))

# produce plot at y15
p1<-ggplot(subData, aes(x=Group, y=cumy15, fill=Group)) +
  geom_bar(stat = "identity") +
  theme_bw()

#produce plot at y5-15
p2<-ggplot(df, aes(x=time_num, y=vaccinees, group=Group, colour=Group)) +
  geom_point(stat="identity") +
  geom_line(stat="identity") +
  scale_x_continuous("time since the introduction of RTS,S") + scale_y_continuous("number of fully vaccinated children")

# save file
if(save_plots){
  tiff(paste(pathPlots,"Figure 0 ",".tif",sep=""), width=12, height=8, units="cm", pointsize=8, compression="lzw", res=resolution)
  print(p2)
  dev.off()
}
