
# produce plot 
DataEff = DataEffAll
DataEff = as.data.frame(DataEff)
DataEff$efficacy = as.numeric(DataEff$efficacy)
DataEff$time = as.numeric(DataEff$time)
DataEff$Vaccine=gsub("_"," ", DataEff$Vaccine)
DataEff$Vaccine <- factor(DataEff$Vaccine, levels = c("without booster", "with booster"))

# p1 = ggplot(DataEff[DataEff$Vaccine=="without_booster",], aes(x=time, y= efficacy,  colour=Group), linetype='solid') + geom_line(stat="identity", size=1)  +
  # geom_line(data=DataEff[DataEff$Vaccine=="with_booster",], aes(x=time, y= efficacy,  colour=Group), linetype='dashed', size=1) +
  # facet_wrap(~) +
  # scale_x_continuous("Time since third dose of RTS,S (years)") + scale_y_continuous("Efficacy against infection")+
  # theme_bw()
  
  p1 = ggplot(DataEff, aes(x=time, y= efficacy,  colour=Group), linetype='solid') + geom_line(stat="identity", size=0.5)  +
  geom_line(data=DataEff, aes(x=time, y= efficacy,  colour=Group), linetype='dashed', size=0.5) +
  facet_wrap(~Vaccine) +
  scale_x_continuous("Time since third dose of RTS,S (years)") + scale_y_continuous("Efficacy against infection")+
  theme_bw()+ 
        scale_color_manual(name="Groups",values=my_col, guide=FALSE) + 
	      scale_fill_manual(name="Groups",values=my_col, guide=FALSE)

  p1

# save file
if(save_plots){
  tiff(paste(pathPlots,"Figure 00 Best Fit Efficacy against infection",".tif",sep=""), width=18, height=11, units="cm", pointsize=8, compression="lzw", res=resolution)
  print(p1)
  dev.off()
}

# produce plot with antibody eff from imperial
DataEff = DataEffAll_antibody
DataEff = as.data.frame(DataEff)
DataEff$efficacy = as.numeric(DataEff$efficacy)
DataEff$time = as.numeric(DataEff$time)
DataEff$Vaccine=gsub("_"," ", DataEff$Vaccine)
DataEff$Vaccine <- factor(DataEff$Vaccine, levels = c("without booster", "with booster"))
p1 = ggplot(DataEff[DataEff$Vaccine=="without_booster",], aes(x=time, y= efficacy,  colour=Group), linetype='solid') + geom_line(stat="identity", size=1)  +
  geom_line(data=DataEff[DataEff$Vaccine=="with_booster",], aes(x=time, y= efficacy,  colour=Group), linetype='dashed', size=1) +
  scale_x_continuous("Time since third dose of RTS,S (years)") + scale_y_continuous("Efficacy against infection")+
  theme_bw()+ 
        scale_color_manual(name="Groups",values=my_col, guide=FALSE) + 
	      scale_fill_manual(name="Groups",values=my_col, guide=FALSE)
  # p1
  
   p1 = ggplot(DataEff, aes(x=time, y= efficacy,  colour=Group), linetype='solid') + geom_line(stat="identity", size=0.5)  +
  geom_line(data=DataEff, aes(x=time, y= efficacy,  colour=Group), linetype='dashed', size=0.5) +
  facet_wrap(~Vaccine) +
  scale_x_continuous("Time since third dose of RTS,S (years)") + scale_y_continuous("Efficacy against infection")+
  theme_bw()+ 
        scale_color_manual(name="Groups",values=my_col, guide=FALSE) + 
	      scale_fill_manual(name="Groups",values=my_col, guide=FALSE)

  p1

# save file
if(save_plots){
  tiff(paste(pathPlots,"Figure 00 Best Fit Efficacy against infection (antibody)",".tif",sep=""), width=15, height=8, units="cm", pointsize=8, compression="lzw", res=resolution)
  print(p1)
  dev.off()
}
