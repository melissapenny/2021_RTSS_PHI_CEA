
#function to plot events averted with and without boost
EventsAvertedPlot_bothVac <- function(Data, my_Event, my_FigureNumber, my_time, xlab='', ylab='', main='', save_plots=T ) {
	
	# set group colours
	groupColours = my_col#groupColours = c('GSK'='#F8766D','EMOD DTK'='#7CAE00','Imperial'='#00BFC4','OpenMalaria'='#C77CFF')
	
	# split data into with- and without-boost
	subData = subset(Data,age_granulation=='fine' & Event==my_Event & Transmission%in%paste('prev',c(3,10,30,50,prevHighNum),sep=''))
	

	subData$Transmission = factor(subData$Transmission)
	subData$Transmission = factor(subData$Transmission,levels=paste('prev',c(3,10,30,50,prevHighNum),sep=''))
	levels(subData$Transmission) = paste('prev',c(3,10,30,50,prevHighNum),sep='')
	
	##-----
	  subData$Transmission <- gsub("prev", "italic('Pf')*PR[2-10] == ", subData$Transmission)
	  subData$Transmission <- factor(subData$Transmission, levels = paste("italic('Pf')*PR[2-10] == ",c(3,10,30,50,prevHighNum),sep=""))
	  subData$Group <- gsub("EMOD DTK", "EMOD~DTK", subData$Group)
	  subData$Group <- factor(subData$Group, levels = c("EMOD~DTK", "GSK",  "Imperial", "OpenMalaria"))
	  ##-----
	  
	data_withoutBoost = subset(subData,Vaccine=='without_booster')
	data_withBoost = subset(subData,Vaccine=='with_booster')
	
	
	# create age group size information for barplot
	  data_withoutBoost$barWidths = data_withoutBoost$Age_upper - data_withoutBoost$Age_lower
	  data_withoutBoost$barMids = data_withoutBoost$Age_lower + data_withoutBoost$barWidths/2
	  
	  data_withBoost$barWidths = data_withBoost$Age_upper - data_withBoost$Age_lower
	  data_withBoost$barMids = data_withBoost$Age_lower + data_withBoost$barWidths/2
		

	data_withoutBoost$Group <- factor(data_withoutBoost$Group, levels = c("EMOD~DTK", "GSK", "Imperial", "OpenMalaria"))
	data_withBoost$Group <- factor(data_withBoost$Group, levels = c("EMOD~DTK", "GSK", "Imperial", "OpenMalaria"))

	# spread data based on summary
	data_withoutBoost_spread = spread_(data_withoutBoost, "Summary", my_time)
	data_withBoost_spread = spread_(data_withBoost, "Summary", my_time)
	
	# produce plot
	plot1 = ggplot(NULL) + theme_bw()
	
	plot1 = plot1 + geom_bar(stat='identity',aes(x= barMids,y=median,width= barWidths,fill=factor(Group)),colour='white',size=0.2,position="dodge",data= data_withoutBoost_spread)
	plot1 = plot1 + geom_point(aes(x=barMids,y=median),shape=20,size=1,data=data_withBoost_spread)
	
	# this gave incorrect plots
	# plot1 = plot1 + geom_bar(stat='identity',aes(x=(Age_lower+Age_upper)/2,y=median/(Age_upper-Age_lower),width=Age_upper-Age_lower,fill=factor(Group)),colour='white',size=0.2,position="dodge",data= data_withoutBoost_spread)
	# plot1 = plot1 + geom_point(aes(x=(Age_lower+Age_upper)/2,y=median/(Age_upper-Age_lower)),shape=20,size=1,data=data_withBoost_spread)
	
	plot1 = plot1 + facet_grid(Transmission~Group,labeller=label_parsed, scales="free_y")
	plot1 = plot1 + xlab(xlab) + ylab(ylab)
	plot1 = plot1 + ggtitle(main)
	plot1 = plot1 + xlim(c(0,20))
	plot1 = plot1 + scale_fill_manual(name='group',values=groupColours)
	plot1 = plot1 + guides(fill=FALSE)

	if(NumGroupsToPlot == 4){
	  plot_width = 18
	  plot_height = 15
	}
	if(NumGroupsToPlot == 2){
	  plot_width = 11
	  plot_height = 15
	}
	if(NumGroupsToPlot == 1){
	  plot_width = 6
	  plot_height = 15
	}
	# draw or save to file
	if(save_plots){
		tiff(paste(pathPlots,"Figure ",my_FigureNumber,".tif",sep=""), compression="lzw", res= resolution,  width=plot_width, height=plot_height, units="cm", pointsize=8)
		print(plot1)
		dev.off()
	} else {
		print(plot1)
	}
	
}
