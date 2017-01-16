rm(list = ls())

library(ggplot2)
library(ggthemes)
library(Hmisc)
library(scales)
library(dplyr)

filenames <- list.files("Data", pattern="*.csv", full.names=TRUE)
df <- lapply(filenames, read.csv)
names(df) <- substr(filenames, 6, 50) #remove data folder from file name
names(df) <- unlist(strsplit(names(df), ".",fixed=T))[(1:length(filenames))*2 - 1] #remove .csv from file name

for (i in 1:length(names(df))){
  ref = df[[i]]
  if (is.element(NA,as.Date(ref[,"status_published"]))){
    ref = ref[as.Date(ref[,"status_published"], "%m/%d/%Y") > as.Date("2000/1/1"),]
  } else{
    ref = ref[as.Date(ref[,"status_published"]) > as.Date("2000/1/1"),]
  }
  #ref = ref[as.Date(ref[,"status_published"]) > as.Date("2000/1/1"),]
  date = as.Date(ref[,"status_published"])
  reactions = as.numeric(ref[,"num_reactions"])
  mydf = data.frame(date, reactions)
  
  mydf$Year = format(mydf$date, "%Y")
  mydf$Month <- format(mydf$date, "%b")
  mydf$Day <- format(mydf$date, "%d")
  mydf$YearMonth <- format(mydf$date, "%y-%b")
  # <- aggregate(data= mydf, reactions~YearMonth, FUN=sum)
  
  myPlot<- ggplot(data=mydf, aes(x= YearMonth, y = reactions, color=reactions, group=1)) +
    geom_point() +
    scale_color_gradient(low="blue", high="red")+
    stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", colour = "red", width = 0.3)+
    stat_summary(fun.y = "mean", colour = "yellow", size = 1, geom="line")+
    stat_smooth() +
    ggtitle(paste("Monthly Reactions: ", names(df)[i], sep="")) +
    #facet_grid(facets = Year ~ .) +
    theme_fivethirtyeight()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7),
          legend.position="none")+
    scale_y_continuous(labels = comma)
  
  ggsave(myPlot, file = paste("Result/plot", names(df)[i], ".png", sep=""), width= 15, heigh= 10)
}
