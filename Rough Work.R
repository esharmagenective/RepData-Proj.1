### Rough Work for project

activity<-read.csv("activity.csv")

str(activity)
steps<-tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
dates<-unique(activity$date)
stepssum<-data.frame(Steps=steps, Date=dates)

library(ggplot2)

a<-ggplot(stepssum,aes(x= Steps))
a+
    geom_histogram(color="blue", fill="lightblue")+
    labs(y="Number of Days", title = "Total Number of Steps")+
    geom_vline(aes(xintercept=mean(stepssum$Steps)), size=1, linetype=3, color="#878787")
    theme_bw()
    
