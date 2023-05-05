### Rough Work for project ###

#####################
#   mean total steps
#####################
activity<-read.csv("activity.csv")

str(activity)
steps<-tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
dates<-unique(activity$date)
stepssum<-data.frame(Steps=steps, Date=dates)

library(ggplot2)

a<-ggplot(stepssum,aes(x= Steps))
a+
    geom_histogram(color="darkblue", fill="lightblue")+
    labs(y="Number of Days", title = "Total Number of Steps")+
    geom_vline(aes(xintercept=mean(stepssum$Steps)), size=1, linetype=3, color="#878787")
    theme_bw()

x<-mean(stepssum$Steps)   

print(paste("The answer is",x))

##########################
#   average daily pattern
##########################

daily<-tapply(activity$steps, activity$interval, FUN = mean, na.rm=TRUE)
intervals<-unique(activity$interval)
avg<-data.frame(Steps=daily, Interval=intervals)

b<-ggplot(avg,aes(x=Interval,y=Steps))
b+geom_line(color="#553ea8", size=1)+
    labs(title = "Average Number of Steps by Interval")+
    theme_bw()

y<-max(avg$Steps)
print(avg[which.max(avg$Steps) , ])

##################
#   Imputing NA's
##################






