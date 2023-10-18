library(tidyverse)

data <- ChickWeight
head(data)
dim(data)
summary(data) ##compute the descriptives of the chickweight data


#Aggregate the mean weight depending on the diet fed to chicks
dw <- aggregate(weight~Diet, data, mean)

##mean weight per diet
ggplot(dw, aes(x=Diet, y=weight)) + geom_col(width = 0.75, fill="brown2")+theme_minimal()+
  ggtitle("Mean chickweight Per Diet")

#findind max value for each diet
wd <- aggregate(weight~Diet, data, max)
ggplot(wd, aes(Diet, weight)) + geom_col(fill="green4", width =0.85 )+theme_light()+
  geom_text(label=wd$weight,size=4,vjust=-0.25)+
  ggtitle("Maximum weight of chick per diet")

##here we compute the weight of all chicks over time
##We see that as time increases the weight of chicks increases
#wot <- aggregate(data$weight, by=list(t=data$Time), FUN = 'mean')
wt <- aggregate(weight~Time, data, mean)

##we plot the trend of weight over time
#ggplot(wot, aes(x=t, y=x)) + geom_line()
ggplot(wt, aes(x=Time, y=weight,colour="yellow" )) + geom_line(stat = "identity",inherit.aes = TRUE)+
  ggtitle("Mean Growth trend of chicks over time")+theme_minimal()

##aggregate the mean weight based on diet and time of feeding
aggregate(data$weight,by=list(time=data$Time,diet=data$Diet), FUN = mean)
dt <- aggregate(weight~Diet+Time, data, mean)
ggplot(dt, aes(x=Time, y=weight,colour=Diet)) + geom_line()+
  theme_bw()+
  geom_point(alpha=0.3)+
  geom_smooth(alpha=0.2, linewidth=1)+
  ggtitle("Weight over time per diet")
