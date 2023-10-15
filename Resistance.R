library(tidyverse)
library(reshape2)
library(reshape)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(dplyr)

#Patients data
lymph <- read.csv("D:\\DS files\\Datasets\\lympatient.csv")
View(lymph)

#Plot Gender distribution
gen <- lymph %>% count(Gender) %>% mutate(Percents=paste0(round(n/sum(n),2),"%"))
ggplot(gen, aes(x=Gender, y=n))+geom_bar(stat = "identity", width = 0.80, fill="blue2" )+
  geom_text(aes(label=Percents), size=5, hjust=0.5)+theme_clean() + 
  xlab("Gender")+ylab("Percentage Count")+
  ggtitle("Plot of Gender Distribution")+
  theme(axis.text.y = element_text(size = 10,face="plain"))+coord_flip()


#Isolates data
myiso <- read.csv("D:\\DS files\\Datasets\\isolates.csv")
View(myiso)


#Count of isolate orgnisms
iso <- myiso %>% count(Organisms) %>% mutate(Percent=n/nrow(myiso)*100) %>% 
  mutate(Pert=paste0(round(n/sum(n)*100,2),"%"))
view(iso)

#plot of the percentage count of isolated organisms
isograph <- ggplot(iso, aes(x=Organisms,y=Percent))+
  geom_bar(stat = "identity",fill="darkblue" ,width = 0.75)+theme_bw()+
  coord_flip() +geom_text(aes(label=Pert), size=3, hjust=-0.5)
isograph + scale_y_continuous(labels=scales::percent_format(accuracy = 1))
isograph + xlab("Micro organisms")+ylab("Percentages of micro organisms")+
  ggtitle("Percentage distribution of isolated micro-organisms")+
  theme(axis.text.y = element_text(size = 10,face="italic"))
#ggsave(isograph, plot = last_plot(),path="C:\\Users\\nlife\\Downloads\\Documents",scale = 1)


#Antibiotics Resistance of E-coli bacteria
col<- read.csv("D:\\DS files\\Datasets\\e.coli.csv")
view(col)

coli <- col[,-1]
col_long <- coli
col_long$E.coli <- paste0(0,1:nrow(col_long))
col_long <- pivot_longer(col_long,colnames(col[,-1]),names_to = "Antibiotics")
view(col_long)

ggcol <- ggplot(col_long, aes(Antibiotics,E.coli,fill=value))+geom_tile()+coord_fixed()+
  scale_x_discrete(limit=c("AMP","C","CRO","SXT","CXM","FEP","SAM","CN","CAZ","TE","CIP","MEM"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),) +scale_fill_brewer()




#Antibiotic Resistance of Rhizobium bacteria
rhi <- read.csv("D:\\DS files\\Datasets\\Rhiz.csv")
view(rhi)

Rhiz <- rhi[,-1]
Rhiz_long <- Rhiz
Rhiz_long$Rhizobium <- paste0(0,1:nrow(Rhiz_long))
Rhiz_long <- pivot_longer(Rhiz_long,colnames(rhi[,-1]),names_to = "Antibiotics")
view(Rhiz_long)

ggRhiz <- ggplot(Rhiz_long, aes(Antibiotics,Rhizobium,fill=value))+geom_tile()+
  scale_x_discrete(limit=c("AMP","C","CRO","SXT","CXM","FEP","SAM","CN","CAZ","TE","CIP","MEM"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),legend.position="bottom") +scale_fill_hc()



#Antibiotic Resistance of Klebsiella bacteria
kl<-read.csv("D:\\DS files\\Datasets\\Klebs.csv")
view(kl)

kleb <- kl[,-1]
kleb_long <- kleb
kleb_long$Klebsiella <- paste0(0,1:nrow(kleb_long))
kleb_long <- pivot_longer(kleb_long,colnames(kl[,-1]),names_to = "Antibiotics")
view(kleb_long)

ggkleb <- ggplot(kleb_long, aes(Antibiotics,Klebsiella,fill=value))+geom_tile()+
  scale_x_discrete(limit=c("AMP","C","CRO","SXT","CXM","FEP","SAM","CN","CAZ","TE","CIP","MEM"))+
  ylab("Klebsiella")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="bottom")




library(ggpubr)
theme_set(theme_bw()+theme(legend.position = "right"))
rot <- ggarrange(ggRhiz,ggkleb,ncol = 1,nrow = 2)
rot

library(gridGraphics)
grid.newpage()
print(rot,
      vp = viewport(width=0.5,
                    height=1,
                    angle = 270))