data("iris")
head(iris)
dim(iris)
str(iris)
class(iris)
sapply(iris, class)
species <- iris %>% count(Species) %>% mutate(Percentage=paste0(round(n/sum(n)*100,2)))
head(species)
view(species)
pie <-ggplot(species, aes(Species,n, fill=Species)) +
  geom_bar(stat = "identity", width = 1) + coord_polar(start=0)+theme_tufte()+
  geom_text(aes(label=n), color="black",size=5,vjust=1) +scale_fill_brewer()+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
pie
