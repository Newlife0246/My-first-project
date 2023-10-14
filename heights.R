library(tidyverse)
library(dslabs)
data(heights)

view(heights)
str(heights)

heights %>% ggplot(aes(height, fill=sex))+
  geom_density(alpha=0.85)

sex <- heights %>% count(sex) %>% mutate(percentage=paste0(round(n/sum(n)*100,2)))

ggplot(data= sex, aes(sex, percentage, fill=sex)) + geom_bar(stat = "identity", width = 1)
