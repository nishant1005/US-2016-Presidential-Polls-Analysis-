#Votes insights
primary <- read.csv("primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("county_facts.csv", stringsAsFactors = FALSE)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)
install.packages("grid")
library(grid)
install.packages("gridExtra")
library(gridExtra)
install.packages("DT")
library(DT)
install.packages("GGally")
library(GGally)
install.packages("randomForest")
library(randomForest)


votes <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Republican") %>% 
  group_by(state_abbreviation, county) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes),
            votes = max(votes))

demographics %<>%
  filter(state_abbreviation %in% c("IA", "NV", "SC")) %>% 
  select(state_abbreviation = state_abbreviation, county = area_name, 
         income = INC110213, hispanic = RHI725214,
         white= RHI825214, college = EDU685213, density = POP060210) %>% 
  mutate(county = gsub(" County", "", county))


votes <- inner_join(votes, demographics, by = c("state_abbreviation","county"))

datatable(votes, class = 'compact')

votes %>% 
  group_by(winner) %>% 
  summarize(round(mean(income)), round(mean(white)), 
            round(mean(college),1),round(mean(density)),round(mean(hispanic),1))%>%      
  datatable( colnames = c(" ",  "Winner", "Income", "White (non-Hispanic)", "Colege","Density (pop/sq m)", "Hispanic"), class = 'compact', caption = "Average County Demographics by Winner")

votes$winner <- as.factor(votes$winner)

#Random Forest

model <- randomForest(winner ~ income + hispanic + white + college + density, data = votes)

votes


#Naive Bayes
library(e1071)
classifier <- naiveBayes(winner ~ income + hispanic + white + college + density, data = votes)
classifier
summary(classifier)
nb_test_predict <- predict(classifier,votes[,4:8])
nb_test_predict

#Plot naive bayes
table(votes$winner,nb_test_predict)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("gmodels")
library(gmodels)

install.packages("caret")
library(caret)

nbcm<-confusionMatrix(nb_test_predict, votes$winner)
str(nbcm)
nbcm
