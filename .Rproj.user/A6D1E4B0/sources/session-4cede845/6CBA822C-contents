install.packages('tidyverse')
install.packages('tidyr')
install.packages("readr")
library(tidyverse)
library(tidyr)
library(readr)
#after this I had imported all my data sets that I needed. 
cbind1 <- read_csv("cbind1.csv")
cbind2 <- read_csv("cbind2.csv")
rbind1 <- read_csv("rbind1.csv")
rbind2 <- read_csv("rbind2.csv")
merge2 <- read_csv("merge2.csv")
merge1 <- read_csv("merge1.csv")
merge3 <- read_csv("merge3.csv")
columns_bound <- cbind(cbind1, cbind2)
rows_bound <- rbind(rbind1, rbind2)
total <- merge(merge2, cbind1,by = "Vessel")
left_join(merge1, merge3)
left_join(merge3, merge1)
merge1 <- merge1 %>% rename(newName = Vessel)
left_join(merge1, merge3, join_by("newName" == "Vessel"))
newdf <- left_join(merge1, merge3, join_by("newName" == "Vessel"))
#im going to try some other merges out for the files below here.
#first one i did was the right merge.
right_join(merge1, merge2)
right_join(merge2, merge1)
merge1 <- merge1 %>% rename(newName = Vessel2)
# the second one im going to do is the full outer join.
full_join(merge2, merge3)
#this will create a tibble.
# the third merge i am going to do is the inner join merge.
inner_join(merge3, merge1)
#it gave me an error code but it did give me a suggestion to try cross join.
cross_join(merge3, merge1)
#this one gave me a tibble too. 
#im going to retry the inner join merge but flop the x and y.
inner_join(merge1, merge3)
#still did not work trying different data.
inner_join(merge1, merge2)
# did not work. I will try to figure out this later.
# I am trying to merge and make a new df named df1
df1 <- inner_join(merge1, merge2)
#that did not work out im assuming that you have to merge data if they have 
#the same varables I am going to retry but with a cross join and then try two simlar datasets.
df <- cross_join(merge1, merge2)
tibble(df) #this is just to show results in a table.
wholethang <- cross_join(df, merge3)
#just trying something out possibly for the messy graph for Dr. Greer.
Monsterdataset <- cross_join(wholethang, total)
#i will be graphing the monster now. 
install.packages("ggplot2")
ggplot(Monsterdataset, mapping =aes(Ware.x, `Body Thickness 1cm Below Rim.x`)) +
  geom_boxplot(box.linewidth = 100000,box.color = "black" )
#just one large box on a box plot 
#now i will be making a graph for the actual good graph. 
#first I am going to merge cbind1 and 2
graphingdf <- cross_join(cbind1, cbind2)
# this did not work out
#I noticed that columns_bound looks like it has what I need. 
ggplot(data = columns_bound, aes(x = `Body Thickness 1cm Below Rim`,  y = Ware))
+ geom_boxplot()
#this is just a start to a graph I would want.
collums_bound
ggplot(columns_bound, aes(x = Ware, y = `Body Thickness 1cm Below Rim`,
fill = Ware, color = Ware)) + geom_jitter()+
geom_boxplot(alpha = 0.8) +
scale_fill_manual(values = c('pink','lightgreen')) +
scale_color_manual(values = c('magenta','green'))+
theme_classic()+
labs(title = "Ware Body Thickness 1cm", subtitle = "collums_bound", x = "Ware", y = "Body Thickness 1cm Below Rim")+
theme(plot.title = element_text(face = "bold", hjust = 0.5), 
plot.subtitle = element_text(hjust = 0.5), 
axis.title = element_text(face = "bold"), legend.position = "none")
#this is the plot that I wanted!
#now I will be looking at the diffrences inbetween rim diameter and ware types.
#im going to use a graph to visualize this possible diffrence 
ggplot(columns_bound, aes(x = Ware, y = RimDiameter,
                          fill = Ware, color = Ware)) + geom_jitter()+
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c('lightgreen','yellow')) +
  scale_color_manual(values = c('blue','darkorange'))+
  theme_classic()+
  labs(title = "Q1", subtitle = "collums_bound", x = "Ware", y = "RimDiameter")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        axis.title = element_text(face = "bold"), legend.position = "none")
#this one produced fun colors and it gave me an idea to give a better idea how
# to answer question 1. 
#next I am going to make a table just to look at it better.
install.packages("knitr")
#just installed this to help organize and possibly try something out with types
# of diffrent wares
Q1 <- tibble(columns_bound$Ware,columns_bound$RimDiameter)
# wich gave me a great sight to see divison inbetween the two variables.
#now I will be doing a test for Q1. 
library(tidyverse)
Q1
mass_shaptest <- shapiro.test(Q1$`columns_bound$RimDiameter`)
mass_shaptest
hist(Q1$`columns_bound$RimDiameter`) 
#I need to figure out how to separate the data.
stone_data2 <- Q1 %>%
  + filter( Q1$`columns_bound$RimDiameter`> 12 & Q1$`columns_bound$Ware` == "StoneWare")
# I did one lets to the other!(note its 2 beacause I messed up)
Earth_data <- Q1 %>%
  + filter( Q1$`columns_bound$RimDiameter`> 12 & Q1$`columns_bound$Ware` == "Coarse Earthenware")
# let the test begin 
mass_shaptest <- shapiro.test(stone_data2$`columns_bound$RimDiameter`)
mass_shaptest
# now lets do earth
mass_shaptest <- shapiro.test(Earth_data$`columns_bound$RimDiameter`)
mass_shaptest
# second test
hist(stone_data2$`columns_bound$RimDiameter`)
#now lets do earth
hist(Earth_data$`columns_bound$RimDiameter`)
#Third Test
mass_ftest <- var.test(columns_bound$RimDiameter ~ Ware, data = stone_data2)
mass_ftest
# did not work
#now I will be working on question 2
Q2 <- tibble(columns_bound$Ware,columns_bound$VesType)
#just made its own data set.
 Crock_data<- Q1 %>%
  + filter(Q2$`columns_bound$VesType` == "Crock")
 #now the jar data
 Jar_data<- Q1 %>%
   + filter(Q2$`columns_bound$VesType` == "Crock or Jar")
 #now I will be doing a test!
 chisq.test(table(Q2$`columns_bound$Ware`, Q2$`columns_bound$VesType`))
 # I did a chisq.test because there is no numeric variables.
 chisq.test(table(Q2$`columns_bound$VesType`))
 #just a test on only Crocks or Jars
 #now I will test wares
 chisq.test(table(Q2$`columns_bound$Ware`))







 



