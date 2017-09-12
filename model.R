
### Kaggle Titanic Exercise ###

setwd("C:/Users/Andrew/Desktop/Kaggle/Titanic")
train <- read.csv("train.csv", stringsAsFactors = FALSE)



library(stringr)
library(dplyr)


# create two new variables that keep track of which ages are estimated and 
# which ages are missing in the initial train dataset
work <- train
work <- work %>% mutate(age.estimated = ifelse(str_detect(train$Age, "\\d*\\.5") == TRUE, "YES", "NO"))
work <- work %>% mutate(age.na = ifelse(is.na(work$Age) == TRUE, "YES", "NO"))

# Change Pclass to a factor variable
work <- work %>% mutate(Pclass = as.factor(Pclass))

# Change Survived to a factor variable
work <- work %>% mutate(Survived = as.factor(Survived))




sum(!complete.cases(work))
#[1] 177

incomplete <- work[!complete.cases(work),]
summary(incomplete)


# There are 177 missing ages!
sum(is.na(incomplete$Age))
#[1] 177



# Create a function that can loop through the 'work' dataset and add up
# all the missing values in each column
na.sum <- function(column){
        sum(is.na(column))
}

# Age is the only column in the original 'train' dataset that has missing
# values
apply(work, 2, na.sum)



# Double check that you didn't make a mistake:
temp <- train %>% select(-Age)
sum(!complete.cases(temp))
#[1] 0

# Perfect.



### Some exploratory plots ###
#=============================

library(ggplot2)

# Plot the total number of people who survived/didn't survive in each Pclass
qplot(data = work, x = Survived, geom = "bar", fill = factor(Survived)) + facet_grid(. ~ Pclass)



plot <- ggplot(work, aes(Survived))

# Males were a lot less likely to survived than females
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ Sex)


# NOTE: there are more males than females in the dataset (314 females, 577 males)
work %>% group_by(Sex) %>% summarize(n = n())



# There doesn't seem to be much of a pattern in terms of where people
# embarked from and their survival rate
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ Embarked)


# There also doesn't seem to be much to see from SibSp or Parch by themselves
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ factor(SibSp))
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ factor(Parch))


# Also nothing apparent with age.estimated and age.na
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ factor(age.estimated))
plot + geom_bar(aes(fill = Survived), position = "dodge") + facet_grid(. ~ factor(age.na))












