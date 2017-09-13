
### Kaggle Titanic Exercise ###

setwd("C:/Users/Andrew/Desktop/Kaggle/Titanic")



# Load the required packages that you'll be using
library(stringr)
library(dplyr)
library(ggplot2)




# Read in training and test data
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)



# Add Survived variable to test dataset with NA values
test$Survived <- NA



# Combine train and test datasets
full <- rbind(train, test)



# I'll be working with the full dataset


# Make some changes/additions:
# - age.estimated: YES if the age is estimated or NO if it is known
# - age.na: YES if the age is missing or NO if it isn't
# - change Pclass and Survived to factors

full <- full %>% mutate(age.estimated = ifelse(str_detect(Age, "\\d*\\.5") == TRUE, "YES", "NO"),
                        age.na = ifelse(is.na(Age) == TRUE, "YES", "NO"),
                        Pclass = as.factor(Pclass),
                        Survived = as.factor(Survived),
                        train.test = ifelse(is.na(Survived) == TRUE, "test", "train"))

# Check that everything worked properly
nrow(full[which(str_detect(full$Age, "\\d*\\.5")),]) == nrow(full[which(full$age.estimated == "YES"),])
nrow(full[is.na(full$Age),]) == nrow(full[which(full$age.na == "YES"),])
nrow(train) == nrow(full[which(full$train.test == "train"),])
nrow(test) == nrow(full[which(full$train.test == "test"),])

#[1] TRUE for everything



# Excluding the Survived variable, there are 595 incomplete cases in the full dataset
full %>% select(-Survived) %>% summarize(incomplete = sum(!complete.cases(full)))


# Check where the missing values are:
# Create a function that can loop through a dataset and add up
# all the missing values in each column
na.sum <- function(column){
        sum(is.na(column))
}


# There are 263 missing Ages (and 263 corresponding missing values in age.estimated),
# 418 missing Survived values from the test dataset, and 1 missing value in Fare
apply(full, 2, na.sum)

# Clearly some of the missing values are in the train portion and some are in the
# test portion because there or only 595 total incomplete cases.




# The missing Fare value is in the test dataset
which(is.na(full$Fare) == TRUE)
#[1] 1044


# Similar proportion of Age values are missing in both the test and train
# datasets. 
c(na.sum(train$Age), na.sum(test$Age))
c(na.sum(train$Age)/nrow(train), na.sum(test$Age)/nrow(test))


# I'll keep this in mind for now but take a closer look later.







### Some exploratory plots ###
#=============================


# Plot the total number of people who survived/didn't survive in each Pclass
qplot(data = full, x = Survived, geom = "bar", fill = factor(Survived)) + facet_grid(. ~ Pclass)



plot <- ggplot(full, aes(Survived))

plot + geom_bar(aes(fill = Survived)) + facet_grid(. ~ Pclass)

plot + geom_bar(aes(fill = Survived), position = "dodge") +
        facet_grid(Sex ~ Pclass)

# The proportion of females that survived decreases dramatically in Pclass == 3.
# The proportion of males that survived increases dramatically in Pclass == 1.
full[1:891,] %>% group_by(Sex, Pclass) %>% summarize(prop = mean(Survived == 1))





### Age variable ###
#===================



# There doesn't seem to be much going on except that young children have higher survival rates.
cutpoints <- function(vector, ncuts){
        quantile(vector, probs = seq(0, 1, 1/n))
}
n = 4
age.explore <- full %>% 
        filter(!is.na(Age)) %>% 
        mutate(Age = cut(Age, breaks = cutpoints(Age, n), labels = cutpoints(Age, n)[1:n]),
               Survived = as.numeric(as.character(Survived))) %>% 
        group_by(Sex, Pclass, Age) %>% 
        summarize(survival.rate = mean(Survived, na.rm = TRUE),
                  n = n())

ggplot(age.explore, aes(Age, survival.rate)) + 
        geom_bar(stat = "identity") +
        facet_grid(Pclass ~ Sex)



# Function calculates the survival rate of males and females separately of
# Age less than or equal to each integer in the range of 1 to the max Age
age.explore <- function(){
        seq <- 1:max(full$Age, na.rm = TRUE)
        prop <- matrix(rep(NA, length(seq)*2), ncol = 2)
        data <- full %>% mutate(Survived = as.numeric(as.character(Survived))) %>% 
                group_by(Sex)
        for(i in seq){
                prop[i,] <- data %>% filter(Age <= i) %>% 
                        summarize(survival.rate = mean(Survived, na.rm = TRUE)) %>% 
                        pull(survival.rate)
        }
        data.frame(sex = rep(c("female", "male"), each = length(seq)),
                   survival.rate = c(prop[,1], prop[,2]),
                   max.age = rep(seq, 2))
}

age.survival <- age.explore()

ggplot(age.survival, aes(max.age, survival.rate)) + geom_point(aes(color = sex)) +
        geom_smooth(aes(color = sex), se = FALSE)





# Function calculates survival rates of males and females in each
# Pclass with Age less than or equal to each integer from 2 to
# the max Age.
age.explore <- function(){
        seq <- 2:max(full$Age, na.rm = TRUE)
        prop <- matrix(rep(NA, length(seq)*6), ncol = 6)
        data <- full %>% mutate(Survived = as.numeric(as.character(Survived))) %>% 
                group_by(Sex, Pclass)
        sex <- data %>% summarize(survival.rate = mean(Survived, na.rm = TRUE)) %>% 
                pull(Sex)
        class <- data %>% summarize(Survival.rate = mean(Survived, na.rm = TRUE)) %>% 
                pull(Pclass)
        for(i in seq){
                prop[i-1,] <- data %>% filter(Age <= i) %>% 
                        summarize(survival.rate = mean(Survived, na.rm = TRUE)) %>% 
                        pull(survival.rate)
        }
        data.frame(sex = rep(sex, each = length(seq)),
                   pclass = rep(class, each = length(seq)),
                   survival.rate = c(prop[,1], prop[,2], prop[,3], prop[,4], prop[,5], prop[,6]),
                   max.age = rep(seq, 6))
}

age.survival <- age.explore()

ggplot(age.survival, aes(max.age, survival.rate)) +
        geom_line(aes(color = sex)) +
        facet_grid(. ~ pclass)




































