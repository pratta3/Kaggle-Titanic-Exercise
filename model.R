
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





### Title variable ###
#=====================

# Add variable that contains the title associated with each Name
full <- full %>% mutate(Title = str_extract(Name, "[A-Za-z]+\\."))

# Simplify the Titles
full <- full %>% mutate(Title = ifelse(Sex == "male" & !(Title %in% c("Mr.", "Master.")),
                                       "Mr.", Title),
                        Title = ifelse(Sex == "female" & !(Title %in% c("Miss.", "Mrs.")),
                                       "Mrs.", Title))






### Age variable ###
#===================



# Age stats of boys with the title "Master".
master.stats <- full %>% filter(str_detect(Name, "Master\\.")) %>% 
        summarize(n = n(),
                  median = median(Age, na.rm = T),
                  mean = mean(Age, na.rm = T),
                  sd = sd(Age, na.rm = T),
                  min = min(Age, na.rm = T),
                  max = max(Age, na.rm = T))

# There are only 5 boys age 15 or younger that aren't listed as "Master".
full %>% filter(Sex == "male" & Age <= 15 & !str_detect(Name, "Master\\."))

full %>% filter(Title == "Master." & is.na(Age))


# Impute the mean values of the different Titles/Pclass into missing Age values
age.means <- full %>% group_by(Title, Pclass) %>% summarize(mean.age = mean(Age, na.rm = T))

full <- full %>% left_join(age.means, by = c("Title", "Pclass")) %>% 
        mutate(Age = ifelse(is.na(Age), mean.age, Age)) %>% 
        select(-mean.age)

# Max Age for Masters is 14.5
full %>% filter(Title == "Master.") %>% pull(Age) %>% summary

# There are 51 girls Age 14.5 or younger
full %>% filter(Sex == "female" & Age <= 14.5) %>% nrow

# Change their Title to Girl.
full <- full %>% mutate(Title = ifelse(Sex == "female" & Age <= 14.5, "Girl", Title))

# Looks like it worked correctly
full %>% filter(Title == "Girl") %>% nrow
full %>% filter(Sex == "female" & Age <= 14.5) %>% pull(Title) %>% table

# Although it doesn't look like it will play that big of a role
women <- full %>% filter(Sex == "female")
ggplot(women, aes(Survived)) + geom_bar(aes(fill = Survived)) + facet_grid(. ~ Title)


# Just to reiterate that wherever there is a missing value in age.estimated,
# that's because the age was missing initially. These missing values were
# imputed later
full %>% filter(is.na(age.estimated)) %>% pull(age.na) %>% unique
#[1] "YES"
full %>% filter(age.na == "YES") %>% pull(age.esstimated) %>% unique
#[1] NA

# Change missing values in age.estimaged to "imputed"
full <- full %>% mutate(age.estimated = ifelse(is.na(age.estimated), "imputed", age.estimated))

sum(is.na(full$age.estimated))
#[1] 0






### Fare variable ###
#================================


# It looks like passengers that traveled together have the same ticket number.
full %>% arrange(Name) %>% head(20)

# There are 929 unique Tickets
full %>% pull(Ticket) %>% unique %>% length
#[1] 929

# Make a new variable that contains the group size associated with each Ticket
full <- full %>% group_by(Ticket) %>% 
        mutate(group.size = n())

summary(full$group.size)
full %>% arrange(Name) %>% head(20)

# Curious to see if the same could be calculated by summing SibSp and Parch
all((full %>% mutate(size = SibSp + Parch + 1) %>% pull(size)) == full$group.size)

# Interestingly, it's not. Let's check it out further
full <- full %>% mutate(family.size = SibSp + Parch + 1)

full %>% arrange(Ticket) %>% head(20)
# So what happened is that some people travelled with friends but not family.

# Is there anything interesting about the difference between group.size and
# family.size?

groups <- full %>% group_by(group.size) %>% 
        summarize(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T))
families <- full %>% group_by(family.size) %>% 
        summarise(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T))

data <- data.frame(size = groups$group.size, group = groups$survival.rate, family = families$survival.rate)

# Eh, there are only small differences I think.
ggplot(data) + geom_bar(aes(size, group), stat = "identity", alpha = .5) +
        geom_bar(aes(size, family), stat = "identity", fill = 2, alpha = .5)













