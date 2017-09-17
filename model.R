
### Kaggle Titanic Exercise ###



# Overview of the model:

# Ok. So after spending a lot of time exploring the dataset, here's what
# I think:
# - 

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






### Group variables ###
#================================


# It looks like passengers that traveled together have the same ticket number.
full %>% arrange(Name) %>% head(20)

# There are 929 unique Tickets
full %>% pull(Ticket) %>% unique %>% length
#[1] 929

# Make a new variable that contains the group size associated with each Ticket
full <- full %>% group_by(Ticket) %>% 
        mutate(group.size = n()) %>% 
        ungroup

summary(full$group.size)
full %>% arrange(Name) %>% head(20)

# Curious to see if the same could be calculated by summing SibSp and Parch
all((full %>% mutate(size = SibSp + Parch + 1) %>% pull(size)) == full$group.size)

# Interestingly, it's not. Let's make another variable of family.size.
full <- full %>% mutate(family.size = SibSp + Parch + 1)

full %>% arrange(Ticket) %>% head(20)
# So what happened is that some people travelled with friends but not family.





### Fare variable ###
#====================



# Notice that the Fare values appear to be the sum of individual ticket Fares when
# there are groups.

# Also interesting is that some Fares equal 0.
full %>% group_by(Pclass) %>% summarize(mean.total = mean(Fare, na.rm = T),
                                        min.total = min(Fare, na.rm = T),
                                        max.total = max(Fare, na.rm = T),
                                        mean.ind = mean(Fare/group.size, na.rm = T),
                                        min.ind = min(Fare/group.size, na.rm = T),
                                        max.ind = max(Fare/group.size, na.rm = T))

# Look at entries where Fare is 0
full %>% filter(Fare == 0) %>% View

# Weird. Not sure if these are mistakes or if these people somehow got on the
# Titanic for free. Also keep in mind that there are only 17 total where Fare is 0.
# They're also all males. The group.size and Cabin values are suspicious.
# Also they all embarked from Southampton.

# Does survival rate increase as Fare increases?
ggplot(full %>% filter(!is.na(Fare)) %>% 
        filter(Fare != 0) %>% 
        mutate(Fare = Fare/group.size,
               Fare = cut(Fare, quantile(Fare, seq(0, 1, .1)))) %>% 
        group_by(Fare, Sex) %>% 
        summarize(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T)),
       aes(Fare, survival.rate)) +
        geom_bar(stat = "identity", aes(fill = Sex)) +
        facet_grid(. ~ Sex)





### Cabin variable ###
#=====================


# There are a lot of empty values here. Of the ones that aren't
# empty, it looks like the general form is that cabins start with
# a letter followed by a number. Let's see exactly what we're dealing
# with.
full %>% filter(Cabin != "") %>% pull(Cabin) %>% str_extract("^.") %>% unique %>% sort

# A through G and T. A through G almost certainly correspond to the decks on the
# Titanic, which went from A to G. Not sure what the T is for.

# Make a new Deck variable
full <- full %>% mutate(Deck = ifelse(Cabin == "", "", str_extract(Cabin, "^.")))

# How many recorded values are there exactly in Cabin anyway?
full %>% filter(is.na(Cabin)) %>% nrow
# No missing values
full %>% filter(Cabin != "") %>% nrow
#[1] 295

# How are they distributed between the different decks?
ggplot(full %>% filter(Deck != "") %>% group_by(Deck) %>% tally, aes(Deck, n)) + geom_bar(stat = "identity")
ggplot(full %>% filter(Deck != "") %>% 
        group_by(Deck) %>% 
        summarize(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T),
                  n = n()),
       aes(Deck, survival.rate)) + geom_bar(stat = "identity")

# Unfortunately, there doesn't seem to be anything obvious here. That would have been cool.

# What decks do the different Pclasses appear on?
full %>% filter(Deck != "") %>% 
        group_by(Pclass, Deck) %>% tally

# Interestingly, most of the people with a listed Cabin came from first class.
# Also, only first class people were in Decks A, B, C, and T.

# The different survival rates of first class people seems to be related
# to the proportion of females on those decks.
full %>% filter(Deck != "") %>% 
        group_by(Pclass, Deck) %>% 
        summarize(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T),
                  female.prop = mean(Sex == "female"))


# Are there any differences between people with Cabins listed vs.
# people with no Cabin listed?
full %>% group_by(Pclass, Sex, Deck) %>% 
        summarize(survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T)) %>% 
        View

# Is something going on in Deck C?
full %>% filter(Deck == "C") %>% head(20)

# Eh, I don't think so.




### Ticket variable ###
#======================



full %>% pull(Ticket) %>% str_extract("^[A-Z]") %>% unique

# Hm, this produced a missing value as one of the unique values...

# But there are no missing values in the Ticket column
sum(is.na(full$Ticket))

# Got it. Tickets that don't begin with a letter produce
# and NA result.
full %>% pull(Ticket) %>% str_extract("^[A-Z]") %>% head

# Take a look at the Ticket values that start with letters
full %>% filter(str_detect(Ticket, "^[A-Z]")) %>% select(Ticket) %>% unique %>% arrange(Ticket) #%>% View

full %>% filter(str_detect(Ticket, "^[A-Z]")) %>% 
        group_by(Ticket) %>% 
        summarize(group.size = n(),
                  nsurvived = sum(as.numeric(as.character(Survived)), na.rm = T),
                  survival.rate = mean(as.numeric(as.character(Survived)), na.rm = T),
                  na.survived = sum(is.na(Survived))) %>% 
        filter(na.survived == 0) %>% 
        View

# Try narrowing down the number of Tickets by including
# more than just the first letter
full %>% mutate(ticket.letters = str_extract(Ticket, "^.+/([A-Za-z]+|\\d+)|^[A-Za-z]+")) %>% 
        filter(!is.na(ticket.letters)) %>%
        group_by(ticket.letters) %>% 
        summarize(n = n() - sum(is.na(Survived)),
                  nsurvived = sum(as.numeric(as.character(Survived)), na.rm = T),
                  nfemales = sum(Sex == "female" & !is.na(Survived)),
                  survival.rate = nsurvived/n) %>% 
        ungroup %>% 
        summarize(avg.n = mean(n, na.rm = T),
                  avg.survived = mean(nsurvived, na.rm = T),
                  prop.survived = avg.survived/avg.n)

# Interesting maybe?

# What do the numbers look like?
full %>% filter(str_detect(Ticket, "^\\d")) %>% 
        # group_by(Ticket) %>% 
        # summarize(n = n() - sum(is.na(Survived)),
        #           nsurvived = sum(as.numeric(as.character(Survived)), na.rm = T),
        #           nfemales = sum(Sex == "female" & !is.na(Survived)),
        #           survival.rate = nsurvived/n) %>% 
        mutate(ticket.length = str_length(Ticket)) %>%
        group_by(ticket.length) %>% 
        summarize(n = n() - sum(is.na(Survived)),
                  nsurvived = sum(as.numeric(as.character(Survived)), na.rm = T),
                  nfemales = sum(Sex == "female" & !is.na(Survived)),
                  survival.rate = nsurvived/n,
                  nclass1 = sum(Pclass == 1 & !is.na(Survived)),
                  nclass2 = sum(Pclass == 2 & !is.na(Survived)),
                  nclass3 = sum(Pclass == 3 & !is.na(Survived))) %>% 
        arrange(ticket.length) %>% View
        # ungroup %>% 
        # summarize(avg.n = mean(n, na.rm = T),
        #           avg.survived = mean(nsurvived, na.rm = T),
        #           prop.survived = avg.survived/avg.n)



full %>% filter(str_detect(Ticket, "^\\d")) %>% 
        mutate(ticket.length = str_length(Ticket),
               Survived = as.numeric(as.character(Survived))) %>% 
        group_by(ticket.length) %>% 
        summarize(n = n(),
                  nsurvived = sum(Survived, na.rm = T),
                  survival.rate = nsurvived/n,
                  nclass1 = sum(Pclass == 1),
                  nclass2 = sum(Pclass == 2),
                  nclass3 = sum(Pclass == 3))


tickets <- full %>% mutate(ticket.length = str_length(Ticket),
               Survived = as.numeric(as.character(Survived))) %>% 
        group_by(ticket.length) %>% 
        summarize(n = n(),
                  nsurvived = sum(Survived, na.rm = T),
                  survival.rate = nsurvived/n,
                  nclass1 = sum(Pclass == 1),
                  nclass2 = sum(Pclass == 2),
                  nclass3 = sum(Pclass == 3))
# View(tickets)

ggplot(tickets, aes(x = ticket.length)) + geom_bar(aes(y = n), stat = "identity", alpha = .5, fill = 2) + 
        geom_bar(aes(y = nsurvived), stat = "identity", alpha = .5, fill = 4)

full %>% filter(str_detect(Ticket, "^\\d")) %>% 
        mutate(ticket.number = str_extract(Ticket, "^\\d")) %>% 
        group_by(ticket.number) %>% 
        tally

full %>% filter(str_detect(Ticket, "^4")) %>% arrange(Ticket) %>% View

full %>% group_by(group.size) %>% tally



# This is potentially interesting. 1) What are the odds that the wife
# of a husband that survived with a child that survived ALSO survived?
# Probably pretty high. 2) (I forgot)
full %>% filter(group.size >= 3) %>% arrange(group.size, Ticket) %>% View

full <- full %>% mutate(last.name = str_extract(Name, "^[A-Za-z|']+"))
# View(full)

# Create variable last.name
full %>% filter(group.size >= 2) %>% arrange(group.size, Ticket, last.name) %>% View

# Create variables num.NA.survived, num.survived, and all.survived
full <- full %>% group_by(Ticket) %>% 
        mutate(num.NA.survived = sum(is.na(Survived)),
               num.survived = sum(Survived == 1, na.rm = T),
               all.survived = ifelse(num.survived == group.size, "YES", "NO"))

# Ungroup dataset
full <- full %>% ungroup

# Examine groups where all members of the ticket group survived
full %>% filter(all.survived == "YES") %>% arrange(group.size, Ticket) %>% View

# Examine adults with no spouse but with children. Survival rate is
# around 60%, but there are only 55 cases with known survival status.
# However, the survival rate of males and females in this subset is
# basically the same as in the overal train dataset. Womp.
full %>% filter(SibSp == 0 & Parch > 0 & Title %in% c("Mr.", "Mrs.", "Miss.")) %>% 
        filter(!is.na(Survived)) %>% 
        group_by(Sex) %>% 
        summarise(survival.rate = mean(Survived == 1),
                  n = n())

train %>% group_by(Sex) %>% summarize(survival.rate = mean(Survived == 1),
                                      n = n())


# OK but this doesn't exactly answer your question. You were interested
# in knowing the odds that a man or woman survived given that their
# children either survived or did not survive. The reason this is potentially
# important is because the test and train datasets appear to have randomly
# segmented the Titanic population, and in some cases there are unknown
# survival values for people in the same ticket party as one, two, or more
# people that all survived. What are the odds that this other unknown person
# in the same ticket group also survived?

# Create variable that tells whether or not all members in a ticket
# group died
full <- full %>% group_by(Ticket) %>% 
        mutate(all.died = ifelse(num.NA.survived == 0 & num.survived == 0, "YES", "NO"))

full %>% filter((all.survived == "YES" | all.died == "YES") & group.size > 1) %>% 
        arrange(group.size, Ticket) %>% View

# Do couples tend to live and die together? Do they tend to survive
# at a higher rate than normal?

# This isn't a perfect subset (it collects a few sibling pairs as well)
# but is mostly accurate in extracting married couples.
full %>% group_by(last.name) %>% 
        mutate(is.couple = ifelse(n() == 2 & 
                                          length(unique(Title)) == 2 &
                                          !(Title %in% c("Girl", "Miss.", "Master.")) &
                                          length(unique(Sex)) == 2 &
                                          length(unique(Ticket)) == 1 &
                                          SibSp != 0,
                                  "couple", "")) %>% 
        filter(is.couple == "couple") %>% 
        arrange(desc(is.couple), last.name) %>% 
        group_by(last.name) %>% 
        mutate(contains.na = ifelse(sum(is.na(Survived)) > 0, TRUE, FALSE),
               n = n()) %>% 
        filter(!contains.na & !(n < 2)) %>% 
        mutate(both.survived = ifelse(sum(Survived == 1) == 2, TRUE, FALSE),
                  both.died = ifelse(sum(Survived == 1) == 0, TRUE, FALSE),
                  one.survived = ifelse(sum(Survived == 1) == 1, TRUE, FALSE)) %>% 
        ungroup %>% 
        summarize(both.survived = sum(both.survived),
                  both.died = sum(both.died),
                  one.survived = sum(one.survived))

# Alright then, looks like couples did not tend to live or die together.
# Just out of curiosity, was it the man who generally died when only
# one partner survived?
full %>% group_by(last.name) %>% 
        mutate(is.couple = ifelse(n() == 2 & 
                                          length(unique(Title)) == 2 &
                                          !(Title %in% c("Girl", "Miss.", "Master.")) &
                                          length(unique(Sex)) == 2 &
                                          length(unique(Ticket)) == 1 &
                                          SibSp != 0,
                                  "couple", "")) %>% 
        filter(is.couple == "couple") %>% 
        arrange(desc(is.couple), last.name) %>% 
        group_by(last.name) %>% 
        mutate(contains.na = ifelse(sum(is.na(Survived)) > 0, TRUE, FALSE),
               n = n()) %>% 
        filter(!contains.na & !(n < 2)) %>% 
        mutate(one.survived = ifelse(sum(Survived == 1) == 1, TRUE, FALSE)) %>% 
        filter(one.survived) %>% 
        ungroup %>% 
        mutate(n = n()) %>% 
        filter(Sex == "male") %>% 
        summarize(num.men.survived = sum(Survived == 1),
                  n = unique(n))

# Wow! Literally NONE of the men survived. In cases where only one person in
# the pair survived, it was ALWAYS the woman.


# OK. So what did I learn from all this? It looks like Ticket is probably
# not going to be so useful after all.




