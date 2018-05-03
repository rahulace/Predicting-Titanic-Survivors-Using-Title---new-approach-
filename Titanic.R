library(ggplot2)
library(stringr)

setwd("C:/Users/DELL/Desktop/Projects/Predicting-Titanic-Survivors-Using-Title---new-approach-")

#Loading raw data
train <- read.csv("train_titanic.csv")
test <- read.csv("test_titanic.csv")

#Adding "survived" variable to the test set to allow combining dataset
test.survived <- data.frame(Survived = rep("None", nrow=(test)),test[,])
test.survived                            
data.combined <- rbind(train, test.survived)

#Data type of dataset
str(data.combined)

#Converting datatype to factor
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Sex <- as.factor(data.combined$Sex)

#survival rates
table(data.combined$Survived)



str(train)

#Survival rate as per class
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
ggplot(train, aes(x = Pclass, fill = train$Survived)) + 
  geom_bar(width = 0.5) + 
  xlab("Pclass") + 
  ylab("Total count") + 
  labs(fill = "Survived")

#Coverting "Names" to character
train$Name <- as.character(train$Name) 

#To check unique names in training data set
length(unique(as.character(data.combined$Name)))

#Get duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
dup.names


#To check if title has any correlation with other variables
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses

mres <- data.combined[which(str_detect(data.combined$Name, "Mr")),]
mres

masters <- data.combined[which(str_detect(data.combined$Name, "Master")),]
masters

#Create function to extract titles
titlecreator <- function(Name) { 
  Name <- as.character(Name)
  
  if (length(grep("Miss", Name)) > 0) {
    return("Miss")
  } else if (length(grep("Mrs", Name)) > 0) {
    return("Mrs")
  } else if (length(grep("Master", Name)) > 0) {
    return("Master")
  } else if (length(grep("Mr", Name)) > 0) {
    return("Mr")
  } else {
    return("Other")
    }
}
 
Titles <- NULL
for(i in 1:nrow(data.combined)) {
  Titles <- c(Titles, titlecreator(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(Titles)

# To check survival rate with titles
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") + 
  ylab("Total count") + 
  labs(fill = "Survived")

#Distribution of males and females in dataset
table(data.combined$Sex)


#Visualize 3 way relation between Sex, Class and survival rate
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") + 
  ylab("Total count") + 
  labs(fill = "Survived")

#Females have higher survival rate than males

#Relation between Sex, Class and survival rate
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) + 
  facet_wrap(~Sex + ~Pclass) +
  geom_histogram(binwidth = 10)+
  xlab("Age") + 
  ylab("Total count")


#Distribution of age over entire dataset
summary(data.combined$Age)

#To see which title has maximum Na's in age
summary(misses$Age)
summary(masters$Age)
summary(mres$Age) #highest no. of NA's
summary(mrses$Age)

#Relation between Sex and survival rate for titles = "misses"
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) + 
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5)+
  xlab("Age") + 
  ylab("Total count")


# Exploring sibsp variable
summary(data.combined$SibSp)

#Converting sibsp to factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

#Relation between Sibsp, Class and survival rate
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title" ) +
  xlab("SibSp") + 
  ylab("Total count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Title is difinetly a strong predictor

# Exploring Parch variable
summary(data.combined$Parch)

#Converting Parch to factor
data.combined$Parch <- as.factor(data.combined$Parch)

#Relation between Parch, Class and survival rate
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title" ) +
  xlab("Parch") + 
  ylab("Total count") +
  ylim(0,300) +
  labs(fill = "Survived")


#Creating a family size feature
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.SibSp + temp.Parch + 1)


#Relation between Family Size, Class and survival rate
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title" ) +
  xlab("Family Size") + 
  ylab("Total count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Exploring Fares variable
summary(data.combined$Fare)
str(data.combined$Fare)

#Visualizing fare
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,300)

#Relation between Fare, Class and survival rate
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title" ) +
  xlab("Fare") + 
  ylab("Total count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Exploring Embarked variable
summary(data.combined$Embarked)
str(data.combined$Embarked)

#Relation between Embarked, Class and survival rate
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) + 
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title" ) +
  xlab("Embarked") + 
  ylab("Total count") +
  ylim(0,300) +
  labs(fill = "Survived")



######################################################################################

#Prdictive Model
#Random Forst

library(randomForest)

#Model1 = Train set with only Pclass and Title
rf.train1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train1, y = rf.label, importance = T, ntree = 1000)
rf.1 # 79.01% accracy
varImpPlot(rf.1) #Title is way stronger predictor than Pclass



#Model2 = Train set with only Pclass, SibSp and Title
rf.train2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train2, y = rf.label, importance = T, ntree = 1000)
rf.2 # 80.07% accracy
varImpPlot(rf.2) #Title is way stronger predictor than Pclass



#Model3 = Train set with only Pclass, SibSp, Parch and Title
rf.train3 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train3, y = rf.label, importance = T, ntree = 1000)
rf.3 # 80.92% accracy
varImpPlot(rf.3) #Title is way stronger predictor than Pclass



#Model4 = Train set with only Pclass, Family Size and Title
rf.train4 <- data.combined[1:891, c("Pclass", "Title", "family.size")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train4, y = rf.label, importance = T, ntree = 1000)
rf.4 # 81.82% accracy
varImpPlot(rf.4) #Family Size is a stronger predictor than Parch and SibSp


#Model5 = Train set with only Pclass, Family Size, Fare and Title
rf.train5 <- data.combined[1:891, c("Pclass", "Title", "family.size", "Fare")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train5, y = rf.label, importance = T, ntree = 1000)
rf.5 # 83.39% accracy
varImpPlot(rf.5) #Combination of Family Size and Fare brings better accuracy


#Model6 = Train set with only Pclass, Family Size, Fare, Embarked and Title
rf.train6 <- data.combined[1:891, c("Pclass", "Title", "family.size", "Fare", "Embarked")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train6, y = rf.label, importance = T, ntree = 1000)
rf.6 # 81.37% accracy
varImpPlot(rf.6) #Embarked brings down accuracy, hence mot required


# Best Randon Forest model is Model 4 - combination of Pclass, Family Size, Fare and Title
# Our feature engineered variable "Tilte" is strongest predictor of Titanic Passengers survival rate!

