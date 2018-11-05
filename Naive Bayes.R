# Data preprocessing

dataset = read.csv('student-mat-logit.csv')

# no null variables present in the dataset

# Encoding categorical data 
dataset$school = factor(dataset$school,
                        levels = c('GP', 'MS'),
                        labels = c(1,2) )

dataset$sex = factor(dataset$sex,
                     levels= c('M', 'F'),
                     labels= c('1', '2') )

dataset$address = factor(dataset$address,
                         levels = c('U', 'R'),
                         labels = c(1,2) )

dataset$famsize = factor(dataset$famsize,
                         levels= c('LE3', 'GT3'),
                         labels= c('1', '2') )


dataset$Pstatus = factor(dataset$Pstatus,
                         levels= c('A', 'T'),
                         labels= c('1', '2') )

dataset$Mjob = factor(dataset$Mjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

dataset$Fjob = factor(dataset$Fjob,
                      levels= c('at_home', 'health', 'other', 'services', 'teacher'),
                      labels= c('1', '2', '3', '4', '5') )

dataset$reason = factor(dataset$reason,
                        levels= c('course', 'home','other','reputation'),
                        labels= c('1', '2','3','4') )

dataset$guardian = factor(dataset$guardian,
                          levels= c('father', 'mother','other'),
                          labels= c('1', '2','3') )


dataset$schoolsup = factor(dataset$schoolsup,
                           levels= c('yes', 'no'),
                           labels= c('1', '0') )

dataset$famsup = factor(dataset$famsup,
                        levels = c('yes', 'no'),
                        labels = c('1', '0') ) 


dataset$paid = factor(dataset$paid,
                      levels= c('yes', 'no'),
                      labels= c('1', '0') )

dataset$activities = factor(dataset$activities,
                            levels= c('yes', 'no'),
                            labels= c('1', '0') )

dataset$nursery = factor(dataset$nursery,
                         levels= c('yes', 'no'),
                         labels= c('1', '0') )

dataset$higher = factor(dataset$higher,
                        levels= c('yes', 'no'),
                        labels= c('1', '0') )

dataset$internet = factor(dataset$internet,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )

dataset$romantic = factor(dataset$romantic,
                          levels= c('yes', 'no'),
                          labels= c('1', '0') )

#Encoding the target feature as factor
dataset$Pass.Fail = factor(dataset$Pass.Fail, levels = c(0,1))

#Splitting into training set and test set
#install.packages('caTools')  
library(caTools)
set.seed(123)
split = sample.split(dataset$Pass.Fail, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE) 



#Fitting Naive Bayes to the training set
library(e1071)
classifier = naiveBayes(x = training_set[,c(2, 3, 15, 16, 25, 26, 29, 30)], 
                        y = training_set$Pass.Fail)

y_pred = predict(classifier, newdata = test_set[,c(2, 3, 15, 16, 25, 26, 29, 30)])
y_pred_nb = performance(y_pred)
cm = table(test_set[,31], y_pred)

install.packages("pROC")
plot(roc(y_pred))

install.packages("ROCR")
