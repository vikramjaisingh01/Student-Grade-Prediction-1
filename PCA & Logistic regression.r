# Data preprocessing

dataset = read.csv('student-mat-logit.csv')

#install.packages("dummies")
library(dummies)

student = data.frame(dataset)

student_new = dummy.data.frame(student, sep = ".")

student_new[-57] = scale(student_new[-57])


#Splitting into training set and test set
#install.packages('caTools')  
library(caTools)
set.seed(123)
split = sample.split(student_new$Pass.Fail, SplitRatio = 0.8)
training_set = subset(student_new, split == TRUE)
test_set = subset(student_new, split == FALSE) 


# PCA 
library(caret)
library(e1071)


prin_comp = prcomp(training_set[-57], scale. = T)
names(prin_comp)
biplot(prin_comp, scale = 0)
std_dev = prin_comp$sdev
pr_var= std_dev^2
pr_var[1:20]
prop_varex = pr_var/sum(pr_var)
prop_varex[1:20]
#scree plot
plot(prop_varex, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

prin_comp

summary(prin_comp)

#crating new training and test set
training_set_pca = predict(prin_comp, training_set)
training_set_pca = data.frame(training_set_pca)
training_set_pca$Pass.Fail = training_set$Pass.Fail


test_set_pca = predict(prin_comp, test_set)
test_set_pca = data.frame(test_set_pca) 
test_set_pca$Pass.Fail = test_set$Pass.Fail


#Applying logistic regression
classifier = glm(formula = Pass.Fail~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  + PC11 + PC12 + PC13 + PC14 + PC15,
                 family = binomial, 
                 data = training_set_pca)

prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-57])
prob_pred

y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

#making the confusion matrix
cm = table(test_set_pca[, 57], y_pred)
cm


#Applying logistic regression
classifier = glm(formula = Pass.Fail~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                 ,
                 family = binomial, 
                 data = training_set_pca)

prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-57])
prob_pred

y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

#making the confusion matrix
cm = table(test_set_pca[, 57], y_pred)
