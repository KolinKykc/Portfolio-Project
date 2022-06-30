# Importing the dataset
df = read.csv('Postures.csv')
library(ggplot2)
library(dplyr)

#Removing the first row containing just 0
df = df[-1,]


table(df$X0 == '?', useNA = 'always')
table(df$Y0 == '?', useNA = 'always')
table(df$Z0 == '?', useNA = 'always')

table(df$X1 == '?', useNA = 'always')
table(df$Y1 == '?', useNA = 'always')
table(df$Z1 == '?', useNA = 'always')

table(df$X2 == '?', useNA = 'always')
table(df$Y2 == '?', useNA = 'always')
table(df$Z2 == '?', useNA = 'always')

table(df$X3 == '?', useNA = 'always')
table(df$Y3 == '?', useNA = 'always')
table(df$Z3 == '?', useNA = 'always')

table(df$X4 == '?', useNA = 'always')
table(df$Y4 == '?', useNA = 'always')
table(df$Z4 == '?', useNA = 'always')

table(df$X5 == '?', useNA = 'always')
table(df$Y5 == '?', useNA = 'always')
table(df$Z5 == '?', useNA = 'always')

table(df$X6 == '?', useNA = 'always')
table(df$Y6 == '?', useNA = 'always')
table(df$Z6 == '?', useNA = 'always')


table(df$X7 == '?', useNA = 'always')
table(df$Y7 == '?', useNA = 'always')
table(df$Z7 == '?', useNA = 'always')

table(df$X8 == '?', useNA = 'always')
table(df$Y8 == '?', useNA = 'always')
table(df$Z8 == '?', useNA = 'always')

table(df$X9 == '?', useNA = 'always')
table(df$Y9 == '?', useNA = 'always')
table(df$Z9 == '?', useNA = 'always')

table(df$X10 == '?', useNA = 'always')
table(df$Y10 == '?', useNA = 'always')
table(df$Z10 == '?', useNA = 'always')

table(df$X11 == '?', useNA = 'always')
table(df$Y11 == '?', useNA = 'always')
table(df$Z11 == '?', useNA = 'always')


table(df == 0, useNA = 'always')
table(df == '?', useNA = 'always')

df2 = df[,1:35]

df2[df2 == "?"] <- 0
df2 <- as.data.frame(sapply(df2,as.numeric))

summary(df2)

#0
boxplot(X0 ~ Class, data = df2, xlab = "X0",
        ylab = "Class", main = "X0")

boxplot(Y0 ~ Class, data = df2, xlab = "Y0",
        ylab = "Class", main = "Y0")

boxplot(Z0 ~ Class, data = df2, xlab = "Z0",
        ylab = "Class", main = "Z0")


 
ggplot(data = df2, 
       aes(x = X0, y = Y0, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X0, y = Z0, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y0, y = Z0, col = factor(Class))) +
  geom_point()

#1
boxplot(X1 ~ Class, data = df2, xlab = "X1",
        ylab = "Class", main = "X1")

boxplot(Y1 ~ Class, data = df2, xlab = "Y1",
        ylab = "Class", main = "Y1")

boxplot(Z1 ~ Class, data = df2, xlab = "Z1",
        ylab = "Class", main = "Z1")



 
ggplot(data = df2, 
       aes(x = X1, y = Y1, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X1, y = Z1, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y1, y = Z1, col = factor(Class))) +
  geom_point()


#2
boxplot(X2 ~ Class, data = df2, xlab = "X2",
        ylab = "Class", main = "X2")

boxplot(Y2 ~ Class, data = df2, xlab = "Y2",
        ylab = "Class", main = "Y2")

boxplot(Z2 ~ Class, data = df2, xlab = "Z2",
        ylab = "Class", main = "Z2")



 
ggplot(data = df2, 
       aes(x = X2, y = Y2, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X2, y = Z2, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y2, y = Z2, col = factor(Class))) +
  geom_point()


#3
boxplot(X3 ~ Class, data = df2, xlab = "X3",
        ylab = "Class", main = "X3")

boxplot(Y3 ~ Class, data = df2, xlab = "Y3",
        ylab = "Class", main = "Y3")

boxplot(Z3 ~ Class, data = df2, xlab = "Z3",
        ylab = "Class", main = "Z3")

 
ggplot(data = df2, 
       aes(x = X3, y = Y3, col = factor(Class))) +
  geom_point()
#x3y3

 
ggplot(data = df2, 
       aes(x = X3, y = Z3, col = factor(Class))) +
  geom_point()
#x3z3

 
ggplot(data = df2, 
       aes(x = Y3, y = Z3, col = factor(Class))) +
  geom_point()
#y3z3

#4
boxplot(X4 ~ Class, data = df2, xlab = "X4",
        ylab = "Class", main = "X4")

boxplot(Y4 ~ Class, data = df2, xlab = "Y4",
        ylab = "Class", main = "Y4")

boxplot(Z4 ~ Class, data = df2, xlab = "Z4",
        ylab = "Class", main = "Z4")



 
ggplot(data = df2, 
       aes(x = X4, y = Y4, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X4, y = Z4, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y4, y = Z4, col = factor(Class))) +
  geom_point()


#5
boxplot(X5 ~ Class, data = df2, xlab = "X5",
        ylab = "Class", main = "X5")

boxplot(Y5 ~ Class, data = df2, xlab = "Y5",
        ylab = "Class", main = "Y5")

boxplot(Z5 ~ Class, data = df2, xlab = "Z5",
        ylab = "Class", main = "Z5")

 
ggplot(data = df2, 
       aes(x = X5, y = Y5, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X5, y = Z5, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y5, y = Z5, col = factor(Class))) +
  geom_point()




#6
boxplot(X6 ~ Class, data = df2, xlab = "X6",
        ylab = "Class", main = "X6")

boxplot(Y6 ~ Class, data = df2, xlab = "Y6",
        ylab = "Class", main = "Y6")

boxplot(Z6 ~ Class, data = df2, xlab = "Z6",
        ylab = "Class", main = "Z6")



 
ggplot(data = df2, 
       aes(x = X6, y = Y6, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X6, y = Z6, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y6, y = Z6, col = factor(Class))) +
  geom_point()


#7
boxplot(X7 ~ Class, data = df2, xlab = "X7",
        ylab = "Class", main = "X7")

boxplot(Y7 ~ Class, data = df2, xlab = "Y7",
        ylab = "Class", main = "Y7")

boxplot(Z7 ~ Class, data = df2, xlab = "Z7",
        ylab = "Class", main = "Z7")



 
ggplot(data = df2, 
       aes(x = X7, y = Y7, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X7, y = Z7, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y7, y = Z7, col = factor(Class))) +
  geom_point()



#8
png("x8box.png")
boxplot(X8 ~ Class, data = df2, xlab = "X8",
        ylab = "Class", main = "X8")
dev.off()

png("y8box.png")
boxplot(Y8 ~ Class, data = df2, xlab = "Y8",
        ylab = "Class", main = "Y8")
dev.off()

png("z8box.png")
boxplot(Z8 ~ Class, data = df2, xlab = "Z8",
        ylab = "Class", main = "Z8")
dev.off()


 
png("x8y8.png")
ggplot(data = df2, 
       aes(x = X8, y = Y8, col = factor(Class))) +
  geom_point()
dev.off()

 
png("x8z8.png")
ggplot(data = df2, 
       aes(x = X8, y = Z8, col = factor(Class))) +
  geom_point()
dev.off()

 
png("y8z8.png")
ggplot(data = df2, 
       aes(x = Y8, y = Z8, col = factor(Class))) +
  geom_point()
dev.off()


#9
boxplot(X9 ~ Class, data = df2, xlab = "X9",
        ylab = "Class", main = "X9")

boxplot(Y9 ~ Class, data = df2, xlab = "Y9",
        ylab = "Class", main = "Y9")

boxplot(Z9 ~ Class, data = df2, xlab = "Z9",
        ylab = "Class", main = "Z9")



 
ggplot(data = df2, 
       aes(x = X9, y = Y9, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X9, y = Z9, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y9, y = Z9, col = factor(Class))) +
  geom_point()


#10
boxplot(X10 ~ Class, data = df2, xlab = "X10",
        ylab = "Class", main = "X10")

boxplot(Y10 ~ Class, data = df2, xlab = "Y10",
        ylab = "Class", main = "Y10")

boxplot(Z10 ~ Class, data = df2, xlab = "Z10",
        ylab = "Class", main = "Z10")



 
ggplot(data = df2, 
       aes(x = X10, y = Y10, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = X10, y = Z10, col = factor(Class))) +
  geom_point()

 
ggplot(data = df2, 
       aes(x = Y10, y = Z10, col = factor(Class))) +
  geom_point()


kruskal.test(X0 ~ Class, data = df2)


pairwise.wilcox.test(df2$X0, df2$X0, 
                     p.adjust.method = "BH")





#9
png("x9box.png")
boxplot(X9 ~ Class, data = df2, xlab = "X9",
        ylab = "Class", main = "X9")
dev.off()

png("y9box.png")
boxplot(Y9 ~ Class, data = df2, xlab = "Y9",
        ylab = "Class", main = "Y9")
dev.off()

png("z9box.png")
boxplot(Z9 ~ Class, data = df2, xlab = "Z9",
        ylab = "Class", main = "Z9")
dev.off()


 
png("x9y9.png")
ggplot(data = df2, 
       aes(x = X9, y = Y9, col = factor(Class))) +
  geom_point()
dev.off()

 
png("x9z9.png")
ggplot(data = df2, 
       aes(x = X9, y = Z9, col = factor(Class))) +
  geom_point()
dev.off()

 
png("y9z9.png")
ggplot(data = df2, 
       aes(x = Y9, y = Z9, col = factor(Class))) +
  geom_point()
dev.off()

#10
png("x10box.png")
boxplot(X10 ~ Class, data = df2, xlab = "X10",
        ylab = "Class", main = "X10")
dev.off()

png("y10box.png")
boxplot(Y10 ~ Class, data = df2, xlab = "Y10",
        ylab = "Class", main = "Y10")
dev.off()

png("z10box.png")
boxplot(Z10 ~ Class, data = df2, xlab = "Z10",
        ylab = "Class", main = "Z10")
dev.off()


 
png("x10y10.png")
ggplot(data = df2, 
       aes(x = X10, y = Y10, col = factor(Class))) +
  geom_point()
dev.off()

 
png("x10z10.png")
ggplot(data = df2, 
       aes(x = X10, y = Z10, col = factor(Class))) +
  geom_point()
dev.off()

 
png("y10z10.png")
ggplot(data = df2, 
       aes(x = Y10, y = Z10, col = factor(Class))) +
  geom_point()
dev.off()




kruskal.test(X0 ~ Class, data = df2)
kruskal.test(X1 ~ Class, data = df2)
kruskal.test(X2 ~ Class, data = df2)
kruskal.test(X3 ~ Class, data = df2)
kruskal.test(X4 ~ Class, data = df2)
kruskal.test(X5 ~ Class, data = df2)
kruskal.test(X6 ~ Class, data = df2)
kruskal.test(X7 ~ Class, data = df2)
kruskal.test(X8 ~ Class, data = df2)
kruskal.test(X9 ~ Class, data = df2)
kruskal.test(X10 ~ Class, data = df2)

kruskal.test(Y0 ~ Class, data = df2)
kruskal.test(Y1 ~ Class, data = df2)
kruskal.test(Y2 ~ Class, data = df2)
kruskal.test(Y3 ~ Class, data = df2)
kruskal.test(Y4 ~ Class, data = df2)
kruskal.test(Y5 ~ Class, data = df2)
kruskal.test(Y6 ~ Class, data = df2)
kruskal.test(Y7 ~ Class, data = df2)
kruskal.test(Y8 ~ Class, data = df2)
kruskal.test(Y9 ~ Class, data = df2)
kruskal.test(Y10 ~ Class, data = df2)

kruskal.test(Z0 ~ Class, data = df2)
kruskal.test(Z1 ~ Class, data = df2)
kruskal.test(Z2 ~ Class, data = df2)
kruskal.test(Z3 ~ Class, data = df2)
kruskal.test(Z4 ~ Class, data = df2)
kruskal.test(Z5 ~ Class, data = df2)
kruskal.test(Z6 ~ Class, data = df2)
kruskal.test(Z7 ~ Class, data = df2)
kruskal.test(Z8 ~ Class, data = df2)
kruskal.test(Z9 ~ Class, data = df2)
kruskal.test(Z10 ~ Class, data = df2)


pairwise.wilcox.test(df2$X0, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X1, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X2, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X3, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X4, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X5, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X6, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X7, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X8, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X9, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$X10, df2$Class, 
                     p.adjust.method = "BH")



pairwise.wilcox.test(df2$Y0, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y1, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y2, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y3, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y4, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y5, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y6, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y7, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y8, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y9, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Y10, df2$Class, 
                     p.adjust.method = "BH")


pairwise.wilcox.test(df2$Z0, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z1, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z2, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z3, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z4, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z5, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z6, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z7, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z8, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z9, df2$Class, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(df2$Z10, df2$Class, 
                     p.adjust.method = "BH")



####DECISION TREE
df2$Class = factor(df2$Class)

# Splitting the dataset into
# the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df2$Class,
                     SplitRatio = 0.75)
training_set = subset(df2, split == TRUE)
test_set = subset(df2, split == FALSE)

# Fitting Decision Tree Classification
# to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Class ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier,
                 newdata = test_set[-1],
                 type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
cm

# Plotting the tree
plot(classifier)
text(classifier)


# Correlogram in R
# required packages
library(corrplot)

head(df2[1:35])
# correlation matrix
M<-cor(df2[1:35])
head(round(M,2))

# as number
corrplot(M, method="number")
confusionMatrix(y_pred, test_set[, 1], mode = "everything", positive="1")














####SVM
# Fitting SVM to the Training set
library(e1071)

classifier = svm(formula = Class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-1])
# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
cm

# as number
corrplot(M, method="number")
confusionMatrix(y_pred, test_set[, 1], mode = "everything", positive="1")













###NAIVE BAYES

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Class ~ ., data = training_set)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_set)

# Confusion Matrix
cm <- table(test_set$Class, y_pred)
cm
library(caret)
confusionMatrix(y_pred, test_set[, 1], mode = "everything", positive="1")

# Model Evaluation
confusionMatrix(cm)



















#####KNN
# Fitting KNN Model 
library(e1071)
library(caTools)
library(class)


# Feature Scaling
train_scale <- scale(training_set[, 1:35])
test_scale <- scale(test_set[, 1:35])

# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Class,
                      k = 1)
classifier_knn
y_pred <- predict(classifier_knn, newdata = test_set)

# Confusiin Matrix
cm <- table(test_set$Class, classifier_knn)
cm
confusionMatrix(y_pred, test_set[, 1], mode = "everything", positive="1")

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_set$Class)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Class,
                      k = 3)
misClassError <- mean(classifier_knn != test_set$Class)
print(paste('Accuracy =', 1-misClassError))
# Confusiin Matrix
cm <- table(test_set$Class, classifier_knn)
cm
confusionMatrix(y_pred, as.factor(test_set[, 1]), mode = "everything", positive="1")

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Class,
                      k = 7)

y_pred <- predict(classifier_knn, newdata = test_set)

misClassError <- mean(classifier_knn != test_set$Class)
print(paste('Accuracy =', 1-misClassError))

# Confusiin Matrix
cm <- table(test_set$Class, classifier_knn)
cm
confusionMatrix(y_pred, as.factor(test_set[, 1]), mode = "everything", positive="1")


###NAIVE BAYES

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
naiveB <- naiveBayes(Class ~ ., data = training_set)
naiveB

# Predicting on test data'
y_pred <- predict(naiveB, newdata = test_set)

# Confusion Matrix
cm <- table(test_set$Class, y_pred)
cm
library(caret)

# Model Evaluation
confusionMatrix(cm)
confusionMatrix(y_pred, test_set[, 1], mode = "everything", positive="1")
library(MLmetrics)
F1_Score(y_pred, test_set[, 1])


