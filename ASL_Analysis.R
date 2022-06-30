# Importing the dataset
df = read.csv('ASL_DATA.csv')
library(ggplot2)
library(dplyr)
#SUM OF MISSIG VALUES
sum(is.na(df))

#REMOVING THE ID COLUMN
df <- subset( df, select = -ï..Id)

#####VISUALIZATION
par(mfrow=c(1,1))

#Thumb
boxplot(Thumb_Pitch ~ Letter, data = df, xlab = "Thumb Pitch",
        ylab = "Letter", main = "Thumb Pitch")

boxplot(Thumb_Roll ~ Letter, data = df, xlab = "Thumb Roll",
        ylab = "Letter", main = "Thumb Roll")

ggplot(df, aes(x = Thumb_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Thumb_Roll)) +
  geom_density(aes(color = Letter))

#Index
boxplot(Index_Pitch ~ Letter, data = df, xlab = "Index Pitch",
        ylab = "Letter", main = "Index Pitch")

boxplot(Index_Roll ~ Letter, data = df, xlab = "Index Roll",
        ylab = "Letter", main = "Index Roll")

ggplot(df, aes(x = Index_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Index_Roll)) +
  geom_density(aes(color = Letter))

#Middle
boxplot(Middle_Pitch ~ Letter, data = df, xlab = "Middle Pitch",
        ylab = "Letter", main = "Middle Pitch")

boxplot(Middle_Roll ~ Letter, data = df, xlab = "Middle Roll",
        ylab = "Letter", main = "Middle Roll")

ggplot(df, aes(x = Middle_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Middle_Roll)) +
  geom_density(aes(color = Letter))

#Ring
boxplot(Ring_Pitch ~ Letter, data = df, xlab = "Ring Pitch",
        ylab = "Letter", main = "Ring Pitch")

boxplot(Ring_Roll ~ Letter, data = df, xlab = "Ring Roll",
        ylab = "Letter", main = "Ring Roll")

ggplot(df, aes(x = Ring_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Ring_Roll)) +
  geom_density(aes(color = Letter))

#Pinky
boxplot(Pinky_Pitch ~ Letter, data = df, xlab = "Pinky Pitch",
        ylab = "Letter", main = "Pinky Pitch")

boxplot(Pinky_Roll ~ Letter, data = df, xlab = "Pinky Roll",
        ylab = "Letter", main = "Pinky Roll")

ggplot(df, aes(x = Pinky_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Pinky_Roll)) +
  geom_density(aes(color = Letter))

#Wrist
boxplot(Wrist_Pitch ~ Letter, data = df, xlab = "Wrist Pitch",
        ylab = "Letter", main = "Wrist Pitch")

boxplot(Wrist_Roll ~ Letter, data = df, xlab = "Wrist Roll",
        ylab = "Letter", main = "Wrist Roll")

ggplot(df, aes(x = Wrist_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df, aes(x = Wrist_Roll)) +
  geom_density(aes(color = Letter))


###SCALE
df2 <- as.data.frame(scale(df[1 : 12]))
df2[13] <- df[13]

df2[1] <- sqrt(df2[1])

#Thumb
boxplot(Thumb_Pitch ~ Letter, data = df2, xlab = "Thumb Pitch",
        ylab = "Letter", main = "Thumb Pitch")

boxplot(Thumb_Roll ~ Letter, data = df2, xlab = "Thumb Roll",
        ylab = "Letter", main = "Thumb Roll")

ggplot(df2, aes(x = Thumb_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Thumb_Roll)) +
  geom_density(aes(color = Letter))

#Index
boxplot(Index_Pitch ~ Letter, data = df2, xlab = "Index Pitch",
        ylab = "Letter", main = "Index Pitch")

boxplot(Index_Roll ~ Letter, data = df2, xlab = "Index Roll",
        ylab = "Letter", main = "Index Roll")

ggplot(df2, aes(x = Index_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Index_Roll)) +
  geom_density(aes(color = Letter))

#Middle
boxplot(Middle_Pitch ~ Letter, data = df2, xlab = "Middle Pitch",
        ylab = "Letter", main = "Middle Pitch")

boxplot(Middle_Roll ~ Letter, data = df2, xlab = "Middle Roll",
        ylab = "Letter", main = "Middle Roll")

ggplot(df2, aes(x = Middle_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Middle_Roll)) +
  geom_density(aes(color = Letter))

#Ring
boxplot(Ring_Pitch ~ Letter, data = df2, xlab = "Ring Pitch",
        ylab = "Letter", main = "Ring Pitch")

boxplot(Ring_Roll ~ Letter, data = df2, xlab = "Ring Roll",
        ylab = "Letter", main = "Ring Roll")

ggplot(df2, aes(x = Ring_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Ring_Roll)) +
  geom_density(aes(color = Letter))

#Pinky
boxplot(Pinky_Pitch ~ Letter, data = df2, xlab = "Pinky Pitch",
        ylab = "Letter", main = "Pinky Pitch")

boxplot(Pinky_Roll ~ Letter, data = df2, xlab = "Pinky Roll",
        ylab = "Letter", main = "Pinky Roll")

ggplot(df2, aes(x = Pinky_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Pinky_Roll)) +
  geom_density(aes(color = Letter))

#Wrist
boxplot(Wrist_Pitch ~ Letter, data = df2, xlab = "Wrist Pitch",
        ylab = "Letter", main = "Wrist Pitch")

boxplot(Wrist_Roll ~ Letter, data = df2, xlab = "Wrist Roll",
        ylab = "Letter", main = "Wrist Roll")

ggplot(df2, aes(x = Wrist_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(df2, aes(x = Wrist_Roll)) +
  geom_density(aes(color = Letter))




min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization
norm1 <- as.data.frame(lapply(df[1:12], min_max_norm))
norm1[13] <- df[13] 

#Thumb
boxplot(Thumb_Pitch ~ Letter, data = norm1, xlab = "Thumb Pitch",
        ylab = "Letter", main = "Thumb Pitch")

boxplot(Thumb_Roll ~ Letter, data = norm1, xlab = "Thumb Roll",
        ylab = "Letter", main = "Thumb Roll")

ggplot(norm1, aes(x = Thumb_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Thumb_Roll)) +
  geom_density(aes(color = Letter))

#Index
boxplot(Index_Pitch ~ Letter, data = norm1, xlab = "Index Pitch",
        ylab = "Letter", main = "Index Pitch")

boxplot(Index_Roll ~ Letter, data = norm1, xlab = "Index Roll",
        ylab = "Letter", main = "Index Roll")

ggplot(norm1, aes(x = Index_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Index_Roll)) +
  geom_density(aes(color = Letter))

#Middle
boxplot(Middle_Pitch ~ Letter, data = norm1, xlab = "Middle Pitch",
        ylab = "Letter", main = "Middle Pitch")

boxplot(Middle_Roll ~ Letter, data = norm1, xlab = "Middle Roll",
        ylab = "Letter", main = "Middle Roll")

ggplot(norm1, aes(x = Middle_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Middle_Roll)) +
  geom_density(aes(color = Letter))

#Ring
boxplot(Ring_Pitch ~ Letter, data = norm1, xlab = "Ring Pitch",
        ylab = "Letter", main = "Ring Pitch")

boxplot(Ring_Roll ~ Letter, data = norm1, xlab = "Ring Roll",
        ylab = "Letter", main = "Ring Roll")

ggplot(norm1, aes(x = Ring_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Ring_Roll)) +
  geom_density(aes(color = Letter))

#Pinky
boxplot(Pinky_Pitch ~ Letter, data = norm1, xlab = "Pinky Pitch",
        ylab = "Letter", main = "Pinky Pitch")

boxplot(Pinky_Roll ~ Letter, data = norm1, xlab = "Pinky Roll",
        ylab = "Letter", main = "Pinky Roll")

ggplot(norm1, aes(x = Pinky_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Pinky_Roll)) +
  geom_density(aes(color = Letter))

#Wrist
boxplot(Wrist_Pitch ~ Letter, data = norm1, xlab = "Wrist Pitch",
        ylab = "Letter", main = "Wrist Pitch")

boxplot(Wrist_Roll ~ Letter, data = norm1, xlab = "Wrist Roll",
        ylab = "Letter", main = "Wrist Roll")

ggplot(norm1, aes(x = Wrist_Pitch)) +
  geom_density(aes(color = Letter))

ggplot(norm1, aes(x = Wrist_Roll)) +
  geom_density(aes(color = Letter))


kruskal.test(Thumb_Pitch ~ Letter, data = norm1)
kruskal.test(Thumb_Roll ~ Letter, data = norm1)
kruskal.test(Index_Pitch ~ Letter, data = norm1)
kruskal.test(Index_Roll ~ Letter, data = norm1)
kruskal.test(Middle_Pitch ~ Letter, data = norm1)
kruskal.test(Middle_Roll ~ Letter, data = norm1)
kruskal.test(Ring_Pitch ~ Letter, data = norm1)
kruskal.test(Ring_Roll ~ Letter, data = norm1)

kruskal.test(Pinky_Pitch ~ Letter, data = norm1)
kruskal.test(Pinky_Roll ~ Letter, data = norm1)
kruskal.test(Wrist_Pitch ~ Letter, data = norm1)
kruskal.test(Wrist_Roll ~ Letter, data = norm1)


pairwise.wilcox.test(norm1$Thumb_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Thumb_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Index_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Index_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Middle_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Middle_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Ring_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Ring_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Pinky_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Pinky_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Wrist_Pitch, norm1$Letter, 
                     p.adjust.method = "BH")

pairwise.wilcox.test(norm1$Wrist_Roll, norm1$Letter, 
                     p.adjust.method = "BH")

# Correlogram in R
# required packages
library(corrplot)

head(df[1:12])
# correlation matrix
M<-cor(norm1[1:12])
head(round(M,2))

# as number
corrplot(M, method="number")

norm1$Letter = factor(norm1$Letter)

####DECISION TREE

# Splitting the dataset into
# the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(norm1$Letter,
                     SplitRatio = 0.75)
training_set = subset(norm1, split == TRUE)
test_set = subset(norm1, split == FALSE)



# Fitting Decision Tree Classification
# to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Letter ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier,
                 newdata = test_set[-13],
                 type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 13], y_pred)
cm

# Plotting the tree
plot(classifier)
text(classifier)
library(caret)
confusionMatrix(y_pred, test_set[, 13], mode = "everything", positive="1")



####SVM
# Fitting SVM to the Training set
library(e1071)

classifier = svm(formula = Letter ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-13])
# Making the Confusion Matrix
cm = table(test_set[, 13], y_pred)
cm
confusionMatrix(y_pred, test_set[, 13], mode = "everything", positive="1")


###NAIVE BAYES

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Letter ~ ., data = training_set)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_set)

# Confusion Matrix
cm <- table(test_set$Letter, y_pred)
cm
library(caret)

# Model Evaluation
confusionMatrix(cm)
confusionMatrix(y_pred, test_set[, 13], mode = "everything", positive="1")


####RANDOM FOREST
library(randomForest)

# Create random forest
# For classification
rf <- randomForest(Letter ~ .,
                        data = training_set,
                        ntree = 100,
                        proximity = TRUE,
                   )
print(rf)

#####KNN


# Fitting KNN Model 
library(e1071)
library(caTools)
library(class)


# Feature Scaling
train_scale <- scale(training_set[, 1:12])
test_scale <- scale(test_set[, 1:12])

# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Letter,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_set$Species, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_set$Letter)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Letter,
                      k = 3)
misClassError <- mean(classifier_knn != test_set$Letter)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$Letter,
                      k = 7)

misClassError <- mean(classifier_knn != test_set$Letter)
print(paste('Accuracy =', 1-misClassError))

confusionMatrix(classifier_knn, test_set[, 13], mode = "everything", positive="1")

