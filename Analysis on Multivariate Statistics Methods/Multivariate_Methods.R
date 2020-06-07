# import dataset
vowel_train = read.table('~/GSU/2019 Fall/Multivariate/CompleteDataSetsintheBook/data/vowel-train.txt', header = T, sep = ',')
train = vowel_train[,3:12]

vowel_test = read.table('~/GSU/2019 Fall/Multivariate/CompleteDataSetsintheBook/data/vowel-test.txt',header = T, sep = ',')
test = vowel_test[,3:12]

# standardize datasets
train_scaled = scale(train)
test_scaled = scale(test)


#########################################################
############ Question 1: Conduct a principal component 
############ analysis for the training data
#########################################################

# calculate the Principle Component, Proportion of Variance and Cumulative Proportion
eigenvalue=eigen(cov(train_scaled))$values
PC =eigen(cov(train_scaled))$vectors
colnames(PC)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
row.names(PC)=colnames(train_scaled)

PV = CP = 0 
for (i in 1:ncol(train_scaled)){
  PV[i]=eigenvalue[i]/sum(eigenvalue) 
  CP[i]=sum(eigenvalue[1:i])/sum(eigenvalue)}

# Principle componnet
PC 
# Proportion of Variance
PV
# Cumulative Proportion
CP 


# Using function prcomp() to find the info about PCA
# train_PCA = prcomp(train, scale= T) # k = 6
train_PCA = prcomp(train_scaled, scale= T)
summary(train_PCA)
plot(train_PCA, type = 'l')
train_PCA$rotation

# plot the resultant principal components.
biplot(train_PCA, scale = 0)



# Check the normal assumption
par(mfrow=c(2,4))
for(i in 1:7){
  qqnorm(train_PCA$x[,i], main = paste('Q-Q Plot for PC',i ))
  qqline(train_PCA$x[,i]) 
}
par(mfrow=c(2,4))
for(i in 1:7){
  plot(train_PCA$x[,i], main =paste('Plot for PC',i ), ylab = paste('PC',i) )
}


#############################################################################
########## Question 2: Get the scores of these K components 
########## for each observation, and use these scores to condcut the LDA
#############################################################################
### get the scores of these 7 components 


k=7
## option 1
# scaled dataset have ZERO MEAN
PCA_scores_Train=(train_scaled%*%PC)[,1:k]
PCA_scores_Train[1:5,]
PCA_scores_Test = (test_scaled%*%PC)[,1:k]
PCA_scores_Test[1:5,]


## option 2
k=7
train_PCA = prcomp(train, scale= T)
PCA_scores_Train=train_PCA$x[,1:k]
PCA_scores_Train[1:5,]
PCA_scores_Test = (as.matrix(test_scaled)%*%train_PCA$rotation) [,1:k]
PCA_scores_Test[1:5,]


# k = 7
# train_PCA = prcomp(train, scale= T,center = T,retx=TRUE)
# PCA_scores_Train = train_PCA$x[,1:k]
# PCA_scores_Train[1:5,]
# PCA_scores_Test= predict(train_PCA, test)[,1:k]
# PCA_scores_Test[1:5,]


library(MASS)
PCA_Train = data.frame(cbind(vowel_train['y'], PCA_scores_Train))
PCA_Train$y  = as.factor(PCA_Train$y)
PCA_Test = data.frame(cbind(vowel_test['y'], PCA_scores_Test))
PCA_Test$y = as.factor(PCA_Test$y)

### Use these scores to conduct the linear discriminant analysis (LDA) 
lda0 = lda(y~., PCA_Train)

# Calculate the misclassification error rate  
mean(PCA_Train$y  != predict(lda0, PCA_Train)$class)  
mean(PCA_Test$y != predict(lda0, PCA_Test)$class) 

### Set up the Confusion Matrices
PCA.lda_Train_table = table(PCA_Train$y, predict(lda0, PCA_Train)$class); PCA.lda_Train_table
PCA.lda_Test_table = table(PCA_Test$y, predict(lda0, PCA_Test)$class); PCA.lda_Test_table

#########################################################
########## Question 3: use these scores to condcut the QDA
#########################################################
### QDA for Training data set
qda0 = qda(y~., PCA_Train)
mean(PCA_Train$y  != predict(qda0, PCA_Train)$class)  # [1] 0.07954545
mean(PCA_Test$y != predict(qda0, PCA_Test)$class) # [1] 0.5238095 -> [1] 0.5606061

PCA.qda_Train_table = table(PCA_Train$y, predict(qda0, PCA_Train)$class); PCA.qda_Train_table 
PCA.qda_Test_table = table(PCA_Test$y, predict(qda0, PCA_Test)$class); PCA.qda_Train_table 


#########################################################
########## Question 4: For the original training dataset, 
########## conduct LDA and QDA
#########################################################
## the dataset without standardization
Train1 = vowel_train[,2:12]
Test1 = vowel_test[,2:12]

Train1$y = as.factor(Train1$y)
Test1$y = as.factor(Test1$y)

###### LDA
lda1 = lda(y~., Train1)
mean(Train1$y  != predict(lda1, Train1)$class)  #[1] 0.3162879
mean(Test1$y != predict(lda1, Test1)$class) # [1] 0.5562771

Full.lda_Train_table=table(Train1$y, predict(lda1, Train1)$class);Full.lda_Train_table
Full.lda_Test_table=table(Test1$y, predict(lda1, Test1)$class);Full.lda_Test_table

###### QDA
qda1 = qda(y~., Train1)
mean(Train1$y  != predict(qda1, Train1)$class)  # [1] 0.01136364
mean(Test1$y != predict(qda1, Test1)$class) # [1] 0.5281385

Full.qda_Train_table=table(Train1$y, predict(qda1, Train1)$class);Full.qda_Train_table
Full.qda_Test_table = table(Test1$y, predict(qda1, Test1)$class);Full.qda_Test_table



#########################################################
############ Question 5: 
#########################################################
### below two equations do the same function
# train.lda = lda(y~., vowel_train[,-1])
# train.lda1 = lda(vowel_train[,-c(1,2)], vowel_train[,2])

F1 = function(table){
  table = as.matrix(table)
  precision = diag(table) / apply(table, 2, sum)
  recall = diag(table) / apply(table, 1, sum) 
  f1 = 2 * precision * recall / (precision + recall) 
  return(round(f1,2))
}


F1(PCA.lda_Train_table)
which.min(F1(PCA.lda_Train_table))  # 2
F1(PCA.lda_Test_table)
which.min(F1(PCA.lda_Test_table))   # 2
F1(PCA.qda_Train_table)
which.min(F1(PCA.qda_Train_table))  # 2
F1(PCA.qda_Test_table)
which.min(F1(PCA.qda_Test_table))   # 10
F1(Full.lda_Train_table)
which.min(F1(Full.lda_Train_table))  # 6 
F1(Full.lda_Test_table)
which.min(F1(Full.lda_Test_table))   # 5 
F1(Full.qda_Train_table)
which.min(F1(Full.qda_Train_table))  # 6
F1(Full.qda_Test_table)
which.min(F1(Full.qda_Test_table))   # 8 



# visualize the lda plot
train.lda = lda(y~., vowel_train[,-1])
train.lda
train.ld2 = predict(train.lda, dimen = 2)
train.y = vowel_train[,2]
# calculate of center of each cluster
centers = matrix(rep(0, 22), ncol=2)
for (i in 1:11){
  centers[i, 1:2] = colMeans(train.ld2$x[train.ld2$class==i,])
}
plot(train.ld2$x, type = 'n', xlab = 'LD1', ylab = 'LD2',main = 'Plot of LDA on Training Data')
text(train.ld2$x, labels = as.character(train.y), col = 11+as.integer(train.y),cex = 0.5)
points(centers, col = 11+as.integer(train.y), pch = 1, cex = 1,lwd = 5)


test.ld2 = predict(train.lda, vowel_test[,-1], dimen = 2)
test.y = vowel_test[,2]
# calculate of center of each cluster
centers = matrix(rep(0, 22), ncol=2)
for (i in 1:11){
  centers[i, 1:2] = colMeans(test.ld2$x[test.ld2$class==i,])
}
plot(test.ld2$x, type = 'n', xlab = 'LD1', ylab = 'LD2',main = 'Plot of LDA on Test Data')
text(test.ld2$x, labels = as.character(test.y), col = 11+as.integer(test.y),cex = 0.5)
points(centers, col = 11+as.integer(test.y), pch = 1, cex = 1,lwd = 5)





### select observation 5,6,8
newTrain = vowel_train[,2:12][!(vowel_train$y == 5 | vowel_train$y ==6 | vowel_train$y == 8),]
newTest = vowel_test[,2:12][!(vowel_test$y == 5 | vowel_test$y == 6 | vowel_test$y == 8),]
newTrain$y = as.factor(newTrain$y)
newTest$y = as.factor(newTest$y)

str(newTrain)

###### LDA
lda1.1 = lda(y~., newTrain)
mean(newTrain$y  != predict(lda1.1, newTrain)$class)  #[1] 0.3162879 -> [1] 0.25
mean(newTest$y != predict(lda1.1, newTest)$class) # [1] 0.5562771 -> [1] 0.4910714

table(newTrain$y, predict(lda1.1, newTrain)$class)
table(newTest$y, predict(lda1.1, newTest)$class)

###### QDA
qda1.1 = qda(y~., newTrain)
mean(newTrain$y  != predict(qda1.1, newTrain)$class)  # [1] 0.01136364 ->[1] 0.005208333
mean(newTest$y != predict(qda1.1, newTest)$class) # [1] 0.5281385->[1] 0.4107143

table(newTrain$y, predict(qda1.1, newTrain)$class)
table(newTest$y, predict(qda1.1, newTest)$class)




#########################################################
############ Question 6: 
#########################################################
### select observation 1, 3, 6, 10
sampleTrain = vowel_train[,2:12][(vowel_train$y == 1 | vowel_train$y == 3 | vowel_train$y == 6 | vowel_train$y == 10),]
sampleTest = vowel_test[,2:12][(vowel_test$y == 1 | vowel_test$y == 3 | vowel_test$y == 6 | vowel_test$y == 10),]

sampleTrain['yy'] = rep(c(1,2,3,4),48)  
sampleTest['yy'] = rep(c(1,2,3,4), 42)

# ### complete linkage 
# dist.eu = dist(sampleTrain[,2:11],'euclidean')
# hc.eu.c = hclust(dist.eu, 'complete')
# hc.eu.c$height
# plot(hc.eu.c, hang = -1)
# cutree(hc.eu.c, k= 4)


### ward's hierarchical clustering methods 
dist.eu_train = dist(sampleTrain[,2:11],'euclidean')
hc.eu.w_train = hclust(dist.eu_train, 'ward.D2')
plot(hc.eu.w_train, hang = -1)
sampleTrain['wardClusters']=cutree(hc.eu.w_train, k= 4)
mean(sampleTrain[,12] != sampleTrain[,13])  #[1] 0.25

# plot(sampleTrain[,-c(1,12,13)], col = sampleTrain[,12], main = 'true clusters for Train')
# plot(sampleTrain[,-c(1,12,13)], col = sampleTrain[,13], main = 'clusters from ward for Train')

dist.eu_test = dist(sampleTest[,2:11],'euclidean')
hc.eu.w_test = hclust(dist.eu_test, 'ward.D2')
plot(hc.eu.w_test, hang = -1)
sampleTest['wardClusters']=cutree(hc.eu.w_test, k= 4)
mean(sampleTest[,12] != sampleTest[,13])  #[1] 0.7678571

table(sampleTrain[,12], sampleTrain[,13])
table(sampleTest[,12], sampleTest[,13])

### K-means method
sampleTrain.km = kmeans(sampleTrain[,2:11],4)
sampleTrain['kmCluster'] = sampleTrain.km$cluster
mean(sampleTrain[,12] != sampleTrain[,14])  #[1] 0.6875

# plot(sampleTrain[,-c(1,12,13,14)], col = sampleTrain[,12], main = 'true clusters for Train')
# plot(sampleTrain[,-c(1,12,13,14)], col = sampleTrain[,14], main = 'clusters from K-means for Train')
# #points(sampleTrain.km_train$centers, col = 1:4, pch = 8, cex = 2)


sampleTest.km = kmeans(sampleTest[,2:11],4)
sampleTest['kmCluster'] = sampleTest.km$cluster
mean(sampleTest[,12] != sampleTest[,14])  #[1] 0.8452381 #[1] 0.5416667 [1] 0.9702381 [1] 0.3452381

# plot(sampleTest[,-c(1,12,13,14)], col = sampleTest[,12], main = 'true clusters for Test')
# plot(sampleTest[,-c(1,12,13,14)], col = sampleTest[,14], main = 'clusters from K-means for Test')
# # points(sampleTest.km_train$centers, col = 1:4, pch = 8, cex = 2)


table(sampleTrain[,12], sampleTrain[,14])
table(sampleTest[,12], sampleTest[,14])


### Model Based clustering
library(mclust)

sampleTrain.mc = Mclust(sampleTrain[,-c(1,12,13,14)], G = 4)
sampleTrain['mcCluster'] = sampleTrain.mc$classification
mean(sampleTrain[,12] != sampleTrain[,15])  # [1] 0.78125


sampleTest.mc = Mclust(sampleTest[,-c(1,12,13,14)], G = 4)
sampleTest['mcCluster'] = sampleTest.mc$classification
mean(sampleTest[,12] != sampleTest[,15])   # [1] 0.75


table(sampleTrain[,12], sampleTrain[,15])
table(sampleTest[,12], sampleTest[,15])




#################################################################################
#################################################################################
#################################################################################
### From Q4 to Q6, the datasets with standardization are performed as follows ###
#################################################################################
#################################################################################
#################################################################################




#########################################################
########## Question 4: For the original training dataset, 
########## conduct LDA and QDA
#########################################################
## the dataset without standardization
Train1 = cbind(vowel_train['y'], train_scaled)
Test1 = cbind(vowel_test['y'], test_scaled)

Train1$y = as.factor(Train1$y)
Test1$y = as.factor(Test1$y)

###### LDA
lda1 = lda(y~., Train1)
mean(Train1$y  != predict(lda1, Train1)$class)  #[1] 0.3162879 (same)
mean(Test1$y != predict(lda1, Test1)$class) # [1] 0.547619 (little bit smaller than 0.5562771)

Full.lda_Train_table=table(Train1$y, predict(lda1, Train1)$class);Full.lda_Train_table
Full.lda_Test_table=table(Test1$y, predict(lda1, Test1)$class);Full.lda_Test_table

###### QDA
qda1 = qda(y~., Train1)
mean(Train1$y  != predict(qda1, Train1)$class)  # [1] 0.01136364
mean(Test1$y != predict(qda1, Test1)$class) # [1] 0.5844156 (little bit larger than 0.5281385)

Full.qda_Train_table=table(Train1$y, predict(qda1, Train1)$class);Full.qda_Train_table
Full.qda_Test_table = table(Test1$y, predict(qda1, Test1)$class);Full.qda_Test_table



#########################################################
############ Question 5: 
#########################################################
### below two equations do the same function
# train.lda = lda(y~., vowel_train[,-1])
# train.lda1 = lda(vowel_train[,-c(1,2)], vowel_train[,2])

F1 = function(table){
  table = as.matrix(table)
  precision = diag(table) / apply(table, 2, sum)
  recall = diag(table) / apply(table, 1, sum) 
  f1 = 2 * precision * recall / (precision + recall) 
  return(round(f1,2))
}

####  tables for PCA are the same as previous
# F1(PCA.lda_Train_table)
# which.min(F1(PCA.lda_Train_table))  # 2
# F1(PCA.lda_Test_table)
# which.min(F1(PCA.lda_Test_table))   # 2 
# F1(PCA.qda_Train_table)
# which.min(F1(PCA.qda_Train_table))  # 2 
# F1(PCA.qda_Test_table)
# which.min(F1(PCA.qda_Test_table))   # 10
F1(Full.lda_Train_table)
which.min(F1(Full.lda_Train_table))  # 6 
F1(Full.lda_Test_table)
which.min(F1(Full.lda_Test_table))   # 2 (from 5 change to 2) 
F1(Full.qda_Train_table)
which.min(F1(Full.qda_Train_table))  # 6
F1(Full.qda_Test_table)
which.min(F1(Full.qda_Test_table))   # 8 



# visualize the lda plot
train.lda = lda(y~., Train1)
train.lda
train.ld2 = predict(train.lda, dimen = 2)
train.y = Train1$y
# calculate of center of each cluster
par(mfrow = c(1,2))
centers = matrix(rep(0, 22), ncol=2)
for (i in 1:11){
  centers[i, 1:2] = colMeans(train.ld2$x[train.ld2$class==i,])
}
plot(train.ld2$x, type = 'n', xlab = 'LD1', ylab = 'LD2',main = 'Plot of LDA on Standardized Training Data')
text(train.ld2$x, labels = as.character(train.y), col = 11+as.integer(train.y),cex = 0.5)
points(centers, col = 11+as.integer(train.y), pch = 1, cex = 1,lwd = 5)


test.ld2 = predict(train.lda, Test1, dimen = 2)
test.y = Test1$y
# calculate of center of each cluster
centers = matrix(rep(0, 22), ncol=2)
for (i in 1:11){
  centers[i, 1:2] = colMeans(test.ld2$x[test.ld2$class==i,])
}
plot(test.ld2$x, type = 'n', xlab = 'LD1', ylab = 'LD2',main = 'Plot of LDA on Standardized Test Data')
text(test.ld2$x, labels = as.character(test.y), col = 11+as.integer(test.y),cex = 0.5)
points(centers, col = 11+as.integer(test.y), pch = 1, cex = 1,lwd = 5)





### select observation 2,6,8
## convert y from factor back to int. 
## Otherwise factor level 2, 6, 8 are still in dataframe with none values
## which will yield error for QDA:
###### Error in qda.default(x, grouping, ...) : 
###### some group is too small for 'qda'
Train1$y = as.numeric(as.character(Train1$y))
Test1$y = as.numeric(as.character(Test1$y))
newTrain = Train1[!(Train1$y == 2 | Train1$y ==6 | Train1$y == 8),]
newTest = Test1[!(Test1$y == 2 | Test1$y == 6 | Test1$y == 8),]
newTrain$y = as.factor(newTrain$y)
newTest$y = as.factor(newTest$y)

###### LDA
lda1.1 = lda(y~., newTrain)
mean(newTrain$y  != predict(lda1.1, newTrain)$class)  #[1] 0.2265625 (little bit smaller than 0.25)
mean(newTest$y != predict(lda1.1, newTest)$class) # [1] 0.4375 (little bit smaller than 0.4910714)

table(newTrain$y, predict(lda1.1, newTrain)$class)
table(newTest$y, predict(lda1.1, newTest)$class)

###### QDA 

qda1.1 = qda(y~., newTrain)
mean(newTrain$y  != predict(qda1.1, newTrain)$class)  # [1] 0.002604167 (little bit smaller than 0.005208333)
mean(newTest$y != predict(qda1.1, newTest)$class) # [1] 0.4761905  (greater than 0.4107143)

table(newTrain$y, predict(qda1.1, newTrain)$class)
table(newTest$y, predict(qda1.1, newTest)$class)


#########################################################
############ Question 6: 
#########################################################
### select observation 1, 3, 6, 10
sampleTrain = Train1[(Train1$y == 1 | Train1$y == 3 | Train1$y == 6 | Train1$y == 10),]
sampleTest = Test1[(Test1$y == 1 | Test1$y == 3 | Test1$y == 6 | Test1$y == 10),]

sampleTrain['yy'] = as.factor(rep(c(1,2,3,4),48))
sampleTest['yy'] = as.factor(rep(c(1,2,3,4), 42))

# ### complete linkage 
# dist.eu = dist(sampleTrain[,2:11],'euclidean')
# hc.eu.c = hclust(dist.eu, 'complete')
# hc.eu.c$height
# plot(hc.eu.c, hang = -1)
# cutree(hc.eu.c, k= 4)


### ward's hierarchical clustering methods 
dist.eu_train = dist(sampleTrain[,2:11],'euclidean')
hc.eu.w_train = hclust(dist.eu_train, 'ward.D2') #[1] 0.71875
hc.eu.w_train = hclust(dist.eu_train, 'complete') # [1] 0.65625
hc.eu.w_train = hclust(dist.eu_train, 'single') # 0.71875
hc.eu.w_train = hclust(dist.eu_train, 'average') # [1] 0.84375
# plot(hc.eu.w_train, hang = -1, main = 'Cluster for Training Data')
sampleTrain['wardClusters']=cutree(hc.eu.w_train, k= 4)
mean(sampleTrain[,12] != sampleTrain[,13])  #[1] 0.71875 (> 0.25)


dist.eu_test = dist(sampleTest[,2:11],'euclidean')
hc.eu.w_test = hclust(dist.eu_test, 'ward.D2')  # [1] 0.5714286
hc.eu.w_test = hclust(dist.eu_test, 'complete') #[1] 0.6964286
hc.eu.w_test = hclust(dist.eu_test, 'single') # [1] 0.5714286
hc.eu.w_test = hclust(dist.eu_test, 'average') # [1] 0.6964286
# plot(hc.eu.w_test, hang = -1,main = 'Cluster for Test Data')
sampleTest['wardClusters']=cutree(hc.eu.w_test, k= 4)
mean(sampleTest[,12] != sampleTest[,13])  #[1] 0.5714286 (<0.7678571)

table(sampleTrain[,12], sampleTrain[,13])
table(sampleTest[,12], sampleTest[,13])

### K-means method
ite = 10000
nn = array(rep(0, ite))
for (i in 1: ite) {
  sampleTrain.km = kmeans(sampleTrain[,2:11],4)
  sampleTrain['kmCluster'] = sampleTrain.km$cluster
  nn[i]= mean(sampleTrain[,12] != sampleTrain[,14])  #[1] 0.6875
}
mean(nn)


nn = array(rep(0, ite))
for (i in 1: ite) {
  sampleTest.km = kmeans(sampleTest[,2:11],4)
  sampleTest['kmCluster'] = sampleTest.km$cluster
  nn[i]=mean(sampleTest[,12] != sampleTest[,14])  #[1] 0.8452381 #[1] 0.5416667 [1] 0.9702381 [1] 0.3452381
}
mean(nn)



table(sampleTrain[,12], sampleTrain[,14])
table(sampleTest[,12], sampleTest[,14])


### Model-Based clustering
library(mclust)

sampleTrain.mc = Mclust(sampleTrain[,-c(1,12,13,14)], G = 4)
sampleTrain['mcCluster'] = sampleTrain.mc$classification
mean(sampleTrain[,12] != sampleTrain[,15])  # [1] 0.78125 (= 0.78125)


sampleTest.mc = Mclust(sampleTest[,-c(1,12,13,14)], G = 4)
sampleTest['mcCluster'] = sampleTest.mc$classification
mean(sampleTest[,12] != sampleTest[,15])   # [1] 0.75 ( = 0.75)


table(sampleTrain[,12], sampleTrain[,15])
table(sampleTest[,12], sampleTest[,15])
