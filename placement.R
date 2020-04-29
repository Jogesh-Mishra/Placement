df<- read.csv("C:/Users/JOGESH MISHRA/Documents/Placement_Data_Full_Class.csv",header = TRUE)
View(df)
############################## EDD ANALYSIS ##############################

summary(df)
df<-df[,-1]   #REMOVING THE SERIEL NUMBER COLUMN
summary(df)

############################## Creating Barplot ##############################

barplot(table(df$gender))
barplot(table(df$ssc_b))
barplot(table(df$hsc_b))
barplot(table(df$hsc_s))
barplot(table(df$degree_t))
barplot(table(df$workex))
barplot(table(df$specialisation))
barplot(table(df$status))

############################## Scatter Plot ##############################

pairs(~ssc_p+hsc_p+degree_p+etest_p+mba_p,data=df)

summary(df)

############################## OBSERVATIONS ##############################

#SALARY HAS MISSING VALUES
#NO OUTLIER TREATMENT REQUIRED 
#DUMMY VARIABLE NEEDED

############################## Missing value Imputation ##############################

mn=mean(df$salary, na.rm = TRUE)
df$salary[is.na(df$salary)]<-mn
summary(df$salary)

############################## DUMMY variable creation ##############################

install.packages("dummies")
library(dummies)
df$status<-as.factor(df$status)

df<- dummy.data.frame(df)
df<-df[,-1] #Removing genderF
df<-df[,-4] #Removing ssc_bothers
df<-df[,-6] #Removing hsc_bothers
df<-df[,-11] #Removing degree_tothers
df<-df[,-12] #removing workexNo
df<-df[,-15] #removing specialisationMkt&HR
df<-df[,-16] #statusNotPlaced
df<-df[,-8] #Depending on Correlation matrix

cor(df)
round(cor(df),2)
############################## Test-Train Split ##############################

install.packages("caTools")
library(caTools)
split<-sample.split(df,SplitRatio = 0.8)
train_set=subset(df,split==TRUE)             #TRAINING SET
test_set=subset(df,split==FALSE)             #TESTING SET

############################## TRAINING MODEL ##############################

##### LOGISTIC REGRESSION 

install.packages("glmnet")
library(glmnet)
logistic_model = glm(statusPlaced~.,data=train_set,family = "binomial")
summary(logistic_model)
log_test_prob = predict(logistic_model,test_set,type="response")
log_test_prob
log_pred=rep("NO",50)
log_pred[log_test_prob>0.5]<-"YES"

table(log_pred, test_set$statusPlaced)
log_accu = (8+28)/(8+28+12+2)
log_accu

##### LDA ANALYSIS

install.packages("MASS")
library(MASS)
lda_model=lda(statusPlaced~.,data=train_set)
lda_model
lda.pred= predict(lda_model,test_set)
lda.class= lda.pred$class
table(lda.class,test_set$statusPlaced)
lda_accu =(9+29)/(9+29+1+11)
lda_accu

##### KNN TEST 

install.packages("class")
library(class)
train_x = train_set[,-16]
train_y=train_set$statusPlaced

test_x=test_set[,-16]
test_y=test_set$statusPlaced

train_x_s=scale(train_x)
test_x_s= scale(test_x)

set.seed(0)
knn.pred=knn(train_x_s,test_x_s,train_y,k=3)
table(knn.pred,test_y)
knn_accu=(6+28)/(12+3+6+28)
knn_accu

############################ LDA ANALYSIS PERFORMS WELL ##############################

############################## DECISION TREE ##############################

install.packages("rpart")
install.pacakages("rpart.plot")
library(rpart)
library(rpart.plot)
tree<-rpart(statusPlaced~.,data=train_set,method='class',control=rpart.control(maxdepth = 3))
rpart.plot(tree,digits=-3)
test_pred=predict(tree,test_set,type="class")
table(test_pred,test_set$statusPlaced)
tree_accu=(11+29)/(11+29+2+7)
tree_accu

