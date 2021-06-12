library("randomForest")
library("corrgram")
library("ggplot2")
library("Hmisc") #glm logistic regression function
library("DescTools") #Pseudo R squared

#Logit training data in factor and numeric
retailtrain <- read.csv("relaytrain_21_2HW2JG20191102.csv")
retaillogit <- retailtrain[,2:16]
retaillogit <- na.omit(retaillogit)
retaillogit$retained <- factor(retaillogit$retained)
retaillogit$paperless <- as.factor(retaillogit$paperless)
retaillogit$refill <- as.factor(retaillogit$refill)
retaillogit$doorstep <- as.factor(retaillogit$doorstep)
#retaillogit$esent <- scale(retaillogit$esent)

#Logit test data in factor and numeric
retailtest <- read.csv("relaytest_21_2HW3JTF20191107.csv")
retaillogittest <- retailtest[,2:15]
retaillogittest <- na.omit(retaillogittest)
retaillogittest$retained <- factor(retaillogittest$retained)
retaillogittest$paperless <- as.factor(retaillogittest$paperless)
retaillogittest$refill <- as.factor(retaillogittest$refill)
retaillogittest$doorstep <- as.factor(retaillogittest$doorstep)
#retaillogittest$esent <- scale(retaillogittest$esent)
retaillogittest$Days.between.creation.and.first.order <- as.numeric(retaillogittest$Days.between.creation.and.first.order)

#Correlation data in numeric
retailnumeric <- retailtrain[,2:27]
retailnumeric <- retailnumeric[,-12:-15]
retailnumeric <- na.omit(retailnumeric)

res <- cor(retailnumeric)
resround <- round(res, 2)
resround

res2 <- rcorr(as.matrix(retailnumeric),type=c("spearman"))
res2

# Extract the correlation coefficients
df.resR=data.frame(res2$r)

# Extract p-values
df.resP=data.frame(res2$P)
df.resP

write.csv(df.resR, file = "retail-R.csv")

# First Correlogram Example

#corrgram(retailnumeric, order=TRUE, lower.panel=panel.shade,
#         upper.panel=panel.pie, text.panel=panel.txt,
#         main="RetailRelay")

# Logistic regression

mylogit <- glm(retained ~ esent + Days.between.creation.and.first.order + Days.since.last.order + eopenrate + avgorder + ordfreq + paperless + refill + doorstep + favday + city,family=binomial(logit),  data = retaillogit)
summary(mylogit)

mylogit2 <- glm(retained ~ esent + Days.between.creation.and.first.order + eopenrate + avgorder + paperless + refill + doorstep,family=binomial(logit),  data = retaillogit)
summary(mylogit2)

#Calculate the R-squared using CoxSnell
PseudoR2(mylogit2, c("CoxSnell"))

#Predict for mylogit2
predLogit2 <- predict(mylogit2,type = 'response')

#Predict on test data set
mylogittest <- predict(mylogit2,newdata=subset(retaillogittest,select=c(2,3,4,5,6,7,8,9,10,11)),type='response')

#Confusion matrix
table(retaillogittest$retained, mylogittest > .5)
ctable <- as.table(matrix(c(4197, 836, 581, 18925), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predLogit2, retaillogit$retained)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#plot glm
ggplot(retaillogit, aes(x=esent, y=retained)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

# Random Forest
retailnumericRF <- retailnumeric[-c(3,6,8,12:22)]
retailnumericRF$retained <- as.factor(retailnumericRF$retained)
randomForest(retailnumericRF[,-1],retailnumericRF[,1])
importance(randomForest(retailnumericRF[,-1],retailnumericRF[,1]))

#rr_rf = randomForest(retained~., data=retailnumeric, ntree=100, proximity=T)

#RF test data in numeric
retailnumericRFtest <- retailtest[c(2,3,5,6,8,10,11,12)]
retailnumericRFtest <- na.omit(retailnumericRFtest)
retailnumericRFtest$retained <- as.factor(retailnumericRFtest$retained)
retailnumericRFtest$Days.between.creation.and.first.order <- as.numeric(retailnumericRFtest$Days.between.creation.and.first.order)
randomForest(retailnumericRFtest[,-1],retailnumericRFtest[,1])
importance(randomForest(retailnumericRFtest[,-1],retailnumericRFtest[,1]))

