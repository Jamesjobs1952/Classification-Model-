options(repos = c(CRAN = "https://cloud.r-project.org"))
remove(list=ls())
rexnord <-read.csv("Rexnord data.csv", header = TRUE)
View(rexnord)
#Logistic Regression
install.packages("fastDummies")
library(fastDummies)
rexnord_dummyoutcome<-fastDummies::dummy_cols(rexnord[12])
rexnord$Outcome=rexnord_dummyoutcome$Outcome_Won
#Convert Dummy Variables 
str(rexnord)
rexnord_dummyinq<-fastDummies::dummy_cols(rexnord[2])
rexnord$InqOrProj=rexnord_dummyinq$InqOrProj_INQUIRY

rexnord_dummyprojtype<-fastDummies::dummy_cols(rexnord[3])
rexnord$ProjType=rexnord_dummyprojtype$`ProjType_Eng/Eng`

rexnord_dummyreplace<-fastDummies::dummy_cols(rexnord[7])
rexnord$Replacement=rexnord_dummyreplace$Replacement_Yes

rexnord <- rexnord[rexnord$Prod != "Ocelot", ]
rexnord <- rexnord[rexnord$Prod != "Zebra", ]

rexnord$Prod<-as.factor(rexnord$Prod)
prod_num<-as.numeric(rexnord$Prod)
print(prod_num)
#Factorize the data
rexnord$Type<-as.factor(rexnord$Type)
type_num<-as.numeric(rexnord$Type)
print(type_num)
rexnord <- rexnord[rexnord$Type != "Oil Field Distributor", ]

rexnord$Ind<-as.factor(rexnord$Ind)
ind_num<-as.numeric(rexnord$Ind)
print(ind_num)

rexnord$IndSub<-as.factor(rexnord$IndSub)
indsub_num<-as.numeric(rexnord$IndSub)
print(indsub_num)

rexnord$Date<-months(rexnord$Date)
rexnord$Date<-as.factor(rexnord$Date)
date_num<-as.numeric(rexnord$Date)
print(date_num)
str(rexnord)

set.seed(1234)

#Split the Data 
trainrex<-0.6
validrex<-0.2
testrex<-0.2

sampleSizeTraining <- floor(trainrex*nrow(rexnord))
sampleSizeValidation <- floor(validrex*nrow(rexnord))
sampleSizeTest <- nrow(rexnord)-sampleSizeValidation-sampleSizeTraining

indicesTraining <- sort(sample(seq_len(nrow(rexnord)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(rexnord)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

train_rexnord <- rexnord[indicesTraining, ]
valid_rexnord <- rexnord[indicesValidation, ]
test_rexnord <- rexnord[indicesTest, ]


#Run Logistic Regression
LRfit<-glm(formula=Outcome~Prod+InqOrProj+ProjType+Type+Ind+IndSub+Replacement+NetValue+Date+LeadTime+CustNum,data=train_rexnord,family=binomial())
  
#Find Best Fit                               
install.packages("MASS")
library(MASS)

stepAIC(LRfit, direction="both")

good_rexnord<-glm(formula = Outcome ~ Prod + Type + Replacement + NetValue + Date, family = binomial(), data = test_rexnord)

#Generate Predictions
valid_rexnord$good_rexnord<-ifelse(predict(good_rexnord,
                                           valid_rexnord, type = "response")>0.25,1,0)
#Create Confusion Matrix
pred_good_valid<-valid_rexnord$good_rexnord
actual_valid<-valid_rexnord$Outcome
confusion_good  <- table(pred_good_valid, actual_valid)
confusion_good
#Overall Accuracy 
overall_accuracy_good <- 1 - (confusion_good[1,2]+confusion_good[2,1])/sum(confusion_good)
overall_accuracy_good

#Specificity
sensitivity_good <- confusion_good[2,2]/sum(confusion_good[,2])
specificity_good <- confusion_good[1,1]/sum(confusion_good[,1])
sensitivity_good
specificity_good

LRfit_final<-good_rexnord
good_rexnord

summary(LRfit_final)
str(LRfit_final)

############################
remove(list=ls())
library(readxl)
rexnord <- read_excel("C:/Users/James/Downloads/Rexnord data.xlsx")
View(rexnord)

#K Nearest Neighbor
#separate the x variables from y variables
product_raw <- rexnord[,1:11]
prodcut_outcome <- rexnord$Outcome

#convert the categorical into dummies
install.packages("fastDummies")
library(fastDummies)

product_complete <- fastDummies::dummy_cols(product_raw)

productx <- product_complete[,8:118]

#keeping the month only for date information 
productx$month <- format(as.Date(productx$Date, format = "%d/%m/%Y"), "%m")

#Keep the useful information for the final list of the x variables 
product_final <- productx[ , c(1, 3, 5:112)]

#Scale the x variables 
list_columns <- sapply(product_final, is.list)
print(list_columns)

product_final[list_columns] <- lapply(product_final[list_columns], function(product_final)as.numeric(unlist(product_final)))
product_final <- lapply(product_final,as.numeric)

Final_Product <- data.frame(product_final)

scaled_product <- scale(Final_Product)

View(scaled_product)
#set seed for the random number 
set.seed(1234)
#setting up the train set, test set and validation set 
trainfract<-0.6
validfract<-0.2
testfract<-0.2

#setting the classification of different types
sampleSizeTraining <- floor(trainfract*nrow(scaled_product))
sampleSizeValidation <- floor(validfract*nrow(scaled_product))
sampleSizeTest <- nrow(scaled_product)-sampleSizeValidation-sampleSizeTraining

indicesTraining <- sort(sample(seq_len(nrow(scaled_product)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(scaled_product)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

scaled_train_bookx <- scaled_product[indicesTraining, ]
scaled_valid_bookx <- scaled_product[indicesValidation, ]
scaled_test_bookx <- scaled_product[indicesTest, ]

train_class<-prodcut_outcome[indicesTraining]
valid_class<-prodcut_outcome[indicesValidation]
test_class<-prodcut_outcome[indicesTest]

install.packages("class")
library(class)

#building the knn model 
knn_predict <- knn(scaled_train_bookx,scaled_valid_bookx,train_class,k=6)
knnconfusion  <- table(knn_predict, valid_class)
knnconfusion
#model accuracy
overall_accuracy <-(knnconfusion[1,1]+knnconfusion[2,2])/sum(knnconfusion)
overall_accuracy
#model sensitivity 
sensitivity <- knnconfusion[2,2]/sum(knnconfusion[,2])
sensitivity
#model specificty 
specificity <- knnconfusion[1,1]/sum(knnconfusion[,1])
specificity


#Association Rules
remove(list=ls())
library(readxl)
rexnord <- read_excel("C:/Users/James/Downloads/Rexnord data.xlsx")
View(rexnord)
#categorize net value 
str(rexnord)
rexnord$NetVal[rexnord$NetValue<2000]<-"verylow"
rexnord$NetVal[rexnord$NetValue>=2000 & rexnord$NetValue<5000]<-"low"
rexnord$NetVal[rexnord$NetValue>=5000 & rexnord$NetValue<10000]<-"lowmed"
rexnord$NetVal[rexnord$NetValue>=10000 & rexnord$NetValue<50000]<-"med"
rexnord$NetVal[rexnord$NetValue>=50000 & rexnord$NetValue<100000]<-"high"
rexnord$NetVal[rexnord$NetValue>=100000]<-"veryhigh"
#categorize leadtime 
rexnord$LT[rexnord$LeadTime<=2.0]<-"Less 2"
rexnord$LT[rexnord$LeadTime>=3.0 & rexnord$LeadTime<=5.0]<-"3-5 weeks"
rexnord$LT[rexnord$LeadTime>=6.0 & rexnord$LeadTime<=8.0]<-"6-8 weeks"
rexnord$LT[rexnord$LeadTime>=9.0 & rexnord$LeadTime<=12.0]<-"9-12 weeks"
rexnord$LT[rexnord$LeadTime>=13.0 & rexnord$LeadTime<=16.0]<-"13-16 weeks"
rexnord$LT[rexnord$LeadTime>=17.0 & rexnord$LeadTime<=20.0]<-"17-20 weeks"
rexnord$LT[rexnord$LeadTime>=21.0 & rexnord$LeadTime<=25.0]<-"21-25 weeks"
rexnord$LT[rexnord$LeadTime>=26.0]<-"Over26"
        
View(rexnord)
bookfact<-rexnord[,c(1:7,12:14)]
View(bookfact)
        
bookfact$Prod<-as.factor(bookfact$Prod)
bookfact$InqOrProj<-as.factor(bookfact$InqOrProj)
bookfact$ProjType<-as.factor(bookfact$ProjType)
bookfact$Type<-as.factor(bookfact$Type)
bookfact$Ind<-as.factor(bookfact$Ind)
bookfact$IndSub<-as.factor(bookfact$IndSub)
bookfact$Replacement<-as.factor(bookfact$Replacement)
bookfact$Outcome<-as.factor(bookfact$Outcome)
bookfact$NetVal<-as.factor(bookfact$NetVal)
bookfact$LT<-as.factor(bookfact$LT)
        
str(bookfact)
        
install.packages("arules")
library(arules)
        
bookfact_df <- as.data.frame(bookfact)
book_trans <- as(bookfact_df, "transactions")
bookrules<-apriori(book_trans, parameter=list(supp=0.025, conf=0.1, minlen=2, maxlen=5))
summary(bookrules)
        
Won_rules <- subset(bookrules, subset = rhs %in% "Outcome=Won" & lift > 1)
summary(Won_rules)
Won_rules_df<-as(Won_rules, "data.frame")
Won_rules_df<-Won_rules_df[order(Won_rules_df$lift, Won_rules_df$support, decreasing=TRUE), ]
View(Won_rules_df)
        
bookrules<-apriori(book_trans, parameter=list(supp=0.075, conf=0.2, minlen=2, maxlen=5))
summary(bookrules)
        
Lost_rules <- subset(bookrules, subset = rhs %in% "Outcome=Lost" & lift > 1)
summary(Lost_rules)
Lost_rules_df<-as(Lost_rules, "data.frame")
Lost_rules_df<-Lost_rules_df[order(Lost_rules_df$lift, Lost_rules_df$support, decreasing=TRUE), ]
View(Lost_rules_df)
        
        
        
        

