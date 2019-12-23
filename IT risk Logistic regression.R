# Project on IT Risk

set.seed(444)

path="C:/Users/Yash Patil/Desktop/DATA ANALYSIS/R/PROJECT 1/networktrain.csv"
NIV=read.csv(path,header = TRUE)
#View(NIV)

#head(NIV)
#tail(NIV)

str(NIV)
nrow(NIV)
ncol(NIV)
col=length(NIV)
colnames(NIV)

##############################################

table(NIV$class)
NIV$class = ifelse(NIV$class=="normal",0,1)
table(NIV$class)
NIV$class=as.factor(NIV$class)

NIV$service=as.numeric(NIV$service)

NIV$service[NIV$service%in%seq(1,11)]=1
NIV$service[NIV$service%in%seq(12,23)]=2
NIV$service[NIV$service%in%seq(24,35)]=3
NIV$service[NIV$service%in%seq(36,47)]=4
NIV$service[NIV$service%in%seq(48,59)]=5
NIV$service[NIV$service%in%seq(60,66)]=6

NIV$service = as.factor(NIV$service)
str(NIV)

##############################################

# Check for the proportion
for (n in seq(1:(ncol(NIV)-1)))
{
  print(colnames(NIV[n]))
  x=100*(prop.table(table(NIV[n]==0)))
  print(x)
}

# delete columns whose prop. of 0 is greater than 80%
NIV$duration=NULL
NIV$land=NULL
NIV$wrong_fragment=NULL
NIV$urgent=NULL
NIV$hot=NULL
NIV$num_failed_logins=NULL
NIV$num_compromised=NULL
NIV$root_shell=NULL
NIV$su_attempted=NULL
NIV$num_root=NULL
NIV$num_file_creations=NULL
NIV$num_shells=NULL
NIV$num_access_files=NULL
NIV$num_outbound_cmds=NULL
NIV$is_host_login=NULL
NIV$is_guest_login=NULL
NIV$rerror_rate=NULL
NIV$srv_rerror_rate=NULL
NIV$dst_host_rerror_rate=NULL
NIV$dst_host_srv_rerror_rate=NULL

ncol(NIV)


# randomly shuffle the dataset and sample
grp = runif(nrow(NIV))
NIV = NIV[order(grp),]

# identify Numeric, Character and Factor variables from the dataset
# chars = names(NIV)[sapply(NIV,is.character)]

facts = names(NIV)[sapply(NIV,is.factor)]
nums = names(NIV)[sapply(NIV,is.numeric)]
print(facts)
print(nums)

num_data = NIV[nums]
fact_data = NIV[facts]
nrow(num_data)
nrow(fact_data)

# EDA on numeric data
# 1) 0, outlier and missing value check
col_name = colnames(num_data) [apply(num_data, 2, function(n) any(n==0))]
if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  print("No Zeroes")


# check for Nulls
col_name = colnames(NIV) [apply(NIV, 2, function(n) any(is.na(n)))]
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

# check for Blanks
col_name = colnames(NIV) [apply(NIV, 2, function(n) any(n == ""))]
if(length(col_name) > 0)
{
  print("Blanks present")
  print(col_name)
} else print("No Blanks")


# 2) multicollinearity
# check for multicollinearity (correlation among the x-variables)
library(corrplot)
ncol(num_data)
cor = cor(num_data[1:18]) # gives values
corrplot(cor,method="number",type="lower")


#build the model
basemodel = glm(class~., data=NIV, binomial(link="logit"))
summary(basemodel)
str(NIV)

###########################################################################################
##################################################################################


file="C:/Users/Yash Patil/Desktop/DATA ANALYSIS/R/PROJECT 1/networkvalidate.csv"
validate=read.csv(file,header = TRUE)

#View(validate)

str(validate)
nrow(validate)
ncol(validate)
col=length(validate)
colnames(validate)

table(validate$class)
validate$class = ifelse(validate$class=="normal",0,1)
table(validate$class)

validate$class=as.factor(validate$class)

############################################

validate$service=as.numeric(validate$service)

validate$service[validate$service%in%seq(1,11)]=1
validate$service[validate$service%in%seq(12,23)]=2
validate$service[validate$service%in%seq(24,35)]=3
validate$service[validate$service%in%seq(36,47)]=4
validate$service[validate$service%in%seq(48,59)]=5
validate$service[validate$service%in%seq(60,64)]=6


validate$service = as.factor(validate$service)
str(validate)

############################################

# Check for the proportion
for (n in seq(1:(ncol(validate)-1)))
{
  print(colnames(validate[n]))
  x=100*(prop.table(table(validate[n]==0)))
  print(x)
}

# delete columns whose prop. of 0 is greater than 80%
validate$duration=NULL
validate$land=NULL
validate$wrong_fragment=NULL
validate$urgent=NULL
validate$hot=NULL
validate$num_failed_logins=NULL
validate$num_compromised=NULL
validate$root_shell=NULL
validate$su_attempted=NULL
validate$num_root=NULL
validate$num_file_creations=NULL
validate$num_shells=NULL
validate$num_access_files=NULL
validate$num_outbound_cmds=NULL
validate$is_host_login=NULL
validate$is_guest_login=NULL
validate$rerror_rate=NULL
validate$srv_rerror_rate=NULL
validate$dst_host_rerror_rate=NULL
validate$dst_host_srv_rerror_rate=NULL


ncol(validate)

# randomly shuffle the dataset and sample
grp1 = runif(nrow(validate))
validate = validate[order(grp1),]

# identify Numeric, Character and Factor variables from the dataset
# chars = names(NIV)[sapply(NIV,is.character)]

facts1 = names(validate)[sapply(validate,is.factor)]
nums1 = names(validate)[sapply(validate,is.numeric)]
print(facts1)
print(nums1)

num_data1 = validate[nums1]
fact_data1 = validate[facts1]
nrow(num_data1)
nrow(fact_data1)

# EDA on numeric data
# 1) 0, outlier and missing value check
col_name = colnames(num_data1) [apply(num_data1, 2, function(n) any(n==0))]
if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  print("No Zeroes")


# predict the Binary outcome for attrition_value
# type = "response" gives probabilities

#unique(NIV$service)
#unique(validate$service)

basepredictions = predict(basemodel, validate , type="response")
print(basepredictions[1:10])

# check the count to convert probabilites to 0/1
table(validate$class)

cutpoint = 0.4
predictions = ifelse(basepredictions <=0.4, 0,1)
print(predictions[1:10])
 
# confusion matrix
# 1 --> positive class
#install.packages("e1071")
library(caret)
table(validate$class)
confusionMatrix(as.factor(validate$class), as.factor(predictions), positive="1")

#install.packages("ROCR")
library(ROCR)
preds = prediction(basepredictions,validate$class)


# identifying the best cut-off by plotting the ROC curve
# 1) evaluations
# ------------------------------------
evals = performance(preds, "acc")
evals
plot(evals)
abline(h=0.85, v=0.65) # play with these values
max_yval = which.max(slot(evals, "y.values")[[1]])
max_acc = slot(evals, "y.values")[[1]][max_yval]
max_cutoff = slot(evals, "x.values")[[1]][max_yval]
print(paste("Best accuracy = ", round(max_acc,4), 
            " Best Cutoff = ", round(max_cutoff,4)))

predictions_max = ifelse(basepredictions <= max_cutoff, 0,1)
tab = table(predicted = predictions_max,actual = validate$class)
confusionMatrix(as.factor(predictions_max),
                validate$class, positive="1")

# plot ROC
perf = performance(preds, "tpr", "fpr")
plot(perf, colorize=T, main="ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0, b=1)

# area under the curve (AUC)
auc = performance(preds, "auc")
round(unlist(slot(auc, "y.values")),3)


# feature selection technique
step(basemodel)

# exercise (change cutoff to 0.749 & compare confusion matrix with 0.5 cutoff matrix)

# predict the Binary outcome for attrition_value
# type = "response" gives probabilities
basepredictions = predict(basemodel, validate, type="response")
print(basepredictions[1:20])

# check the count to convert probabilites to 0/1
table(validate$class)

cutpoint = 0.05
predictions = ifelse(basepredictions <=0.05, 0,1)
print(predictions[1:10])

library(caret)
table(validate$class)
confusionMatrix(as.factor(validate$class), as.factor(predictions), positive="1")

#################################
file1 = "C:/Users/Yash Patil/Desktop/DATA ANALYSIS/R/PROJECT 1/networktest.csv"
test = read.csv(file1,header = TRUE)

#View(validate)

str(test)
nrow(test)
ncol(test)
col=length(test)
colnames(test)


############################################

str(test$service)
unique(test$service)
test$service=as.numeric(test$service)

test$service[test$service%in%seq(1,11)]=1
test$service[test$service%in%seq(12,23)]=2
test$service[test$service%in%seq(24,35)]=3
test$service[test$service%in%seq(36,47)]=4
test$service[test$service%in%seq(48,59)]=5
test$service[test$service%in%seq(60,64)]=6

test$service = as.factor(test$service)
str(test)

############################################

# Check for the proportion
for (n in seq(1:(ncol(test)-1)))
{
  print(colnames(test[n]))
  x=100*(prop.table(table(test[n]==0)))
  print(x)
}

# delete columns whose prop. of 0 is greater than 80%
test$duration=NULL
test$land=NULL
test$wrong_fragment=NULL
test$urgent=NULL
test$hot=NULL
test$num_failed_logins=NULL
test$num_compromised=NULL
test$root_shell=NULL
test$su_attempted=NULL
test$num_root=NULL
test$num_file_creations=NULL
test$num_shells=NULL
test$num_access_files=NULL
test$num_outbound_cmds=NULL
test$is_host_login=NULL
test$is_guest_login=NULL
test$rerror_rate=NULL
test$srv_rerror_rate=NULL
test$dst_host_rerror_rate=NULL
test$dst_host_srv_rerror_rate=NULL


ncol(test)

# identify Numeric, Character and Factor variables from the dataset
# chars = names(NIV)[sapply(NIV,is.character)]

facts2 = names(test)[sapply(test,is.factor)]
nums2 = names(test)[sapply(test,is.numeric)]
print(facts2)
print(nums2)

num_data2 = test[nums2]
fact_data2 = test[facts2]
nrow(num_data2)
nrow(fact_data2)

# EDA on numeric data
# 1) 0, outlier and missing value check
col_name2 = colnames(num_data2) [apply(num_data2, 2, function(n) any(n==0))]
if(length(col_name2) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name2)
} else
  print("No Zeroes")


# predict the Binary outcome for attrition_value
# type = "response" gives probabilities

#unique(NIV$service)
#unique(validate$service)

preds1 = predict(basemodel, test , type="response")
print(preds1)













