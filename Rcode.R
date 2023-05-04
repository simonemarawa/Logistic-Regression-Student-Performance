######################### STUDENT PERFORMANCE #####################


######################## import data ##############################
student.por <- read.csv("~/Documents/Langara /Semester 2/Predictive Analytics - Qualitative Data (Qual)/Project/data/student-por.csv", 
                        sep=";", stringsAsFactors=T)
attach(student.por)
summary(student.por)
head(student.por)

#detach(student.por)

####################### missing values ############################
#number of missing values in data set : 0
sum(is.na(student.por))

#################### redefine target variable  ################
#G3: Pass (1): 12 >= , Fail (0) : <12
grade =ifelse(G3 >= 12, 1,0 ) #dummy variable

#add new target variable
student.por$grade = grade

################### convert variables to categorical ###########

#convert variables to categorical variable:
cols <- c("school", "address", "famsize",'Pstatus','Medu','Fedu','Mjob','Fjob',
          'reason','guardian','traveltime','studytime','failures','schoolsup','famsup',
          'paid','activities','nursery','higher','internet', 'romantic','famrel','freetime',
          'goout','Dalc','Walc','health','sex')
student.por[cols] <- lapply(student.por[cols], factor)

summary(student.por)

################### variable selection: Categorical variables ###############################

#categorical variables
categorical = student.por[c('grade',"school", "address", "famsize",'Pstatus','Medu','Fedu','Mjob','Fjob',
                            'reason','guardian','traveltime','studytime','failures','schoolsup','famsup',
                            'paid','activities','nursery','higher','internet', 'romantic','famrel','freetime',
                            'goout','Dalc','Walc','health', 'sex')]

#function for chi-squared test on a data frame
chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = chisq.test(x[,i],x[,j],correct=FALSE)$p.value
    }
  }
  return (m)
}
mat = chisqmatrix(categorical)
mat[1,]

################### variable selection: Numerical variables ###############################

library(ggplot2)
library(qqplotr)

#qq plot for absences
ggplot(data = student.por, mapping = aes(sample = absences)) +
  stat_qq_line(col = 2) +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = 'Normal Q-Q Plot for Absences')


#qq plot for age
ggplot(data = student.por, mapping = aes(sample = age)) +
  stat_qq_line(col = 2) +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = 'Normal Q-Q Plot for Age')

############### Kruskal-Wallis for numerical variables
# Absences
kruskal.test(grade ~ absences, data = student.por)

# Age
kruskal.test(grade ~ age, data = student.por)


################### multicollinearity ########################

#initial model to check multicollinearity
library(car)

#data frame with our initial variables only
df_initial_variables =  student.por[c("school", "address",'Medu','Fedu','Mjob','Fjob',
                                      'reason','guardian','traveltime','studytime','failures',
                                      'activities','higher','internet','goout','Dalc','Walc',
                                      'health', 'age', 'sex','grade')]

initial_model  = glm (grade~ school+ sex+ Medu+ Fedu+ Mjob+ reason+
                        address+ Fjob+ guardian+ traveltime+
                        studytime+failures+activities+ higher+internet+
                        goout+Dalc+Walc+health+age+absences, family = binomial, data= student.por)
vif(initial_model)


################### train and split data ####################
library(ggplot2)
library(lattice)
library(caret)
#install.packages("scales")

# use a seed to make the same split every time code is run
set.seed(123)
#split data 80% vs 20%
split = 0.80  

trainIndex = createDataPartition(student.por$grade, p=split, list=FALSE)

# Create training and test set
data_train = student.por[ trainIndex,]
data_test = student.por[-trainIndex,]

# Check the dimension of both training and test dataset
dim(data_train)
dim(data_test)


################### stepwsie backward selection using AIC ########################
library(MASS)

# model with our subset of variables (after removing highly correlated variables)
model_stepwise <- glm(grade ~ school+ sex+reason+ address+ Fjob+
                    guardian+traveltime+studytime+failures+
                    activities+higher+internet+goout+health+age+
                    absences ,family=binomial, data= data_train)

#stepwise backward elimination
stepAIC(model_stepwise)

#################  highest p-value elimination ##################

#check to see if stepwise varibales are all significant
model1 <- glm(grade ~ school + sex + address + guardian + traveltime + 
                    studytime + failures + activities + higher + internet + health + 
                    absences ,family=binomial, data= data_train)
summary(model1)

#new model with health variable removed
model2 <- glm(grade ~ school + sex + address + guardian + traveltime + 
                studytime + failures + activities + higher + internet + 
                absences ,family=binomial, data= data_train)
summary(model2)

#new model with internet variable removed
model3 <- glm(grade ~ school + sex + address + guardian + traveltime + 
                studytime + failures + activities + higher + 
                absences ,family=binomial, data= data_train)
summary(model3)


################### interactions ##################
library(interactions)

#study time and sex interaction
model_studytime_sex =  glm (grade ~ school + sex + address + guardian + traveltime + 
                              studytime + failures + activities + higher + absences+ 
                              studytime* sex ,family = binomial, data= data_train)

cat_plot(model_studytime_sex, pred = sex, modx = studytime, data= data_train, geom = 'line', interval = F,
         main.title = 'Interaction plot between Sex and Study time')

model_school_guardian =  glm (grade~school + sex + address + guardian + traveltime + 
                              studytime + failures + activities + higher + 
                              absences
                            + school* guardian ,family = binomial, data= data_train)
cat_plot(model_school_guardian, pred = school, modx = guardian, data= data_train, geom = 'line', interval = F,
         main.title = 'Interaction plot between School and Guardian')

model_sex_guardian =  glm (grade~school + sex + address + guardian + traveltime + 
                              studytime + failures + activities + higher + 
                              absences
                            + sex* guardian ,family = binomial, data= data_train)
cat_plot(model_sex_guardian, pred = guardian, modx = sex, data= data_train, geom = 'line', interval = F,
         main.title = 'Interaction plot between Sex and Guardian')


model_studytime_activities =  glm (grade~school + sex + address + guardian + traveltime + 
                              studytime + failures + activities + higher + 
                              absences
                            + activities* studytime ,family = binomial, data= data_train)
cat_plot(model_studytime_activities, pred = activities, modx = studytime, data= data_train, geom = 'line', interval = F,
         main.title = 'Interaction plot between Activities and Studytime')
###########################  model comparison: interactions       ###############################
# none of the models have a significant interaction 

summary(model_studytime_sex)
summary(model_school_guardian)
summary(model_sex_guardian)
summary(model_studytime_activities)

#likelihood ratio tests 
lrtest(model, model_studytime_sex) 

lrtest(model, model_school_guardian) 

lrtest(model, model_sex_guardian) 

lrtest(model, model_studytime_activities) 

################# likelihood ratio test: Model comparison #########
library(lmtest)
#install.packages('lmtest')
#model without interaction
model = glm(grade~school + sex + address + guardian + traveltime + 
              studytime + failures + activities + higher + 
              absences ,family=binomial, data= data_train)
summary(model)

#model with interaction
model_sex_guardian =  glm(grade~school + sex + address + guardian + traveltime + 
                             studytime + failures + activities + higher + 
                             absences
                           + sex* guardian ,family = binomial, data= data_train)
summary(model_sex_guardian)

lrtest(model, model_sex_guardian) 

################  predictive power of our model   ##################
library(caret)

#final model: without interaction
model = glm(grade~school + sex + address + guardian + traveltime + 
              studytime + failures + activities + higher + 
              absences ,family=binomial, data= data_train)
summary(model)

#sample proportion of 1's for grade variable
prop =  sum(data_train$grade)/nrow(data_train)

#do prediction using the model
probabilities = predict(model, newdata = data_test, type = "response") 
predicted_classes = ifelse(probabilities > prop, 1, 0)

#create a confusion matrix table with all the statistics
confusionMatrix(table(predicted_classes, data_test$grade), positive = "1")

################  predictive power of model with interaction ##################
library(caret)

#model with sex and guardian interaction
model_sex_guardian =  glm(grade~school + sex + address + guardian + traveltime + 
                            studytime + failures + activities + higher + 
                            absences
                          + sex* guardian ,family = binomial, data= data_train)

#sample proportion of 1's for grade variable
prop =  sum(data_train$grade)/nrow(data_train)

#do prediction using the model
probabilities = predict(model_sex_guardian, newdata = data_test, type = "response") 
predicted_classes = ifelse(probabilities > prop, 1, 0)

#create a confusion matrix table with all the statistics
confusionMatrix(table(predicted_classes, data_test$grade), positive = "1")

####### ROC curve ############
library(pROC)

#model with sex and guardian interaction
model_sex_guardian =  glm(grade~school + sex + address + guardian + traveltime + 
                            studytime + failures + activities + higher + 
                            absences
                          + sex* guardian ,family = binomial, data= data_train)

# our model: no interaction
model = glm(grade~school + sex + address + guardian + traveltime + 
              studytime + failures + activities + higher + 
              absences ,family=binomial, data= data_train)

#roc curve for model without interaction
rocplot_no_interaction = roc(grade ~ fitted(model), data=data_train)

#roc curve for model with interaction
rocplot_with_interaction <- roc(grade ~ fitted(model_sex_guardian), data=data_train) 

# Specficity on x axis if legacy.axes=F  
plot.roc(rocplot_no_interaction, legacy.axes=TRUE, col = 'blue', main = 'Two ROC curves')
plot.roc(rocplot_with_interaction, add=TRUE, col="red")
legend(0.8,0.2, c("Model with interaction", "Model without interaction"), lty=1, 
       col = c("red", "blue"), bty="n", inset=c(0,-0.15))


# auc = area under ROC curve = concordance index Model3
#model with no interaction
auc(rocplot_no_interaction) 

#model with interaction
auc(rocplot_with_interaction) 

########### Hosmer-Lemershow Test###########

library(ResourceSelection)

#our model :without interactions
model = glm(grade~school + sex + address + guardian + traveltime + 
              studytime + failures + activities + higher + 
              absences ,family=binomial, data= data_train)

hoslem.test(model$y, fitted(model), g = 11)


