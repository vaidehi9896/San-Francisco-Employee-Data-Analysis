#R CODE- SF Emp data analysis

getwd()

setwd("C://Users//vaidehi//Downloads//527-Data Analytics")
data=read.csv("analytics_project.csv", header = T)
dim(data)
# sampling
set.seed(5)
sample_size=150000
sdata = sample(1:nrow(data),sample_size,replace=F)
subdata=data[sdata,]
names(subdata)
subdata=data
dim(subdata)
write.csv(subdata,"C://Users//vaidehi//Downloads//527-Data Analytics//samp.csv", row.names = FALSE)
install.packages("janitor")
library(janitor)
subdata=clean_names(subdata)
names(subdata)

# removal of extra columns

install.packages("dplyr")
library(dplyr)

install.packages("plyr")
library(plyr)
subdata=select(subdata,-c(organization_group_code))
subdata=select(subdata,-c(department))
subdata=select(subdata,-c(union_code))
subdata=select(subdata,-c(job_family_code))
subdata=select(subdata,-c(job_code))
subdata=select(subdata,-c(employee_identifier))
names(subdata)

# negative values numeric columns checking

nrow(subdata[subdata$salaries<0,])
subdata$salaries[subdata$salaries < 0]=mean(subdata$salaries)
nrow(subdata[subdata$overtime <0,])
subdata$overtime[subdata$overtime < 0]=mean(subdata$overtime)
nrow(subdata[subdata$other_salaries <0,])
subdata$other_salaries[subdata$other_salaries < 0]=mean(subdata$other_salaries)
nrow(subdata[subdata$total_salary <0,])
subdata$total_salary[subdata$total_salary < 0]=mean(subdata$total_salary)
nrow(subdata[subdata$retirement <0,])
subdata$retirement[subdata$retirement < 0]=mean(subdata$retirement)
nrow(subdata[subdata$health_and_dental <0,])
subdata$health_and_dental[subdata$health_and_dental < 0]=mean(subdata$health_and_dental)
nrow(subdata[subdata$other_benefits <0,])
subdata$other_benefits[subdata$other_benefits < 0]=mean(subdata$other_benefits)
nrow(subdata[subdata$total_benefits <0,])
subdata$total_benefits[subdata$total_benefits < 0]=mean(subdata$total_benefits)
nrow(subdata[subdata$total_compensation <0,])
subdata$total_compensation[subdata$total_compensation < 0]=mean(subdata$total_compensation)

#check and replace missing na
sum(subdata$year_type == "")
sum(is.na(subdata$year_type))

sum(subdata$year == "")
sum(is.na(subdata$year))
sum(subdata$year == "__NOT_APPLICABLE__")

sum(is.na(subdata$organization_group))
sum(subdata$organization_group == "")
sum(subdata$organization_group == "__NOT_APPLICABLE__")

sum(is.na(subdata$department_code))
sum(subdata$department_code == "")
sum(subdata$department_code == "__NOT_APPLICABLE__")

sum(is.na(subdata$union))
sum(subdata$union == "")
sum(subdata$union == "__NOT_APPLICABLE__")

sum(is.na(subdata$job_family))
sum(subdata$job_family == "")
sum(subdata$job_family == "__NOT_APPLICABLE__")

sum(is.na(subdata$job))
sum(subdata$job == "")
sum(subdata$job == "__NOT_APPLICABLE__")

sum(is.na(subdata$salaries))
sum(subdata$salaries == "")
sum(is.na(subdata$overtime))
sum(subdata$overtime == "")
sum(is.na(subdata$other_salaries))
sum(subdata$other_salaries == "")
sum(is.na(subdata$total_salary))
sum(subdata$total_salary == "")
sum(is.na(subdata$retirement))
sum(subdata$retirement == "")
sum(is.na(subdata$health_and_dental))
sum(subdata$health_and_dental == "")
sum(is.na(subdata$other_benefits))
sum(subdata$other_benefits == "")
sum(is.na(subdata$total_benefits))
sum(subdata$total_benefits == "")
sum(is.na(subdata$total_compensation))
sum(subdata$total_compensation == "")

# check crf
opt=count(subdata$department_code)
crf=table(subdata$department_code)/nrow(subdata)

labels=opt$x
pie(crf,labels)

subdata$department_code[subdata$department_code == ""] = "__NOT_APPLICABLE__"

sum(subdata$department_code == "")
sum(subdata$department_code == "__NOT_APPLICABLE__")

opt=count(subdata$union)
crf=table(subdata$union)/nrow(subdata)

labels=opt$x
pie(crf,labels)

sum(is.na(subdata$union))
sum(subdata$union == "")
subdata$union[subdata$union == ""] = "Employees"

cleandata=subdata
names(subdata)


# ANOVA
anov=lm(subdata$salaries~subdata$job)
summary(anov)
anova(anov)
names(subdata&job)

anov=lm(subdata$salaries~subdata$organization_group)
summary(anov)
anova(anov)

# removal of col

# creation of dummies
install.packages("dummies")
library(dummies) 

View(subdata)
head(subdata)

names(subdata)
subdata=dummy.data.frame(subdata,names="year")
subdata=select(subdata,-c(year2013))

subdata=dummy.data.frame(subdata,names="year_type")

subdata=clean_names(subdata)

subdata=dummy.data.frame(subdata,names="organization_group")

subdata=dummy.data.frame(subdata,names="department_code")

subdata=dummy.data.frame(subdata,names="union")

subdata=dummy.data.frame(subdata,names="job_family")

subdata=dummy.data.frame(subdata,names="job")

names(subdata)

dim(subdata)

dim(dumdata)
dumdata=subdata
compdata=dumdata

# check cor for numeric variables
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")

# for transformation on var overtime
t=subdata$overtime*subdata$overtime
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$overtime)
t=1/(subdata$overtime)
subdata=select(subdata,-c(overtime))
cor(subdata$salaries,subdata$other_salaries, method = "pearson")

# for transformation on var other_salaries
t=subdata$other_salaries*subdata$other_salaries
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$other_salaries)
t=1/(subdata$other_salaries)
subdata=select(subdata,-c(other_salaries))

cor(subdata$salaries,subdata$retirement, method = "pearson")
cor(subdata$salaries,subdata$health_and_dental, method = "pearson")

# for transformation on var health_and_dental
t=subdata$health_and_dental*subdata$health_and_dental
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$health_and_dental)
t=1/(subdata$health_and_dental)
subdata=select(subdata,-c(health_and_dental))

cor(subdata$salaries,subdata$other_benefits, method = "pearson")
cor(subdata$salaries,subdata$total_compensation, method = "pearson")
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$total_benefits, method = "pearson")

#correlation for total_compensation
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")

# for hold out evaluation
subdata=subdata[sample(nrow(subdata)),]

select.data = sample(1:nrow(subdata),0.7*nrow(subdata))
train.data=subdata[select.data,]
test.data=subdata[-select.data,]

dim(train.data)
dim(test.data)
names(train.data)

#salaries
m1=lm(train.data$salaries ~ .,data=train.data)
summary(m1)

m2=step(m1, direction = "backward", trace = T)

summary(m2)

res=rstandard(m2)
plot (fitted(m2), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test.data)

y1=predict.glm(m2,test.data)
y=test.data[,165]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_1

install.packages("car")
library(car)
vif(m2)

cor(train.data$total_compensation,train.data$total_benefits, method="pearson") 

#remove total_benefits
cor(train.data$total_compensation,train.data$other_benefits, method="pearson")
cor(train.data$total_compensation,train.data$retirement, method="pearson") 

#remove retirement
cor(train.data$total_compensation,train.data$total_salary, method="pearson") 

#remove total_salary
cor(train.data$total_compensation,train.data$job_power_and_fire_executive, method="pearson")
cor(train.data$total_compensation,train.data$job_police_and_investigation, method="pearson")
cor(train.data$total_compensation,train.data$job_medical_health_and_diagnostic_expert, method="pearson")
cor(train.data$total_compensation,train.data$job_family_worker, method="pearson")
cor(train.data$total_compensation,train.data$job_family_police_services, method="pearson")
cor(train.data$total_compensation,train.data$job_family_nursing, method="pearson")
cor(train.data$total_compensation,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$total_compensation,train.data$union_municipals, method="pearson")
cor(train.data$total_compensation,train.data$union_firefighters, method="pearson")
cor(train.data$total_compensation,train.data$department_code_mta, method="pearson")
cor(train.data$total_compensation,train.data$department_code_fir, method="pearson")
cor(train.data$total_compensation,train.data$department_code_dph, method="pearson")
cor(train.data$total_compensation,train.data$organization_group_public_works_transportation_commerce, method="pearson")
cor(train.data$total_compensation,train.data$organization_group_public_protection, method="pearson")
cor(train.data$total_compensation,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$total_compensation,train.data$organization_group_community_health, method="pearson")

cor(train.data$other_benefits,train.data$total_benefits, method="pearson")
cor(train.data$other_benefits,train.data$retirement, method="pearson")     
cor(train.data$other_benefits,train.data$total_salary, method="pearson")
cor(train.data$other_benefits,train.data$job_power_and_fire_executive, method="pearson")
cor(train.data$other_benefits,train.data$job_police_and_investigation, method="pearson")
cor(train.data$other_benefits,train.data$job_medical_health_and_diagnostic_expert, method="pearson")
cor(train.data$other_benefits,train.data$job_family_worker, method="pearson")
cor(train.data$other_benefits,train.data$job_family_police_services, method="pearson")
cor(train.data$other_benefits,train.data$job_family_nursing, method="pearson")
cor(train.data$other_benefits,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$other_benefits,train.data$union_municipals, method="pearson")
cor(train.data$other_benefits,train.data$union_firefighters, method="pearson")
cor(train.data$other_benefits,train.data$department_code_mta, method="pearson")
cor(train.data$other_benefits,train.data$department_code_fir, method="pearson")
cor(train.data$other_benefits,train.data$department_code_dph, method="pearson")
cor(train.data$other_benefits,train.data$organization_group_public_works_transportation_commerce, method="pearson")
cor(train.data$other_benefits,train.data$organization_group_public_protection, method="pearson")
cor(train.data$other_benefits,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$other_benefits,train.data$organization_group_community_health, method="pearson")

cor(train.data$job_power_and_fire_executive,train.data$total_benefits, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$retirement, method="pearson")     
cor(train.data$job_power_and_fire_executive,train.data$total_salary, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_police_and_investigation, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_medical_health_and_diagnostic_expert, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_worker, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_police_services, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_nursing, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$union_municipals, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$union_firefighters, method="pearson") 

#remove union firefighters
cor(train.data$job_power_and_fire_executive,train.data$department_code_mta, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$department_code_fir, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$department_code_dph, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_public_works_transportation_commerce, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_public_protection, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_community_health, method="pearson")

cor(train.data$job_power_and_fire_executive,train.data$total_benefits, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$retirement, method="pearson")     
cor(train.data$job_power_and_fire_executive,train.data$total_salary, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_police_and_investigation, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_medical_health_and_diagnostic_expert, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_worker, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_police_services, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_nursing, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$union_municipals, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$union_firefighters, method="pearson") 

#remove union firefighters
cor(train.data$job_power_and_fire_executive,train.data$department_code_mta, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$department_code_fir, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$department_code_dph, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_public_works_transportation_commerce, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_public_protection, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$organization_group_community_health, method="pearson")


cor(train.data$organization_group_community_health,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$organization_group_public_protection,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$organization_group_public_works_transportation_commerce,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$department_code_dph,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$department_code_fir,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$union_firefighters,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$department_code_mta,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$organization_group_community_health,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$union_municipals,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$job_family_management_and_development_agency,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$job_family_nursing,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$job_family_worker,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$job_police_and_investigation,train.data$job_family_management_and_development_agency, method="pearson")
cor(train.data$job_power_and_fire_executive,train.data$total_salary, method="pearson")
cor(train.data$union_firefighters ,train.data$union_municipals, method="pearson")
cor(train.data$other_benefits ,train.data$total_benefits, method="pearson")
cor(train.data$total_compensation ,train.data$total_benefits, method="pearson")
cor(train.data$total_benefits ,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$total_benefits ,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$total_benefits ,train.data$organization_group_general_city_responsibilities, method="pearson")
cor(train.data$total_benefits ,train.data$organization_group_general_city_responsibilities, method="pearson")

# removal of var with corelation greater than 0.9 for salary backward
train_s.data=train.data
test_s.data=test.data

train_s.data=select(train_s.data,-c(retirement))
train_s.data=select(train_s.data,-c(total_salary))
train_s.data=select(train_s.data,-c(total_benefits))
train_s.data=select(train_s.data,-c(union_firefighters))

test_s.data=select(test_s.data,-c(retirement))
test_s.data=select(test_s.data,-c(total_salary))
test_s.data=select(test_s.data,-c(total_benefits))
test_s.data=select(test_s.data,-c(union_firefighters))

# influential points
options(max.print=999999999)
influence.measures(m2)
summary(influence.measures(m2))

# run model again after vif once
m5=lm(train_s.data$salaries ~ .,data=train_s.data)
summary(m5)

m6=step(m5, direction = "backward", trace = T)

summary(m6)

# 2nd time model in sal
R_MAX_NUM_DLLS=500
install.packages("ggplot2")
library(ggplot2)
res=rstandard(m6)
plot (fitted(m6), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test_s.data)
y1=predict.glm(m6,test_s.data)
y=test_s.data[,164]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test_s.data))
rmse_1

install.packages("car")
library(car)
vif(m4)
names(m6)
new=data.frame()


#Total compensation-Backward

#total_compensation
m4=lm(train.data$total_compensation ~ .,data=train.data)
summary(m4)

m3=step(m4, direction = "backward", trace = T)

summary(m3)

# residual analysis

res=rstandard(m3)
plot (fitted(m3), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test.data)
y1=predict.glm(m3,test.data)
y=test.data[,171]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_1

install.packages("car")
library(car)
vif(m3)

cor(train.data$total_salary,train.data$retirement, method="pearson")
cor(train.data$total_salary,train.data$salaries, method="pearson") 

#remove total_salary
cor(train.data$total_salary,train.data$organization_group_culture_recreation, method="pearson")
cor(train.data$total_salary,train.data$department_code_lib, method="pearson")
cor(train.data$total_salary,train.data$department_code_rec, method="pearson")
cor(train.data$total_salary,train.data$health_and_dental, method="pearson")
cor(train.data$total_salary,train.data$other_benefits, method="pearson")
cor(train.data$total_salary,train.data$total_benefits, method="pearson")
cor(train.data$total_salary,train.data$department_code_fam, method="pearson")

cor(train.data$organization_group_culture_recreation,train.data$total_benefits, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$retirement, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$salaries, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$department_code_lib, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$department_code_rec, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$health_and_dental, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$other_benefits, method="pearson")
cor(train.data$organization_group_culture_recreation,train.data$department_code_fam, method="pearson")

cor(train.data$salaries,train.data$total_benefits, method="pearson")
cor(train.data$salaries,train.data$retirement, method="pearson") 

#remove salaries
cor(train.data$salaries,train.data$department_code_lib, method="pearson")
cor(train.data$salaries,train.data$department_code_rec, method="pearson")
cor(train.data$salaries,train.data$health_and_dental, method="pearson")
cor(train.data$salaries,train.data$department_code_fam, method="pearson")
cor(train.data$salaries,train.data$other_benefits, method="pearson")

cor(train.data$department_code_lib,train.data$total_benefits, method="pearson")
cor(train.data$department_code_lib,train.data$retirement, method="pearson")
cor(train.data$department_code_lib,train.data$other_benefits, method="pearson")
cor(train.data$department_code_lib,train.data$department_code_rec, method="pearson")
cor(train.data$department_code_lib,train.data$health_and_dental, method="pearson")
cor(train.data$department_code_lib,train.data$department_code_fam, method="pearson")

cor(train.data$department_code_rec,train.data$total_benefits, method="pearson")
cor(train.data$department_code_rec,train.data$retirement, method="pearson")
cor(train.data$department_code_rec,train.data$other_benefits, method="pearson")
cor(train.data$department_code_rec,train.data$health_and_dental, method="pearson")
cor(train.data$department_code_rec,train.data$department_code_fam, method="pearson")

cor(train.data$retirement,train.data$total_benefits, method="pearson") 

#remove retirement
cor(train.data$retirement,train.data$other_benefits, method="pearson")
cor(train.data$retirement,train.data$health_and_dental, method="pearson")
cor(train.data$retirement,train.data$department_code_fam, method="pearson")

cor(train.data$health_and_dental,train.data$total_benefits, method="pearson") 
cor(train.data$health_and_dental,train.data$other_benefits, method="pearson")
cor(train.data$health_and_dental,train.data$department_code_fam, method="pearson")

cor(train.data$total_benefits,train.data$other_benefits, method="pearson") 
cor(train.data$total_benefits,train.data$department_code_fam, method="pearson")

cor(train.data$other_benefits,train.data$department_code_fam, method="pearson")


train.data=select(train.data,-c(retirement))
train.data=select(train.data,-c(total_salary))
train.data=select(train.data,-c(total_benefits))
train.data=select(train.data,-c(salaries))

test.data=select(test.data,-c(retirement))
test.data=select(test.data,-c(total_salary))
test.data=select(test.data,-c(total_benefits))
test.data=select(test.data,-c(salaries))

names(train.data)

#build model again after removing multicoll
m5=lm(train.data$total_compensation ~ .,data=train.data)
summary(m5)

m6=step(m5, direction = "backward", trace = T)

summary(m6)

# residual analysis
res=rstandard(m6)
plot (fitted(m6), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test.data)
y1=predict.glm(m6,test.data)
y=test.data[,167]
rmse_2 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_2

install.packages("car")
library(car)
vif(m3)

#Total compensation - Forward

# check cor for numeric variables
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")

# for transformation on var overtime
t=subdata$overtime*subdata$overtime
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$overtime)
t=1/(subdata$overtime)

subdata=select(subdata,-c(overtime))

cor(subdata$salaries,subdata$other_salaries, method = "pearson")

# for transformation on var other_salaries
t=subdata$other_salaries*subdata$other_salaries
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$other_salaries)
t=1/(subdata$other_salaries)
subdata=select(subdata,-c(other_salaries))


cor(subdata$salaries,subdata$retirement, method = "pearson")
cor(subdata$salaries,subdata$health_and_dental, method = "pearson")

# for transformation on var health_and_dental
t=subdata$health_and_dental*subdata$health_and_dental
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$health_and_dental)
t=1/(subdata$health_and_dental)
subdata=select(subdata,-c(health_and_dental))

cor(subdata$salaries,subdata$other_benefits, method = "pearson")

cor(subdata$salaries,subdata$total_compensation, method = "pearson")

cor(subdata$salaries,subdata$total_salary, method = "pearson")

cor(subdata$salaries,subdata$total_benefits, method = "pearson")

#correlation for total_compensation
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")

# for hold out evaluation
subdata=subdata[sample(nrow(subdata)),]

select.data = sample(1:nrow(subdata),0.7*nrow(subdata))
train.data=subdata[select.data,]
test.data=subdata[-select.data,]

dim(train.data)
dim(test.data)

# corealtion with variable total compensation
cor(compdata$total_compensation,compdata$salaries, method = "pearson")
cor(compdata$total_compensation,compdata$overtime, method = "pearson")
cor(compdata$total_compensation,compdata$other_salaries, method = "pearson")
cor(compdata$total_compensation,compdata$total_salary, method = "pearson")
cor(compdata$total_compensation,compdata$retirement, method = "pearson")
cor(compdata$total_compensation,compdata$health_and_dental, method = "pearson")
cor(compdata$total_compensation,compdata$other_benefits, method = "pearson")
cor(compdata$total_compensation,compdata$total_benefits, method = "pearson")

#transformation for overtime and other_salaries
t=compdata$overtime*compdata$overtime
cor(compdata$total_compensation,t, method = "pearson")
t=log(compdata$overtime)
t=1/(compdata$overtime)
compdata=select(compdata,-c(overtime))

t=compdata$other_salaries*compdata$other_salaries
cor(compdata$total_compensation,t, method = "pearson")
t=log(compdata$other_salaries)
t=1/(compdata$other_salaries)
compdata=select(compdata,-c(other_salaries))

#split data
compdata=compdata[sample(nrow(compdata)),]

select.data = sample(1:nrow(compdata),0.7*nrow(compdata))
train.data_c=compdata[select.data,]
test.data_c=compdata[-select.data,]
names(compdata)
dim(train.data_c)
dim(test.data_c)
names(test.data)

#total_compensation forward model
m1=lm(train.data_c$total_compensation ~ .,data=train.data_c)
summary(m1)

base=lm(total_compensation~total_benefits, data=train.data_c)
m2=step(base, scope=list(upper=m1, lower=~1),direction="forward",trace=F)
summary(m2)

res=rstandard(m2)
plot (fitted(m2), res, main = "Predicted vs residuals plot")
abline(a=0, b=0, col='red') 

qqnorm (res)
qqline (res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
names(test.data_c)
jarque.bera.test(res)

y1=predict.glm(m2,test.data_c)
y=test.data_c[,171]
rmse = sqrt((y-y1)%*%(y-y1)/nrow(test.data_c))
rmse

install.packages("car")
library(car)
vif(m2)

cor(train.data$total_benefits,train.data$total_salary, method="pearson")
cor(train.data$total_benefits, train.data$other_benefits, method="pearson")
cor(train.data$total_benefits, train.data$salaries, method="pearson")
cor(train.data$total_benefits, train.data$department_code_fir, method="pearson")
cor(train.data$total_benefits, train.data$organization_group_community_health, method="pearson")
cor(train.data$total_benefits, train.data$health_and_dental, method="pearson")
cor(train.data$total_benefits, train.data$retirement, method="pearson") 

#remove retirement
cor(train.data$health_and_dental, train.data$salaries, method="pearson")  
cor(train.data$health_and_dental, train.data$total_salary, method="pearson")
cor(train.data$health_and_dental, train.data$other_benefits, method="pearson")
cor(train.data$health_and_dental, train.data$department_code_fir, method="pearson")
cor(train.data$health_and_dental, train.data$organization_group_community_health, method="pearson")
cor(train.data$health_and_dental, train.data$retirement, method="pearson")

cor(train.data$retirement, train.data$other_benefits, method="pearson")
cor(train.data$retirement, train.data$department_code_fir, method="pearson")
cor(train.data$retirement, train.data$organization_group_community_health , method="pearson")
cor(train.data$retirement, train.data$salaries, method="pearson") 

#remove salaries
cor(train.data$retirement, train.data$total_salary , method="pearson")

cor(train.data$organization_group_community_health, train.data$other_benefits, method="pearson")
cor(train.data$organization_group_community_health, train.data$department_code_fir, method="pearson")
cor(train.data$organization_group_community_health, train.data$total_salary, method="pearson")
cor(train.data$organization_group_community_health, train.data$salaries, method="pearson")

cor(train.data$total_salary, train.data$salaries, method="pearson") 

#remove total salary
cor(train.data$total_salary, train.data$department_code_fir, method="pearson")
cor(train.data$total_salary, train.data$other_benefits, method="pearson")

cor(train.data$salaries, train.data$department_code_fir, method="pearson")
cor(train.data$salaries, train.data$other_benefits, method="pearson")

cor(train.data$other_benefits, train.data$department_code_fir, method="pearson")

alias(lm(train.data$total_compensation ~ .,data=train.data))

train.data=select(train.data,-c(retirement))
train.data=select(train.data,-c(total_salary))
train.data=select(train.data,-c(salaries))

test.data=select(test.data,-c(retirement))
test.data=select(test.data,-c(total_salary))
test.data=select(test.data,-c(salaries))

#total_compensation forward model after mul

m1=lm(train.data$total_compensation ~ .,data=train.data)
summary(m1)

base=lm(total_compensation~total_benefits, data=train.data)
m2=step(base, scope=list(upper=m1, lower=~1),direction="forward",trace=F)

summary(m2)

res=rstandard(m2)
plot (fitted(m2), res, main = "Predicted vs residuals plot")
abline(a=0, b=0, col='red')

qqnorm (res)
qqline (res,col=2)

jarque.bera.test(res)

names(test.data)
y1=predict.glm(m2,test.data)
y=test.data[,168]
rmse = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse

vif(m2)


cor(train.data$total_benefits,train.data$total_salary, method="pearson")
cor(train.data$total_benefits, train.data$other_benefits, method="pearson")
cor(train.data$total_benefits, train.data$salaries, method="pearson")
cor(train.data$total_benefits, train.data$department_code_fir, method="pearson")
cor(train.data$total_benefits, train.data$organization_group_community_health, method="pearson")
cor(train.data$total_benefits, train.data$health_and_dental, method="pearson")
cor(train.data$total_benefits, train.data$retirement, method="pearson") 

#remove retirement
# 2nd time model run after  vif
m3=lm(train.data$total_compensation ~ .,data=train.data)
summary(m3)

base=lm(total_compensation~other_benefits, data=train.data)
m4=step(base, scope=list(upper=m3, lower=~1),direction="forward",trace=F)

summary(m4)

res=rstandard(m4)
plot (fitted(m4), res, main = "Predicted vs residuals plot")
abline(a=0, b=0, col='red')

qqnorm (res)
qqline (res,col=2)

jarque.bera.test(res)

names(test.data)
y1=predict.glm(m4,test.data)
y=test.data[,168]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_1




#building model for predicting  variable ”salaries” with forward selection
# check cor for numeric variables

cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")
# transformation on var overtime
t=subdata$overtime*subdata$overtime
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$overtime)
cor(subdata$salaries,t, method = "pearson")
t=1/(subdata$overtime)
cor(subdata$salaries,t, method = "pearson")

subdata=select(subdata,-c(overtime))

cor(subdata$salaries,subdata$other_salaries, method = "pearson")
#  transformation on var other_salaries
t=subdata$other_salaries*subdata$other_salaries
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$other_salaries)
cor(subdata$salaries,t, method = "pearson")
t=1/(subdata$other_salaries)
cor(subdata$salaries,t, method = "pearson")
subdata=select(subdata,-c(other_salaries))



cor(subdata$salaries,subdata$retirement, method = "pearson")
cor(subdata$salaries,subdata$health_and_dental, method = "pearson")

#  transformation on var health_and_dental
t=subdata$health_and_dental*subdata$health_and_dental
cor(subdata$salaries,t, method = "pearson")
t=log(subdata$health_and_dental)
cor(subdata$salaries,t, method = "pearson")
t=1/(subdata$health_and_dental)
cor(subdata$salaries,t, method = "pearson")
subdata=select(subdata,-c(health_and_dental))
cor(subdata$salaries,subdata$other_benefits, method = "pearson")
cor(subdata$salaries,subdata$total_compensation, method = "pearson")
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$total_benefits, method = "pearson")

#correlation for total_compensation
cor(subdata$salaries,subdata$total_salary, method = "pearson")
cor(subdata$salaries,subdata$overtime, method = "pearson")

#salaries
m1=lm(train.data$salaries ~ .,data=train.data)
summary(m1)

base=lm(salaries~retirement, data=train.data)
m3=step(base, scope=list(upper=m1, lower=~1),direction="forward",trace=F)
summary(m3)

# residual analysis

res=rstandard(m3)
plot (fitted(m3), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test.data)
y1=predict.glm(m3,test.data)
y=test.data[,171]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_1

install.packages("car")
library(car)


vif(m3)

#rechecking the correlations again
cor(subdata$total_compensation,compdata$salaries, method = "pearson")
cor(subdata$total_compensation,compdata$overtime, method = "pearson")
cor(subdata$total_compensation,compdata$other_salaries, method = "pearson")
cor(subdata$total_compensation,compdata$total_salary, method = "pearson")
cor(subdata$total_compensation,compdata$retirement, method = "pearson")
cor(subdata$total_compensation,compdata$health_and_dental, method = "pearson")
cor(subdata$total_compensation,compdata$other_benefits, method = "pearson")
cor(subdata$total_compensation,compdata$total_benefits, method = "pearson")

test.data=select(test.data,-c(total_benefits))
test.data=select(test.data,-c(total_salary))

train.data=select(train.data,-c(total_benefits))
train.data=select(train.data,-c(total_salary))

m4=lm(salaries~retirement, data=train.data)
m5=step(base, scope=list(upper=m1, lower=~1),direction="forward",trace=F)

summary(m5)

# residual analysis

res=rstandard(m5)
plot (fitted(m5), res, main = "Predicted vs residuals plot")

qqnorm(res)
qqline(res,col=2)

install.packages("normtest")
library(normtest)
install.packages("tseries")
library(tseries)
jarque.bera.test(res)

names(test.data)
y1=predict.glm(m5,test.data)
y=test.data[,171]
rmse_1 = sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_1

