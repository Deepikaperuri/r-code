###############################################################
#------------------ Cutlets dataset ----------------------

df<-read.csv('C:\\Users\\Gayathri\\Desktop\\excelr\\assignments\\hypothesis\\Cutlets.csv')
View(df)

barplot(df$Unit.A)
barplot(df$Unit.B)
hist(df$Unit.B)
hist(df$Unit.A)

####### normality test
class(df$Unit.B)
shapiro.test(df$Unit.A) ## pvalue=0.32>0.05 means it  follows normal distribution
shapiro.test(df$Unit.B) ## pvalue=0.522>0.05  it also follows normal distribution

##### variance test
var.test(df$Unit.A,df$Unit.B) ### pvalue=0.4562>0.05 means variances are equal 

##### from our problem statement x is descrete , y is continious
?t.test()
#H0 : diameter of cutlets are same
#H1 : diameter of cutlets are not same
t.test(df$Unit.A,df$Unit.B,alternative = 'two.sided',paired = T)
#### pvalue<-0.4723>0.05 means accept null nypothesis 

###################################################################################
#------------------- labTAT dataset -------------------------

df<-read.csv('C:\\Users\\Gayathri\\Desktop\\excelr\\assignments\\hypothesis\\LabTAT.csv')
View(df)
## normality test
shapiro.test(df$Laboratory.1) ## 0.55>0.05
shapiro.test(df$Laboratory.2) ## 0.8637 > 0.05
shapiro.test(df$Laboratory.3) ## 0.4205> 0.05
shapiro.test(df$Laboratory.4) ### 0.6619>0.05

#H0 : there is o significant difference between the groups
#H1 : there is atleast one significant difference among the groups

#### stacking data

df1 <- stack(df)
View(df1)
library(car)
?leveneTest
leveneTest(values~ind,data=df1)
# pvalue = 0.05161 > 0.05

anova <- aov(values~ind,data=df1)
summary(anova)

## pvalue < 0.05 we reject null hypothesis 
# there is atleast one significentdifference among the groups

##################################################################################

#-------------------- BuyerRatio ----------------------------------

df<-read.table('C:\\Users\\Gayathri\\Desktop\\excelr\\assignments\\hypothesis\\BuyerRatio.txt')
View(df)
a<-stack(df)
View(a)
table(a)
x<-chisq.test(table(a))
x
table(df)
##########################
df<-read.csv('C:\\Users\\Gayathri\\Desktop\\excelr\\assignments\\hypothesis\\Faltoons.csv')
View(df)
table(df$Weekdays,df$Weekend)

prop.test(x=c(167,47),n=c(400,400))

prop.test(df$Weekdays,df$Weekend)



####################### customer order
library(readxl)
df<-read.csv(file.choose())
View(df)
df[df]
a<-stack(df)
table(df)
CrossTable(df)
a<-ifelse(df$Phillippines=='Defective',0,1)
b<-ifelse(df$Indonesia=='Defective',0,1)
c<-ifelse(df$Malta=='Defective',0,1)
d<-ifelse(df$India=='Defective',0,1)
data<-as.data.frame(cbind(a,b,c,d))
View(data)
colnames(data)<-c('Phillippines','Indonesia','Malta','India')
a<-stack(data)
View(a)
table(a)
x<-chisq.test(table(a))
x
