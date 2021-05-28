#EF climate anxiety and pro- environmental behav. 
#H. D Questions on the Swedish climate anxiety scale measuring behavioural engagement can be changed for specific questions from the pro-environmental behaviour questionnaire that better capture how climate anxiety is manifested in external behaviour.

install.packages("GPArotation")
library(GPArotation)
library('lavaan')
library('readr')
library('readxl')
library('psych')
library('nFactors')
library(tidyr)
library(ggplot2)
#Density line
library(dplyr)
library(scales)

#outliers
install.packages("mice")
library(mice)

library(GPArotation)

getwd()
datapath <- '/Users/sandraswartling/Documents/KURSER/UPPSATS 2020/Data/Data study 3 CA and PEB.xlsx'
CAPEB <- read_xlsx(datapath, col_names=TRUE)
names(CAPEB) <- c ('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10', 'Q11', 'Q12', 'Q13', 'Q14', 'Q15', 'Q16','Q17', 'Q18', 'Q19', 'Q20', 'Q21', 'Q22', 'Q23', 'Q24', 'Q25', 'Q26', 'Q27','Q28', 'Q29', 'Q30', 'Q31', 'Q32', 'Q33', 'Q34', 'Q35', 'Q36', 'Q37', 'Q38', 'Q39', 'Q40','Q41', 'Q42', 'Q43', 'Q44', 'Q45')

#Histogram w. facet wrap
CAPEB %>% gather() %>% head()
ggplot(gather(CAPEB), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = 'free_x')

#Density line
data(tips, package = 'reshape2')
CAPEB %>% gather() %>% head()
ggplot(gather(CAPEB), aes(value)) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  geom_density(adjust=1) +
  facet_wrap(~key, scales = 'free_x') 

#accuray
summary(CAPEB)

#recode 43, formula is max score plus minimum score minus participant. e.g: if a person score 5(5+1-5=6-5=1)
table(CAPEB$Q43)
CAPEB[ , c(43)]= 6- CAPEB[ , c(43)]
table(CAPEB$Q43)

#outliers
cutoff= sum(mahal<mean(mahal)+3*sd(mahal)) #qchisq(1-0,005,ncol(CAPEB))
mahal= mahalanobis(CAPEB, colMeans(CAPEB), cov(CAPEB))
#cutoff
ncol(CAPEB)
summary (mahal<cutoff)

#exclude outliers, exclude ppl with scores higher than cutoff (but I didn't manage to create cutoff or outliers)
noout2=subset(CAPEB, mahal<cutoff)

#no outliers

#additivity
correl= cor(noout2,use = 'pairwise.complete.obs')
symnum(correl)
correl

#assumption set up
random= rchisq(nrow(noout2), 5)
fake= lm(random~.,data = noout2)
standardized= rstandard(fake) #I don't have this from a previous analysis, can´t run
fitted= scale (fake$fitted.values)

#Linearity
qqnorm(standardized)
abline(0,1)
#looks ok, on a line, btw -2 and 2 but around 1.5 it goes upwards a bit

#homogeneity
plot(fitted, standardized)
abline(0,0)
abline(v=0)

#more spread on the upper right.
#using library(GPArotation)

#correaltion adequacy Bartlett's test (significant?)
cortest.bartlett(correl, n=nrow(noout2))
#significant, Significant effect, correlations large enough to continue

#sampling adequacy KMO test (at least 0.7)
KMO(noout2)

#How many factors
nofactors=fa.parallel(noout2, fm='ml', fa='fa')
sum(nofactors$fa.values>0.7)#new kaiser kriterion
#5 factors suggested by parallell
# 6 by Kaiser

#simple structure with a 5 factor model
CAPEBround1=fa(noout2, nfactors=5, rotate ='oblimin' , fm='ml')
CAPEBround1 #paste into excel and highlight values above 0.299 to see loadings

CAPEBround2=fa(noout2[ , -c(17, 21, 22, 28, 29, 31,35, 36)], nfactors=5, rotate ='oblimin' , fm='ml')
CAPEBround2 

CAPEBround3=fa(noout2[ , -c(2, 9, 17, 21, 22, 28, 29, 31,34, 37, 35, 3, 43, 44, 45)], nfactors=5, rotate ='oblimin' , fm='ml')
CAPEBround3 

CAPEBround4=fa(noout2[ , -c(2,7, 9, 10, 17, 21, 22, 26, 28, 29, 31,34, 37, 35, 3, 43, 44, 45)], nfactors=5, rotate ='oblimin' , fm='ml')
CAPEBround4 #nothing double, nothing without loading and no factor with less than 3 indicators (f2 14, 15, 16 and f1 4, 11, 12)

#simple structure with a 6 factor model
CAPEBround5=fa(noout2, nfactors=6, rotate ='oblimin' , fm='ml')
CAPEBround5 

CAPEBround6=fa(noout2[ , -c(1,9, 10, 17, 21, 22, 24, 29, 33, 34, 36, 37, 39, 43, 44, 45)], nfactors=6, rotate ='oblimin' , fm='ml')
CAPEBround6 #nothing double or not loading, all f with at least 3 indicators (f5 40, 41, 42 f2 14, 15, 16 f1 4, 11, 12)

#compare models
modelf5= fa(noout2[ , -c(2,7, 9, 10, 17, 21, 22, 26, 28, 29, 31,34, 37, 35, 3, 43, 44, 45)], nfactors=5, rotate ='oblimin' , fm='ml')
modelf5

modelf6= fa(noout2[ , -c(1,9, 10, 17, 21, 22, 24, 29, 33, 34, 36, 37, 39, 43, 44, 45)], nfactors=6, rotate ='oblimin' , fm='ml')
modelf6

#get CFI
finalmodelf5= fa(noout2[ , -c(2,7, 9, 10, 17, 21, 22, 26, 28, 29, 31,34, 37, 35, 3, 43, 44, 45)], nfactors=5, rotate ='oblimin' , fm='ml')
1- ((finalmodelf5$STATISTIC-finalmodelf5$dof)/(finalmodelf5$null.chisq-finalmodelf5$null.dof))

finalmodelf6= fa(noout2[ , -c(1,9, 10, 17, 21, 22, 24, 29, 33, 34, 36, 37, 39, 43, 44, 45)], nfactors=6, rotate ='oblimin' , fm='ml')
1- ((finalmodelf6$STATISTIC-finalmodelf6$dof)/(finalmodelf6$null.chisq-finalmodelf6$null.dof))

#reliability 
factor1= c(4, 11, 12)
factor2= c(14, 15, 16)
factor3= c(18, 19, 20, 23, 25)
factor4= c(2, 3, 5, 6, 7, 8,13)
factor5= c(40,41,42)
factor6= c(26, 27, 28, 30, 31, 32, 35, 38)

#CHANGE TO NOOUT2
psych::alpha(noout[, factor1])
psych::alpha(noout2[, factor2])
psych::alpha(noout2[, factor3])
psych::alpha(noout2[, factor4])
psych::alpha(noout2[, factor5])
psych::alpha(noout2[, factor6])
#look for raw alpha
#alpha factor 1 better without q8 but makes it too few questions...

#create new factor scores
noout$f1 = apply(noout2[, factor1],1, mean)#creates averages core
noout$f2 = apply(noout2[, factor2],1, mean)#creates averages core
noout$f3 = apply(noout2[, factor3],1, mean)#creates averages core

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)
