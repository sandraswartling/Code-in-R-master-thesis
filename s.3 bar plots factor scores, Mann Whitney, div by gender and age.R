#bar plot for every factor divided by gender, Mann Whitney u test instead of t-test
#H D difference btw groups on climate anxiety

library('lavaan')
library('readr')
library('readxl')
library('tidyr')
library('ggplot2')
library('dplyr')
library('scales')

#barplot, f1, gender
# Basic barplot

pdf <- data.frame(gender=c("female", "male"),
                 averagescore=c(12.79545455, 12.34482759))
head(pdf)


p <-ggplot(data=pdf, aes(x=gender, y= averagescore, fill=gender)) +
 geom_bar(stat="identity")+
  ggtitle('Cognitive- emotional impairement score')
p

#Mann Whitney U test, NOT SIGNIFICANT
wilcox.test(averagescore ~ gender, data= pdf, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f2, gender
# Basic barplot

pdf2 <- data.frame(gender=c("female", "male"),
                  averagescore2=c(1.563131313, 1.643678161))
head(pdf2)


p2 <-ggplot(data=pdf2, aes(x=gender, y= averagescore2, fill=gender)) +
  geom_bar(stat="identity")+
  ggtitle('Emotional- functional impairment score')
p2

#Mann Whitney U test, NOT SIGNIFICANT
wilcox.test(averagescore2 ~ gender, data= pdf2, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f3, gender
# Basic barplot

pdf3 <- data.frame(gender=c("female", "male"),
                  averagescore3=c(2.78030303, 2.511494253))
head(pdf3)


p3 <-ggplot(data=pdf3, aes(x=gender, y= averagescore3, fill=gender)) +
  geom_bar(stat="identity")+
  ggtitle('Experience of climate change')
p3

#Mann Whitney U test, NOT SIGNIFICANT
wilcox.test(averagescore3 ~ gender, data= pdf3, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f4, gender
# Basic barplot

pdf4 <- data.frame(gender=c("female", "male"),
                  averagescore4=c(4.278409091, 3.918103448))
head(pdf4)


p4 <-ggplot(data=pdf4, aes(x=gender, y= averagescore4, fill=gender)) +
  geom_bar(stat="identity")+
  ggtitle('Pro- environmental behaviour, less effort and impact')
p4

#Mann Whitney U test, NOT SIGN
wilcox.test(averagescore4 ~ gender, data= pdf4, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f5, gender
# Basic barplot

pdf5 <- data.frame(gender=c("female", "male"),
                  averagescore5=c(2.805871212, 2.612068966))
head(pdf5)


p5 <-ggplot(data=pdf5, aes(x=gender, y= averagescore5, fill=gender)) +
  geom_bar(stat="identity")+
  ggtitle('Pro- environmental behaviour, more effort and impact')
p5

#Mann Whitney U test, NON SIGN
wilcox.test(averagescore5 ~ gender, data= pdf5, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f6, gender
# Basic barplot

pdf6 <- data.frame(gender=c("female", "male"),
                  averagescore6=c(3.025252525, 2.545977011))
head(pdf6)


p6 <-ggplot(data=pdf6, aes(x=gender, y= averagescore6, fill=gender)) +
  geom_bar(stat="identity")+
  ggtitle('Shame, Guilt, Hopelessness')
p6

#Mann Whitney U test, NON SIGN
wilcox.test(averagescore6 ~ gender, data= pdf6, mu=0, alt='two.sided', conf.int=T, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f1, age
# Basic barplot

pdfa <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                  averagescorea=c(2.159340659, 1.902040816, 1.732142857, 1.581280788,1.551020408,1.692307692))
head(pdfa)


pa <-ggplot(data=pdfa, aes(x=age, y= averagescorea, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Cognitive- emotional impairement score')
pa

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa$averagescorea, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f2, age
# Basic barplot

pdfa2 <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                   averagescorea2=c(1.692307692, 1.661904762, 1.6, 1.540229885,1.333333333,1.538461538))
head(pdfa2)


pa2 <-ggplot(data=pdfa2, aes(x=age, y= averagescorea2, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Emotional- functional impairement score')
pa2

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa2$averagescorea2, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f3, age
# Basic barplot

pdfa3 <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                    averagescorea3=c(3.243589744, 2.766666667, 2.55, 2.563218391,2.30952381,2.512820513))
head(pdfa3)


pa3 <-ggplot(data=pdfa3, aes(x=age, y= averagescorea3, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Experience of climate change')
pa3

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa3$averagescorea3, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f4, age
# Basic barplot

pdfa4 <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                    averagescorea4=c(4.207692308, 4.14, 3.94, 3.827586207,2.785714286,2.512820513))
head(pdfa4)


pa4 <-ggplot(data=pdfa4, aes(x=age, y= averagescorea4, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Pro-environmental behaviour, small effort & impact')
pa4

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa4$averagescorea4, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f5, age
# Basic barplot

pdfa5 <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                    averagescorea5=c(2.836538462, 2.883928571, 2.7625, 2.681034483,2.017857143,2.807692308))
head(pdfa5)


pa5 <-ggplot(data=pdfa5, aes(x=age, y= averagescorea5, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Pro-environmental behaviour, more effort & impact')
pa5

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa5$averagescorea5, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)

#barplot, f6, age
# Basic barplot

pdfa6 <- data.frame(age=c("18-25", "25-35","35-45","45-55", "55-65", "65+"),
                    averagescorea6=c(3.307692308, 3.09047619, 2.766666667, 2.747126437,1.952380952,2.641025641))
head(pdfa6)


pa6 <-ggplot(data=pdfa6, aes(x=age, y= averagescorea6, fill=age)) +
  geom_bar(stat="identity")+
  ggtitle('Shame, guilt & hopelessness')
pa6

#Pairwise Mann Whitney U test, NOT SIGNIFICANT
pairwise.wilcox.test(pdfa6$averagescorea6, pdfa$age, conf.level=0.95, paired=F, exakt=T, correct=T)
