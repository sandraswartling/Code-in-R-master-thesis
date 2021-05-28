#Scatterplot, perhaps regression, line or inverted u- shape,  climate anxiety and behaviour 
#H. B A sufficient amount of climate anxiety will correlate with pro-environmental behaviour. Thus, we hypothesise that either a linear regression or an inverted u- shape will model climate anxiety and pro-environmental behaviour.  We will test this to discover if a linear or u- shaped model fit better. 
# http://r-statistics.co/Linear-Regression.html

library('lavaan')
library('readr')
library('readxl')
library('tidyr')
library('ggplot2')
library('dplyr')
library('scales')

getwd()
datapath <- '/Users/sandraswartling/Documents/KURSER/UPPSATS 2020/Data/factor scores and demo doggebogge.xlsx'
sumf<- read_xlsx(datapath, col_names=TRUE)

#reg 1, CA and PEB1 (less effort and impact)
CAscore <- sumf$`summaf1f2f6`
PEBscore <- sumf$`summaf4`

# Kod för Basic scatter plot
ggplot(sumf, aes(x=CAscore, y=PEBscore))+
  geom_point(size=2, shape=10, color="coral3", fill="coral3")+
  geom_smooth(method=lm, color='orange')+
  ggtitle('Climate Anxiety and Pro- env.behaviour,less effort and impact')+ 
  theme_bw()


# Fit regression line
require(stats)
reg<-lm(PEBscore ~ CAscore, data = sumf)
reg

#correlation
cor(sumf$summaf1f2f6, sumf$summaf4)

linearMod <- lm(PEBscore ~ CAscore , data=sumf)  # build linear regression model on full data
print(linearMod)

summary(linearMod) #t- value 5.14, p- value individual predictor variables 0.000000678, model p- value = 0.000000678

AIC(linearMod)  
BIC(linearMod)

#corr 2, CA and PEB2 larger imapct
CAscore <- sumf$`summaf1f2f6`
PEBscore2 <- sumf$`summaf5`

# Kod för Basic scatter plot
ggplot(sumf, aes(x=CAscore, y=PEBscore2))+
  geom_point(size=2, shape=10, color="darkred")+
  geom_smooth(method=lm, color='orange')+
  ggtitle('Climate Anxiety and Pro- env.behaviour,more effort and impact')+
  theme_bw()

# Fit regression line
require(stats)
reg2<-lm(PEBscore2 ~ CAscore, data = sumf)
reg2

#correlation
cor(sumf$summaf1f2f6, sumf$summaf5)

linearMod2 <- lm(PEBscore2 ~ CAscore , data=sumf)  # build linear regression model on full data
print(linearMod2)

summary(linearMod2) #t- value 11.100 p- value individual predictor variables 0.0000000000000002, model p- value  0.00000000000000022 

AIC(linearMod2)  
BIC(linearMod2)


