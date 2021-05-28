#H. A We hypothesise that the data collected will validate the climate anxiety scale from Clayton and Karazia’s study and that a similar four-factor structure will match our sample. 
install.packages("knitr")
library('lavaan')
library('readr')
library(readxl)
library(tidyr)
library(ggplot2)
#Density line
library(dplyr)
library(scales)


getwd()
setwd('/Users/sandraswartling/Documents/KURSER/UPPSATS 2020/Data')
datapath <- '/Users/sandraswartling/Documents/KURSER/UPPSATS 2020/Data/Data study 3 climate anxiety scale.xlsx'
df3 <- read_xlsx(datapath,col_names=TRUE)
names(df3) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22")

#Histogram w. facet wrap
df3 %>% gather() %>% head()
ggplot(gather(df3), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = 'free_x')

#Density line
data(tips, package = 'reshape2')
df3 %>% gather() %>% head()
ggplot(gather(df3), aes(value)) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  geom_density(adjust=1) +
  facet_wrap(~key, scales = 'free_x') 

#CFA w. MLM
previous_factors <-"Anxiety_cei =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8
                    Anxiety_fi  =~ Q9 + Q10 + Q11 + Q12 + Q13
                    Experience  =~ Q14 + Q15 + Q16
                    Behavioraleng =~ Q17 + Q18 + Q19 + Q20 + Q21 + Q22"
fit_CAs3 <- cfa(previous_factors, data=df3, std.lv=TRUE, estimator='MLM')
parameterEstimates(fit_CAs3,ci=F, standardized = T)
summary(fit_CAs3, fit.measures=T, standardized=T)
save_name <- '/Users/sandraswartling/Documents/KURSER/UPPSATS 2020/save_outputCAS3.txt'
sink(save_name)
print(summary(fit_CAs3, fit.measures=T, standardized=T))
sink()

#table with factor loadings, cleaner
library(dplyr) 
library(tidyr)
library(knitr)
parameterEstimates(fit_CAs3, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")





