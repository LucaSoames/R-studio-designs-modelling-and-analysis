library(tidyverse)
library(ggplot2)
install.packages("GGally")
library(GGally)
data = read.csv("PST2data(2).csv")

view(data)
summary(data)


#A 1.2

data2 = mutate(data, "TimeSince" = Year - 1970)
View(data2)

#A 1.3

summary(data2)


#A 2.1

ggpairs(data2)


#A 2.2
library(ggpubr)

data2 %>%
  ggplot(aes(x = TimeSince, y = Transistors)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  labs(title = "Variability of Transistors over Time Since 1970", x = "Time since 1970 in Years", y = "Number of Transistors") +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = x ~ y)



  #A 2.3

data2 %>%
  ggplot(aes(x = log(TimeSince), y = Transistors)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  labs(title = "Variability of Transistors vs Natural Log of Time since 1970", 
       x = "Natural Log of Years Since 1970", 
       y = " Number of Transistors") +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = x ~ y)


#A 2.4
data2 %>%
  ggplot(aes(x = TimeSince, y = log(Transistors))) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  labs(title = "Natural Log of the Number Of Transistors vs Years Since 1970",
       x = "Years Since 1970",
       y = "Natural Log of Number of Transistors") + 
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = x ~ y)


#A 2.5
data2 %>%
  ggplot(aes(x = log(TimeSince), y = log(Transistors))) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  labs(title = "Natural Log of the Number of Transistors vs Natural Log of the Years Since 1970",
       x = "ln(Years Since 1970)", 
       y = "ln(Number of Transistors)") + 
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = x ~ y)
 

#B 1.1
library(broom)

model= lm(data= data2, formula = log(Transistors) ~ TimeSince) 
model

glance(model)

modelConf = tidy(model, conf.int = TRUE, conf.level = 0.95)
modelConf

#B 1.2

summary(model)

#B 2.1

epsilon <- residuals(model)

model_fort = fortify(model)
ggplot(data = model_fort, aes( x = .fitted, y = .resid))+
  geom_point()+
  theme_bw() +
  labs(title = "Variability of Model Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

#B 2.2

ggplot(data = model_fort, aes(sample= .stdresid))+
  stat_qq(alpha = 0.5)+
  theme_bw()+
  geom_abline() + 
  labs(title = "Quantiles of Standardised Residuals vs Standard Normal Distribution Values",
       x = "Standardised Residuals",
       y = "Normal Distributed Values")
#B 3.2

df = read.csv("pst2individualised.csv")
df2 = filter(df, Username == "n11047836")

#standard error

summary(model)$coefficients

# determine residual degrees of freedom

model$df.residual

# determine test statistic

ts = (0.3264-0.287547)/0.005128

# perform two sided t-test

2*pt(ts, df = 105, lower.tail = FALSE)

#B 3.3 
# critical rejection

alpha = 0.05
standardised_rejection_level = qt(p = c(0.025, 0.975), 
                                  df = 105)
standardised_rejection_level

#C 1.1

model_fort = fortify(model)
ggplot(data = model_fort, aes( x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme_bw() +
  labs(title = "Variability of Model Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")



