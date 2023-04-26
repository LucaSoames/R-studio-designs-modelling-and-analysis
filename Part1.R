library(tidyverse)
data(Theoph)
summary(Theoph)
?datasets::Theoph
Theoph1 = select(Theoph, Wt, Dose)
summary(Theoph1)

Table1 = function(data){
  return(
    data.frame(
      Minimum=min(data),
      Maximum=max(data),
      Median=median(data),
      Mean= mean(data),
      'Standard Deviation'= sd(data)
    )
  )
}
Table1(Theoph$Wt)
Table1(Theoph$Dose)
Table1(Theoph$Wt)

Theoph %>%
  mutate(Subject = as.character(Subject),
         Subject = factor(Subject, levels = as.character(1:12))) %>%
  group_by(Subject)%>% 
  summarize(Minimum=min(conc, na.rm = TRUE),
            Median=median(conc, na.rm = TRUE),
            Maximum=max(conc))
# Create a plot that shows the variability in weight.

Theoph %>% 
  group_by(Subject) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(x = Wt)) + 
  geom_histogram(binwidth = 3, colour = "purple", fill = "white") +
  labs(title = "Weight Variability of Subjects",y = "Subjects", 
       x = "Weight(kg)",) 

# Create a plot that shows the variability in dose across all the subjects.

Theoph %>% 
  group_by(Subject) %>%
  slice(2) %>%
  ungroup() %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(x = Dose)) + 
  geom_histogram(binwidth = 0.2, colour = "purple", fill = "white") +
  labs(title = "Dose Variability of Subjects",y = "Subjects", 
       x = "Dose(mg)",) 

#Create a plot that shows the variability in concentration in each subject.
Theoph %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
ggplot(aes(x = Subject, y = conc)) +
  geom_boxplot(aes(), alpha=1) +
  geom_jitter(alpha=0.5, width=0.1) +
  labs(title = "Variability of Concentration of each Subject", x="Subject", y="Concentration(mg/kg)")

#Create a plot that shows how concentration in the blood stream varies with time.

Theoph %>%
  ggplot(aes(x = Time, y = conc)) + 
  geom_point() + 
  labs(title = "Concentration in Blood Stream over Time",x = "Time(hr)", y = "Concentration(mg/kg)")

# Create a plot that shows how the concentration varies with time for each subject.

Theoph %>%
  group_by(Subject) %>%
  mutate(Subject=as.character(Subject),
        Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(Time, conc, col=Subject)) +
  geom_line()+
  labs(title = "Concentration in Each Subject over Time", x = "Time (hr)", "Concentration (mg/kg)")

#Recreate the plot from the previous activity,but this time add a smooth line of best fit to the data.

Theoph %>%
  group_by(Subject) %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(Time, conc, col=Subject)) +
  geom_line()+
  geom_smooth(method=lm, se=FALSE, colour = 'black')+
  labs(title = "Concentration in Each Subject over Time", x = "Time (hr)", y = "conc (mg/kg)")
  
#Exercise: Copy and paste the below code fragment into your R script file. Replace the ... 
#with your own code in order to make this a function that reads in a value of time, t, 
#and a vector of parameters for c_0,k_a,k_c, and returns y_2 (t).

parms = c(480, 0.105, 1.494)

calculate_blood_amount <- function(t, parms){
  y_2 = ((k_a*c_0)/(k_c-k_a))*exp(-k_a*t)+((k_a*c_0)/(k_a-k_c))*exp(-k_c*t)
  
  c_0 <- parms[1]
  k_a <- parms[2]
  k_c <- parms[3]
  
  return(y_2)
  
}

#plot the amount of the compound in the bloodstream for the first 24 hours since ingestion

df = data.frame(t=c(0, 24))

ggplot(data = df, aes(x=t))+ 
  stat_function(fun = calculate_blood_amount, args = list(parms)) + 
  geom_point(y = 27.6, x = 1.911) + 
  geom_label(data = data.frame(x = 9, y = 27.6), aes(x,y), label = "(1.91, 27.6), Max concentration (mg)", size = 3) +
  labs(title = "Theophylline in Bloodstream over Time in Hours.", y = "Theophylline(mg)", x = "Time(hr)") +
  theme_bw()

#Create a data frame containing a sequence of times from 0 to 24 hours in fifteen minute increments

{
  df_15min = data.frame(t =seq(0, 24, by = 0.25)) 
  y_1 = expression(c_0*exp(-k_a*t)) 
  y_2 = expression(((k_a*c_0)/(k_c-k_a))*exp(-k_a*t)+((k_a*c_0)/(k_a-k_c))*exp(-k_c*t)) 
  
  c_0 = 480 
  k_a = 0.063
  k_c = 0.456
  
  df_15min = mutate(df_15min, Stomach_Theoph = eval(y_1))
  df_15min = mutate(df_15min, Blood_Theoph = eval(y_2))
  
  Time_Hr = df_15min$t
  
  head(df_15min, 24)
  
  Stomach_Theoph_2dec = round((df_15min$Stomach_Theoph), digits = 2)
  Blood_Theoph_2dec = round((df_15min$Blood_Theoph), digits = 2)
  
  
  Table_df = cbind(Time_Hr, Stomach_Theoph_2dec, Blood_Theoph_2dec)
}

View(Table_df)

#Create a visualisation that shows y_1 (t) and y_2 (t). 

calculate_stomach_amount <- function(t, parms){
  y_1 = (c_0*exp(-k_a*t))
  
  c_0 <- parms[1]
  k_a <- parms[2]
  k_c <- parms[3]
  
  return(y_1)
  
}
ggplot(data = df, aes(x=t)) + 
  stat_function(fun = calculate_blood_amount, args = list(parms)) + 
  stat_function(fun= calculate_stomach_amount, args = list(parms), colour = "Red") +
  labs(title = "Theophylline in Bloodstream over Time in Hours.", y = "Theophylline(mg)", x = "Time(hr)") +
  theme_bw()


#You may need to produce an additional plot to draw an effective comparison.
install.packages("patchwork")
library(patchwork)

p1 = Theoph %>%
  group_by(Subject) %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(Time, conc, col=Subject)) +
  geom_line()+
  labs(x = "Time(hr)", y = "Concentration (mg/kg)") 
  

p2 = ggplot(data = df, aes(x=t))+ 
  stat_function(fun = calculate_blood_amount, args = list(parms)) + 
  labs(y = "Theophylline(mg)", x = "Time(hr)") +
  theme_bw()


p3 = Theoph %>%
  group_by(Subject) %>%
  mutate(Subject=as.character(Subject),
         Subject=factor(Subject, levels=as.character(1:12))) %>%
  ggplot(aes(Time, conc, col=Subject)) +
  geom_line() +
  stat_function(fun = calculate_blood_amount, args = list(parms)) +  
  labs(title = "Theophylline in Bloodstream over Time in Hours.", y = "Theophylline", x = "Time") +
  theme_bw()

p3 / (p1 + p2)

