---
title: "Respirometry BE"
output: html_document
editor_options: 
chunk_output_type: console
---
# Goal: To track the production of CO2 in ECB larvae experiencing long day and short day conditions with the intent of phenotyping diapausing larvae.

## Libraries
````{r}
##Load Libraries
library(readxl)
library(ggplot2)

##Read in the data
data <- read_excel("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/GitHub/Respirometry/Data/Respir.xlsx", sheet = "Sheet1")

##Remove (read:subset) "Blank Control" syringes using exclaimation point
data2=subset(data,data$strain!="UZ")
dataBE=subset(data2,data2$syringe!="Blank Control")
dataBE=subset(dataBE, dataBE$stage!="P")
dataBE=subset(dataBE, dataBE$stage!="E")
dataBE=subset(dataBE, dataBE$stage!="A")
dataBE=subset(dataBE, dataBE$stage!="I")

##Set "syringe" identifier as a number
dataBE$syringe=as.numeric(dataBE$syringe)
`````

`````{Plots of CO2 production}
#Hourly CO2 production
##Plot of hourly CO2 production as an individual line plots
ggplot(data=dataBE,aes(x=day,y=co2_hourly))+
  geom_point()+geom_line()+
  ggtitle("BE Larvae")+
  ylab("CO2 production")+
  facet_wrap(~syringe)+
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,8))

##Set "treat" (read:treatment) as a factor (read:independent variable)
dataBE$treat=as.factor(dataBE$treat)

##Plot of hourly CO2 production as a merged line plot
ggplot(data=dataBE,aes(x=day,y=co2_hourly,color=treat))+
  theme_classic()+
  ggtitle("BE Larvae")+
  ylab("CO2 production")+
  scale_colour_discrete(name="Diapause",
  breaks=c("1", "2"),labels=c("Diapause", "Non-Diapause"))+
  geom_point()+stat_smooth()+
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,8))
`````

`````{ANOVA Model for CO2 production}
## Significance of photoperiod and/or day to explain hourly CO2 production 
mod=aov(dataBE$co2_hourly ~ dataBE$treat*dataBE$day)
summary(mod)
`````

`````{Plot of CO2 production weighted by mass}
##Hourly CO2 production weighted by wet mass
## Plot of hourly CO2 production weighted by mass
ggplot(data=dataBE,aes(x=day,y=co2_hourly_mass))+
  geom_point()+geom_line()+
  ggtitle("BE Larvae")+
  ylab("CO2 production by mass")+
  facet_wrap(~syringe)+
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,100))

##Plot of hourly CO2 production weighted by mass as a merged line plot
ggplot(data=dataBE,aes(x=day,y=co2_hourly_mass,color=treat))+
  geom_point()+stat_smooth()+
  theme_classic()+
  ggtitle("BE Larvae")+
  ylab("CO2 production by mass")+
  scale_colour_discrete(name="Diapause",
  breaks=c("1", "2"),labels=c("Diapause", "Non-Diapause"))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,100))
````

`````{ANOVA model of CO2 production weighted by mass}
## Significance of photoperiod and/or day to explain hourly CO2 production weighted by mass
mod1=aov(dataBE$co2_hourly_mass ~ dataBE$treat*dataBE$day)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1) 
`````

