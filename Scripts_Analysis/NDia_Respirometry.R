---
title: "Strain Comparison within a Treatment"
output: html_document
editor_options: 
chunk_output_type: console
---
  #Libraries and Reading in Data
  
````{r}
library(readxl)
library(ggplot2)

##Read in the data
data <- read_excel("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/GitHub/Respirometry/Data/Respir.xlsx", sheet = "Sheet1")

##Remove (read:subset) "Blank Control" syringes using exclaimation point
dataUB=subset(data,data$syringe!="Blank Control")
dataUB=subset(dataUB, dataUB$stage!="P")
dataUB=subset(dataUB, dataUB$stage!="E")
dataUB=subset(dataUB, dataUB$stage!="A")
dataUB=subset(dataUB, dataUB$stage!="I")
dataUB=subset(dataUB, dataUB$treat!="1")

`````

# Plots of CO2 production
`````{r}
#Hourly CO2 production
##Plot of hourly CO2 production as an individual line plots
ggplot(data=dataUB,aes(x=day,y=co2_hourly))+
  geom_point()+
  geom_line()+
  ggtitle("Non Diapause")+
  ylab("CO2 production")+
  facet_wrap(~strain)+
  scale_x_continuous(breaks = c(5,15,30,45,60))+
  scale_y_continuous(limits=c(0,8))

##Set "treat" (read:treatment) as a factor (read:independent variable)
dataUB$strain=as.factor(dataUB$strain)

##Plot of hourly CO2 production as a merged line plot
ggplot(data=dataUB,aes(x=day,y=co2_hourly,color=strain))+
  theme_classic()+
  ggtitle("Non Diapause Treatment")+
  ylab("CO2 production")+
  scale_colour_discrete(name="Strain",
                        breaks=c("BE", "UZ"),labels=c("BE", "UZ"))+
  geom_point()+stat_smooth()+
  scale_x_continuous(breaks =c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,8))
`````

#ANOVA Model for CO2 production
```{r}
## Significance of photoperiod and/or day to explain hourly CO2 production 
mod=aov(dataUB$co2_hourly ~ dataUB$strain*dataUB$day)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
```

#Plot of CO2 production weighted by mass
````{r}
##Hourly CO2 production weighted by wet mass
## Plot of hourly CO2 production weighted by mass
ggplot(data=dataUB,aes(x=day,y=co2_hourly_mass))+
  geom_point()+geom_line()+
  ggtitle("Non Diapause: Strain Comparison")+
  ylab("CO2 production by mass")+
  facet_wrap(~strain)+
  scale_x_continuous(breaks = c(5,15,30,45,60))+
  scale_y_continuous(limits=c(0,100))

##Plot of hourly CO2 production weighted by mass as a merged line plot
ggplot(data=dataUB,aes(x=day,y=co2_hourly_mass,color=strain))+
  geom_point()+stat_smooth()+
  theme_classic()+
  ggtitle("Non Diapause Treatment by Mass")+
  ylab("CO2 production by mass")+
  scale_colour_discrete(name="Strain",
                        breaks=c("BE", "UZ"),labels=c("BE", "UZ"))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous(limits=c(0,100))
````

#ANOVA model of CO2 production weighted by mass
```{r}
## Significance of photoperiod and/or day to explain hourly CO2 production weighted by mass
mod1=aov(dataUB$co2_hourly_mass ~ dataUB$strain*dataUB$day)
par(mfrow=c(2,2))
summary(mod1)
plot(mod1)
```