setwd("C:/Users/doggy/OneDrive/Documents/OmicsInOncology_CPS341nBio435_FS25/week2/exercises")
#%>% CTRL+SHIFT+M shortcut
library(ggplot2)
View(mpg)
ggplot(data=mpg, aes(x=class, y=displ))+
    geom_boxplot(outlier.shape=NA)+ #no explicit outliers
    geom_jitter(aes(color=manufacturer), width=0.2, alpha=0.7)+ #add jittered
    theme_classic()+
    ylim(0,10)+ #y axis limits
    labs(
        title= "Engine size by car class",
        x="Car class",
        y= "Engine size (displacement in liters)",
        color="Manufacturer"
    ) +
    theme(legend.position="bottom") #legend on bottom
install.packages("tidyverse")
library(tidyverse)
mpg %>% #calculate mean engine size
    select(displ) %>%  #select engine size
    pull() %>%  #extract values as vector
    mean(na.rm=TRUE) #find mean, remove NAs
#3.471795
#Exercise 5
prawnGR<-read.csv("prawnGR.CSV")
ggplot(prawnGR, aes(x=diet, y=GRate))+
    geom_boxplot()+
    geom_jitter(aes(color=diet), width=0.2)+
    theme_minimal()
table(prawnGR$diet)
#Shapiro Wilk normality test
shapiro.test(prawnGR$GRate[prawnGR$diet=="Natural"])
shapiro.test(prawnGR$GRate[prawnGR$diet=="Artificial"])
var.test(Grate~diet, data=prawnGR)
t_test_result<=t.test(GRate~diet, data=prawnGR, var.equal=TRUE)
print(t_test_result)
growth.lm <- lm(GRate ~ diet, data = prawnGR)
summary(growth.lm)
anova_result <- anova(growth.lm)
print(anova_result)
par(mfrow=c(2,2))
plot(growth.lm)
gigartina <- read.csv("Gigartina.CSV")
str(gigartina)
table(gigartina$diatom.treat) # replicates per treatment
ggplot(gigartina, aes(x = diatom.treat, y = diameter)) +
    geom_boxplot() + # boxplot of diameter by treatment
    geom_jitter(aes(color = diatom.treat), width = 0.2) +
    theme_minimal()
gigartina.lm <- lm(diameter ~ diatom.treat, data = gigartina)
anova_result <- anova(gigartina.lm)
print(anova_result)
par(mfrow=c(2,2))
plot(gigartina.lm)
install.packages("mosaic")
library(mosaic)
tukey_result <- TukeyHSD(aov(diameter ~ diatom.treat, data = gigartina))
print(tukey_result)
plot(tukey_result)
temora <- read.csv("data/TemoraBR.csv")
str(temora)
temora$Facclimitisation_temp <- factor(temora$acclimitisation_temp) # convert acclimitisation_temp to factor
ggplot(temora, aes(x = temp, y = beat_rate, color = Facclimitisation_temp)) +
    geom_point() + # scatterplot of beat_rate vs temp by acclimitisation_temp
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()
ancova_model <- lm(beat_rate ~ temp * Facclimitisation_temp, data = temora)
summary(ancova_model)
anova_result <- anova(ancova_model)
print(anova_result)
par(mfrow=c(2,2))
plot(ancova_model)

