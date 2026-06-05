#Latent syndromes 

rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(FactoMineR)
#Load data
setesdal_prsplant<-read.csv("data/combined_wide_dataset.csv",header=TRUE)


names(setesdal_prsplant)


setesdal_varselect<- setesdal_prsplant[,c(4:18,21:136,138:216)]
setesdal_scale<-data.frame(scale(setesdal_varselect))


mfa1<-MFA(setesdal_varselect,group=c(15,40,76,15,64),
          name.group = c("Biomass","Community","PlantNuts","PRS","Pools"),
          type=c("s","s","s","s","s"))#Group is number of variables in each group, here biomass, community, plant nutrients, prs
summary(mfa1)

plot(mfa1,choix="group")
plot(mfa1,choix="ind")
plot(mfa1,choix="var")

mfa1$quanti.var$coord

#Describe the dimensions
dimdesc(mfa1,proba=0.05)


#Sites and syndromes
scores_df <- as.data.frame(
  mfa1$ind$coord
)

scores_df$Treatment <- setesdal_prsplant$TreatmentID
ggplot(
  scores_df,
  aes(Dim.1,
      Dim.3,
      color = Treatment)
)+
  geom_point(size = 4)+
  theme_bw()
