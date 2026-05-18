#Ordination of biomass and nutrient data

rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)

#Load data
setesdal_prsplant<-read.csv("data/combined_wide_dataset.csv",header=TRUE)

names(setesdal_prsplant)
#PCA on all biomass
setesdal_ord_biomass<-setesdal_prsplant[,4:18]
#Drop rows with some NAs (will exclude all plots without PRS etc)
setesdal_ord_biomass_complete<-na.omit(setesdal_ord_biomass)


pca <- prcomp(setesdal_ord_biomass_complete, scale. = TRUE)
biplot(pca)

scores <- as.data.frame(pca$x)
scores$TreatmentID <- setesdal_prsplant$TreatmentID[complete.cases(setesdal_prsplant[, names(setesdal_ord_biomass_complete)])]

ggplot(scores, aes(PC1, PC2, color = TreatmentID)) +
  geom_point(size = 3) +
  theme_bw()

loadings <- as.data.frame(pca$rotation)
loadings$Variable <- rownames(loadings)

# Scale arrows (important!)
arrow_scale <- 5

percentVar <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)

ggplot(scores, aes(PC1, PC2, color = TreatmentID)) +
  geom_point(size = 3) +
  
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * arrow_scale,
                   yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE) +
  
  geom_text(data = loadings,
            aes(x = PC1 * arrow_scale,
                y = PC2 * arrow_scale,
                label = Variable),
            inherit.aes = FALSE,
            hjust = 1.1,
            vjust = 1.1) +
  ggtitle("Biomass")+
  xlab(paste0("PC1 (", percentVar[1], "%)")) +  ylab(paste0("PC2 (", percentVar[2], "%)"))+
  theme_bw()



setesdal_ord_All<-setesdal_prsplant[,4:ncol(setesdal_prsplant)]
setesdal_ord_All<- setesdal_ord_All %>% select(-PlotID)
#Drop rows with some NAs (will exclude all plots without PRS etc)
setesdal_mean_complete<-na.omit(setesdal_ord_All)

pcaM <- prcomp(setesdal_mean_complete[,4:ncol(setesdal_mean_complete)], scale. = TRUE)

scoresM <- as.data.frame(pcaM$x)
scoresM$TreatmentID <- setesdal_prsplant$TreatmentID[complete.cases(setesdal_prsplant[, names(setesdal_mean_complete)])]

ggplot(scoresM, aes(PC1, PC2, color = TreatmentID)) +
  geom_point(size = 3) +
  theme_bw()

loadingsM <- as.data.frame(pcaM$rotation)
loadingsM$Variable <- rownames(loadingsM)

# Scale arrows (important!)
arrow_scale <- 30

percentVarM <- round(100 * (pcaM$sdev^2 / sum(pcaM$sdev^2)), 1)

ggplot(scoresM, aes(PC1, PC2)) +
  geom_point(aes(color = TreatmentID), size = 3) +
  
  geom_segment(
    data = loadingsM,
    aes(x = 0, y = 0,
        xend = PC1 * arrow_scale,
        yend = PC2 * arrow_scale),
    color = ifelse(grepl("Jun|Aug", loadingsM$Variable), "darkgreen",
                   ifelse(grepl("prs", loadingsM$Variable, ignore.case = TRUE),
                          "goldenrod", "black")),
    arrow = arrow(length = unit(0.2, "cm")),
    inherit.aes = FALSE
  ) +
  
  geom_text(
    data = loadingsM,
    aes(
      x = PC1 * arrow_scale,
      y = PC2 * arrow_scale,
      label = Variable
    ),
    color = ifelse(grepl("Jun|Aug", loadingsM$Variable), "darkgreen",
                   ifelse(grepl("prs", loadingsM$Variable, ignore.case = TRUE),
                          "goldenrod", "black")),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = 1.1,
    size = 3
  ) +
  
  ggtitle("All") +
  xlab(paste0("PC1 (", percentVarM[1], "%)")) +
  ylab(paste0("PC2 (", percentVarM[2], "%)")) +
  theme_bw()
