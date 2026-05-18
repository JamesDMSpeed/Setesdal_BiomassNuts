rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)


#Load data
setesdal_prsplant<-read.csv("data/combined_wide_dataset.csv",header=TRUE)

#Correlation matrices
#Between and within groups of variables

#Create subgroups of variables by type
names(setesdal_prsplant)
biomass_list<-setesdal_prsplant[,4:18]
plantnut_list<-setesdal_prsplant[,19:86]
prs_list<-setesdal_prsplant[,88:102]

#Subgroups by plant group
herb_list<-setesdal_prsplant %>%  select(matches("herb", ignore.case = TRUE))
graminoid_list<-setesdal_prsplant %>%  select(matches("graminoid", ignore.case = TRUE))
dwarfshrub_list<-setesdal_prsplant %>%  select(matches("shrub", ignore.case = TRUE))
litter_list<-setesdal_prsplant %>%  select(matches("litter", ignore.case = TRUE))


#Correlations Within variable groups
cor_Biomass <- cor(biomass_list, use = "pairwise.complete.obs")
cor_Nuts <- cor(plantnut_list, use = "pairwise.complete.obs")
cor_PRS <- cor(prs_list, use = "pairwise.complete.obs")

cor_Biomass_df <- cor_Biomass %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

cor_Nuts_df <- cor_Nuts %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

cor_PRS_df <- cor_PRS %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

ggplot(cor_Biomass_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile() + labs(x = NULL, y = NULL)+
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cor_Nuts_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile()+ labs(x = NULL, y = NULL) +
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cor_PRS_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile()+ labs(x = NULL, y = NULL) +
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Correlations Between variable groups
cor_df_BiomassPRS <- cor(biomass_list, prs_list, use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("X_var") %>%
  pivot_longer(-X_var, names_to = "Y_var", values_to = "correlation")
cor_df_BiomassPRS

ggplot(cor_df_BiomassPRS, aes(x = X_var, y = Y_var, fill = correlation)) +
  geom_tile() +xlab("Biomass")+ylab("PRS")+
  scale_fill_gradient2() +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

cor_df_BiomassNuts <- cor(biomass_list, plantnut_list, use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("X_var") %>%
  pivot_longer(-X_var, names_to = "Y_var", values_to = "correlation")
cor_df_BiomassNuts

ggplot(cor_df_BiomassNuts, aes(x = X_var, y = Y_var, fill = correlation)) +
  geom_tile() +xlab("Biomass")+ylab("Plant nutrients")+
  scale_fill_gradient2() +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

cor_df_PRSNuts <- cor(prs_list, plantnut_list, use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("X_var") %>%
  pivot_longer(-X_var, names_to = "Y_var", values_to = "correlation")
cor_df_PRSNuts

ggplot(cor_df_PRSNuts, aes(x = X_var, y = Y_var, fill = correlation)) +
  geom_tile() +xlab("PRS")+ylab("Plant nutrients")+
  scale_fill_gradient2() +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Correlations within plant groups
cor_Herb<- cor(herb_list, use = "pairwise.complete.obs")
cor_Gram <- cor(graminoid_list, use = "pairwise.complete.obs")
cor_Shrub <- cor(dwarfshrub_list, use = "pairwise.complete.obs")
cor_Litter <- cor(litter_list, use = "pairwise.complete.obs")

cor_Herb_df <- cor_Herb %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

cor_Gram_df <- cor_Gram %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

cor_Shrub_df <- cor_Shrub %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))

cor_Litter_df <- cor_Litter %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")%>%
  filter(as.numeric(factor(Var1)) <= as.numeric(factor(Var2)))


ggplot(cor_Herb_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile() + labs(x = NULL, y = NULL)+ggtitle('Herbs')+
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cor_Gram_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile() + labs(x = NULL, y = NULL)+ggtitle('Graminoids')+
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cor_Shrub_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile() + labs(x = NULL, y = NULL)+ggtitle('Dwarf shrubs')+
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cor_Litter_df, aes(x = Var2, y = Var1, fill = correlation)) +
  geom_tile() + labs(x = NULL, y = NULL)+ggtitle('Litter')+
  scale_fill_gradient2() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(pheatmap)
pheatmap(cor_Biomass)
pheatmap(cor_Nuts)
pheatmap(cor_PRS,
         clustering_distance_rows = as.dist(1 - cor_PRS),
         clustering_distance_cols = as.dist(1 - cor_PRS)
)
pheatmap(cor_Gram)
pheatmap(cor_Herb)
pheatmap(cor_Shrub)
pheatmap(cor_Litter)

