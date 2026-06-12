#Figures
rm(list=ls())

library(patchwork)


pcaVeg<-readRDS("figures/p_comord.rds")
biomassfig<-readRDS("figures/p_bio.rds")
plantnuts<-readRDS("figures/p_nuts.rds")
prsfig<-readRDS("figures/p_prs.rds")



combinefig<-(pcaVeg / biomassfig / prsfig) | plantnuts#+
#  plot_layout(widths = c(1.3,1))
combinefig
ggsave("figures/combfig.png",width=12,height=12,units="in")

