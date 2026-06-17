#Latent syndromes 

rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(FactoMineR)
library(emmeans)
library(multcomp)
library(multcompView)

#Load data
setesdal_prsplant<-read.csv("data/combined_wide_dataset.csv",header=TRUE)

setesdal_prsplant$TreatmentID<-factor(setesdal_prsplant$TreatmentID,
                                      levels=c("Mainland_G", "Mainland_NG","Island_NG"),
                                      labels = c("Grazed", "Exclosure", "Island"))
names(setesdal_prsplant)

#Hellinger transformation of plant community data
#Z trasnformation of other variables
#setesdal_prsplant[,21:55]<-decostand(setesdal_prsplant[,21:55],method="hellinger")
#setesdal_prsplant[,c(4:18,56:136,138:152)]<-scale(setesdal_prsplant[,c(4:18,56:136,138:152)],scale=TRUE,center = TRUE)

#setesdal_varselect<- setesdal_prsplant[,c(4:13,21:136,138:216)]#With pools
#setesdal_varselect<- setesdal_prsplant[,c(4:13,21:60,138:216)]#With pools, without concs
setesdal_varselect<- setesdal_prsplant[,c(4:18,21:136,138:152)]#Without pools

#setesdal_scale<-data.frame(scale(setesdal_varselect))



mfa1<-MFA(setesdal_varselect,
          #group=c(10,40,76,15,64),
          #name.group = c("Biomass","Community","Nutrients","PRS","Pools"),
          group=c(15,40,76,15),
          name.group = c("Biomass","Community","Nutrients","PRS"),
          type=c("s","s","s","s"))#Group is number of variables in each group, here biomass, community, plant nutrients, prs
          #type=c("s","s","s","s","s"))#Group is number of variables in each group, here biomass, community, plant nutrients, prs
         
summary(mfa1)

plot(mfa1,choix="group")
plot(mfa1,choix="ind")
plot(mfa1,choix="var")

mfa1$quanti.var$coord

#Describe the dimensions
dimensvars<-dimdesc(mfa1,proba=0.05,axes=1:5)
dimensvars

#Sites and syndromes
scores_df <- as.data.frame(
  mfa1$ind$coord
)

scores_df$Treatment <- setesdal_prsplant$TreatmentID
ggplot(
  scores_df,
  aes(Dim.1,
      Dim.4,
      color = Treatment)
)+
  geom_point(size = 4)+
    theme_bw()


# Extract quantitative results
d1 <- dimensvars$Dim.1$quanti
d4 <- dimensvars$Dim.4$quanti

# Get variable names significant in either axis
sig_vars <- union(rownames(d1), rownames(d4))

var_coords <- as.data.frame(mfa1$quanti.var$coord)

# Keep only significant ones
var_coords <- var_coords[sig_vars, c("Dim.1", "Dim.4")]
var_coords$var <- rownames(var_coords)

arrow_scale <- 5  # adjust visually

var_coords <- var_coords %>%
  mutate(
    Dim.1 = Dim.1 * arrow_scale,
    Dim.4 = Dim.4 * arrow_scale
  )

ggplot(
  scores_df,
  aes(Dim.1, Dim.4, color = Treatment)
) +
  geom_point(size = 4) +
  theme_bw() +
  
  # Arrows
  geom_segment(
    data = var_coords,
    aes(x = 0, y = 0, xend = Dim.1, yend = Dim.4),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "black",
    inherit.aes = FALSE
  ) +
  
  # Labels
  geom_text(
    data = var_coords,
    aes(x = Dim.1, y = Dim.4, label = var),
    size = 3,
    color = "black",
    inherit.aes = FALSE,
    hjust = 0.5,
    vjust = -0.5
  )

scores_df_long<-scores_df %>%
  pivot_longer(
    cols = matches("Dim"), 
    names_to = "Dimension",
    values_to = "Score"
  ) 



eig <- as.data.frame(mfa1$eig)
eig$dim <- seq_len(nrow(eig))

ggplot(eig, aes(x = dim, y = `percentage of variance`)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(`percentage of variance`, 1)),
            vjust = -0.5) +
  labs(
    x = "Dimension",
    y = "Variance explained (%)"
  ) +
  theme_bw()
ggsave("figures/screeplot.png",height=6,width=8,units="in")


#Variables with high loadings
var <- as.data.frame(mfa1$quanti.var$coord)
var$name <- rownames(var)

top_vars <- var %>%
  pivot_longer(cols = starts_with("Dim"), 
               names_to = "dim", values_to = "loading") %>%
  group_by(dim) %>%
  
  # rank by absolute loading
  mutate(abs_loading = abs(loading)) %>%
  arrange(dim, desc(abs_loading)) %>%
  
  # keep top 5 strongest overall
  slice_head(n = 6) %>%
  
  summarise(
    pos = paste(name[loading > 0][order(-loading[loading > 0])], collapse = "\n"),
    neg = paste(name[loading < 0][order(-loading[loading < 0])], collapse = "\n")#ordered so the most negative is at the bottom
  ) %>%
  rename(Dimension = dim)

top_vars


library(dplyr)
library(emmeans)

letters_df <- scores_df_long %>%
  filter(Dimension %in% c("Dim.1","Dim.2","Dim.3","Dim.4")) %>%
  group_by(Dimension) %>%
  do({
    model <- lm(Score ~ Treatment, data = .)
    
    # Ensure correct reference level
    .$Treatment <- relevel(.$Treatment, ref = "Grazed")  
    emm <- emmeans(model, ~ Treatment)
    
    cld_res <- cld(
      emm,
      Letters = letters,
      adjust = "none",   
      level = 0.95          
    )
    
    data.frame(
      Treatment = cld_res$Treatment,
      Letters = trimws(cld_res$.group),
      y = max(.$Score, na.rm = TRUE) + 0.5
    )
  })
letters_df

ggplot(data=scores_df_long[scores_df_long$Dimension %in% c("Dim.1","Dim.2","Dim.3","Dim.4"),],aes(x=Treatment,y=Score))+geom_point(aes(color=Treatment))+
  facet_wrap(~Dimension,scales="free_y",ncol=2)+theme_bw()+scale_y_continuous(limits=c(-5,5.1))+
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    shape = 16,
    aes(color =Treatment)  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    aes(color = Treatment)
  ) +
  
  geom_text(
    data = letters_df,
    aes(x = Treatment, y = 5.1, label = Letters),
    inherit.aes = FALSE,
    size = 5
  ) +
  
 
  
  #Sig axes -  Positive loadings at top
  geom_text(
    data = top_vars[top_vars$Dimension%in% c("Dim.1","Dim.2","Dim.3","Dim.4"),],
    aes(x = Inf, y = 4.8, label = paste0(pos)),
    hjust = 1.1,
    vjust = 1,
    inherit.aes = FALSE,
    size = 3,
    color = "darkgreen"
  ) +
  # Negative loadings at bottom
  geom_text(
    data = top_vars[top_vars$Dimension%in% c("Dim.1","Dim.2","Dim.3","Dim.4"),],
    aes(x = Inf, y = -5, label = paste0(neg)),
    hjust = 1.1,
    vjust = 0,
    inherit.aes = FALSE,
    size = 3,
    color = "darkred"
  )
ggsave("figures/mfrascores.png",height=10,width=8,units="in")

lm1<-lm(Dim.1~Treatment,data=scores_df)
summary(lm1)
lm2<-lm(Dim.2~Treatment,data=scores_df)
summary(lm2)
lm3<-lm(Dim.3~Treatment,data=scores_df)
summary(lm3)
lm4<-lm(Dim.4~Treatment,data=scores_df)
summary(lm4)
lm5<-lm(Dim.5~Treatment,data=scores_df)
summary(lm5)



plot_mfa_arrows <- function(
    mfa_obj,
    scores_df,
    treatment_col = "Treatment",
    dims = list(c(1, 2), c(3, 4)),
    pval = 0.05,
    arrow_scale = 3,
    max_arrows = NULL,
    repel = FALSE,
    hulls = TRUE
) {
  library(dplyr)
  library(ggplot2)
  
  if (repel) {
    require(ggrepel)
  }
  
  # --- 1. Get dimdesc results
  dimensvars <- FactoMineR::dimdesc(mfa_obj, proba = pval, axes = 1:5)
  
  # --- 2. Build scores + arrows
  var_coords <- as.data.frame(mfa_obj$quanti.var$coord)
  
  scores_list <- list()
  arrows_list <- list()
  eig <- mfa_obj$eig
  
  for (i in seq_along(dims)) {
    
    d <- dims[[i]]
    
    panel_name <- paste0(
      "Dim ", d[1], " (", round(eig[d[1], 2], 1), "%) vs ",
      "Dim ", d[2], " (", round(eig[d[2], 2], 1), "%)"
    )
    
    # Scores
    scores_tmp <- scores_df %>%
      transmute(
        DimX = .data[[paste0("Dim.", d[1])]],
        DimY = .data[[paste0("Dim.", d[2])]],
        Treatment = .data[[treatment_col]],
        panel = panel_name
      )
    
    scores_list[[i]] <- scores_tmp
    
    # Significant vars
    sig_vars <- union(
      rownames(dimensvars[[paste0("Dim.", d[1])]]$quanti),
      rownames(dimensvars[[paste0("Dim.", d[2])]]$quanti)
    )
    
    arrows_tmp <- var_coords[sig_vars, c(paste0("Dim.", d[1]), paste0("Dim.", d[2]))]
    
    if (nrow(arrows_tmp) == 0) next
    
    arrows_tmp$var <- rownames(arrows_tmp)
    arrows_tmp$panel <- panel_name
    colnames(arrows_tmp)[1:2] <- c("DimX", "DimY")
    
    arrows_tmp <- arrows_tmp %>%
      mutate(
        DimX = DimX * arrow_scale,
        DimY = DimY * arrow_scale
      )
    
    if (!is.null(max_arrows)) {
      arrows_tmp <- arrows_tmp %>%
        mutate(length = sqrt(DimX^2 + DimY^2)) %>%
        slice_max(length, n = max_arrows)
    }
    
    arrows_list[[i]] <- arrows_tmp
  }
  
  scores_all <- bind_rows(scores_list)
  arrows_all <- bind_rows(arrows_list)
  
  # --- 3. Compute convex hulls (FIXED)
  if (hulls) {
    hulls_df <- scores_all %>%
      group_by(panel, Treatment) %>%
      filter(n() >= 3) %>%   # ✅ avoid hull errors
      slice(chull(DimX, DimY)) %>%
      ungroup()
  }
  
  # --- 4. Base plot
  p <- ggplot(scores_all, aes(x = DimX, y = DimY, color = Treatment)) +
    facet_wrap(~panel, scales = "free") +
    theme_bw() +
    xlab("") + ylab("")
  
  # ✅ Add hulls FIRST (so they are behind points)
  if (hulls && exists("hulls_df")) {
    p <- p + geom_polygon(
      data = hulls_df,
      aes(
        x = DimX,
        y = DimY,
        fill = Treatment,
        group = interaction(panel, Treatment)
      ),
      alpha = 0.2,
      color = NA
    )
  }
  
  # Points + arrows
  p <- p +
    geom_point(size = 3) +
    geom_segment(
      data = arrows_all,
      aes(x = 0, y = 0, xend = DimX, yend = DimY),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black",
      inherit.aes = FALSE
    )
  
  # Labels
  if (repel) {
    p <- p + ggrepel::geom_text_repel(
      data = arrows_all,
      aes(x = DimX, y = DimY, label = var),
      inherit.aes = FALSE,
      size = 3,
      segment.color = "grey"
    )
  } else {
    p <- p + geom_text(
      data = arrows_all,
      aes(x = DimX, y = DimY, label = var),
      inherit.aes = FALSE,
      size = 3,
      hjust = 0.5,
      vjust = -0.5
    )
  }
  
  return(p)
}


plot_mfa_arrows(
  mfa_obj = mfa1,
  scores_df = scores_df,
  treatment_col = "Treatment",
  dims = list(c(1, 2), c(3, 4)),
  arrow_scale = 3,
  #max_arrows = 10,
  repel = TRUE
)
ggsave("figures/MFA_2panel.png",height=8,width=12,units="in")


