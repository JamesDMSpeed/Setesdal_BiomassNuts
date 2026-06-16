#Opening and formatting biomass, PRS and plant nutrient data
rm(list=ls())

library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)
library(vegan)


#Biomass data
biomass_dat<-read_xlsx("data/biomass_2025.xlsx")

#Add a column for plot ID
biomass_dat$PlotID<-paste(biomass_dat$Site_id,biomass_dat$Treatment,biomass_dat$`G/NG`,biomass_dat$Plot,sep="_")
#Add a column forMain plot ID
biomass_dat$MainPlotID<-paste(biomass_dat$Site_id,biomass_dat$Treatment,biomass_dat$`G/NG`,sep="_")

#Add treatmentID
biomass_dat$TreatmentID<-paste(biomass_dat$Treatment,biomass_dat$`G/NG`,sep="_")
biomass_dat$TreatmentID<-factor(biomass_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"),
                                labels = c("Grazed", "Exclosure", "Island"))

biomass_dat<-biomass_dat%>%
  mutate(TotalBiomass = Graminoids + Herbs+Dwarf_Shrub)

biomass_dat$Month<-factor(format(as.Date(biomass_dat$Date),"%b"),levels=c("Jun","Aug"))

#Convert from biomass per plot (0.5*0.5m) to m2
biomass_dat$Litter_Bio<-biomass_dat$Litter_Bio*4
biomass_dat$Graminoids<-biomass_dat$Graminoids*4
biomass_dat$Herbs<-biomass_dat$Herbs*4
biomass_dat$Dwarf_Shrub<-biomass_dat$Dwarf_Shrub*4

biomass_long<-pivot_longer(
  biomass_dat,
  cols = c(Litter_Bio, Graminoids, Herbs, Dwarf_Shrub, TotalBiomass),
                names_to = "Fraction",
                values_to = "Biomass")

#Biomass
ggplot(data=biomass_dat,aes(y=Litter_Bio,x = as.factor(Date)))+geom_boxplot()+facet_wrap(~TreatmentID)+theme_bw()
pd <- position_dodge(width = 0.9)

p_bio<-ggplot(data=biomass_long[biomass_long$Fraction!="TotalBiomass" ,],aes(x=TreatmentID,y=Biomass,fill=Fraction,group=Fraction))+
    geom_bar(
    stat = "summary",
    fun = "mean",
    position = pd
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = pd,
    width = 0.2
  )+facet_wrap(~Month)+
  theme_bw()+ylab(expression(Biomass~(g~m^{-2})))+ theme(
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(
      fill = "white",
      colour = "black"
    ))+xlab("")+
    scale_fill_manual(name=NULL,values=c(
                        "Dwarf_Shrub" = "goldenrod",
                      "Graminoids"  = "darkgreen",
                      "Herbs"       = "#e41a1c",
                      "Litter_Bio"  = "#984ea3"),
         labels = c(
          "Dwarf_Shrub" = "Dwarf shrubs",
          "Graminoids"  = "Graminoids",
          "Herbs"       = "Forbs",
          "Litter_Bio"  = "Litter"))
p_bio
ggsave("figures/Biomass.png",height=6,width=8,units="in")  
saveRDS(p_bio,"figures/p_bio.rds")
ggplot(data=biomass_long[biomass_long$Fraction!="TotalBiomass" & biomass_long$Fraction!="Litter_Bio",],aes(x=Date,y=Biomass,fill=Fraction))+
  geom_bar(stat = "summary", fun = "mean")+facet_wrap(~TreatmentID)+theme_bw()+ylab("Biomass (g)")
ggplot(data=biomass_long[biomass_long$Fraction!="TotalBiomass"  ,],aes(x=Date,y=Biomass,fill=Fraction))+
  geom_bar(stat = "summary", fun = "mean")+facet_wrap(~TreatmentID)+theme_bw()



#Biomass proportion regrowth
biomass_regrowth <- biomass_long %>%
    select(PlotID,MainPlotID, TreatmentID,Month, Biomass,Fraction) %>%
    pivot_wider(
    names_from = Month,
    values_from = Biomass
  ) %>%
  mutate(
    August_regrowth = ifelse(Jun == 0 | is.na(Jun),
                             NA,
                             Aug / Jun) #Regrowth is NA if June biomass is 0
  )

names(biomass_regrowth)[5:7]<-c("JunBiomass","AugBiomass","RegrowthBiomass")

biomass_wide<-biomass_regrowth %>%
  pivot_wider(values_from = c(JunBiomass,AugBiomass,RegrowthBiomass),
              names_from = Fraction)

#Main plot average
biomass_wide_plot<-biomass_wide %>% 
  group_by(MainPlotID,TreatmentID) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )



ggplot(data=biomass_regrowth[biomass_regrowth$Fraction!="TotalBiomass" & biomass_regrowth$Fraction!="Litter_Bio",]
       ,aes(x=TreatmentID,y=RegrowthBiomass,fill=Fraction))+
  geom_bar(stat="summary",fun="mean",position="dodge")+theme_bw()+
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0.2,
    position = position_dodge(width = 0.9))+
  geom_hline(yintercept=1,lty=2)    
   



########################################
#Plant point intercept (and diversity) data

plantdat<-read_xlsx('data/pointI_2025.xlsx')
#NAs in species columns are zeros
plantdat <- plantdat %>% 
  mutate(
    across(
      "Agrostis cappilaris":"Litter",
      ~replace(., is.na(.), 0)
    ))
#Treatment and plot ID columns
plantdat<-plantdat %>% 
  mutate( 
    Treatment = if_else(Grazing == 1, "G", "NG"),
    Type = case_when(
      startsWith(Site, "I") ~ "Island",
      startsWith(Site, "M") ~ "Mainland",
      TRUE ~ NA_character_
    ))

#Add a column for plot ID
plantdat$PlotID<-paste(plantdat$Site,plantdat$Type,plantdat$Treatment,plantdat$Plot,sep="_")
#Add a column forMain plot ID
plantdat$MainPlotID<-paste(plantdat$Site,plantdat$Type,plantdat$Treatment,sep="_")

#Add treatmentID
plantdat$TreatmentID<-paste(plantdat$Type,plantdat$Treatment,sep="_")
plantdat$TreatmentID<-factor(plantdat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"),
                             labels = c("Grazed", "Exclosure", "Island"))

#Add species richness and diversity columns
plantdat$species_richness<-specnumber(plantdat[,5:65])
plantdat$shannon<-diversity(plantdat[,5:65],"shannon")

#Filter out empty columns
plantdat <- plantdat %>%
  select(
    where(~ !is.numeric(.) || sum(., na.rm = TRUE) != 0)
  )

plantPI_plotlevel<-plantdat %>%
  group_by(MainPlotID,TreatmentID) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )
dim(plantPI_plotlevel)#15 plots

ggplot(data=plantPI_plotlevel,aes(x=TreatmentID,y=species_richness))+geom_boxplot()+theme_bw()
ggplot(data=plantPI_plotlevel,aes(x=TreatmentID,y=shannon))+geom_boxplot()+theme_bw()

#######################################

#Nutrient data
plantnut_dat<-read_xlsx("data/Setesdal-plant-samples-June-2025.xlsx")

#Add a column for plot ID
plantnut_dat$PlotID<-paste(plantnut_dat$Site_id,plantnut_dat$Treatment,plantnut_dat$`G/NG`,plantnut_dat$Plot,sep="_")
#Add a column forMain plot ID
plantnut_dat$MainPlotID<-paste(plantnut_dat$Site_id,plantnut_dat$Treatment,plantnut_dat$`G/NG`,sep="_")

#Add treatmentID
plantnut_dat$TreatmentID<-paste(plantnut_dat$Treatment,plantnut_dat$`G/NG`,sep="_")
plantnut_dat$TreatmentID<-factor(plantnut_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"),
                                 labels = c("Grazed", "Exclosure", "Island"))

#CN, CP and NP
plantnut_dat$CN <- plantnut_dat$`TotC (%)`/plantnut_dat$`TotN (%)`
plantnut_dat$CP <- plantnut_dat$`TotC (%)`/(plantnut_dat$`P (g/kg)`/10) #scaled to %
plantnut_dat$NP <- plantnut_dat$`TotN (%)`/(plantnut_dat$`P (g/kg)`/10)

summary(plantnut_dat)

summary(as.factor(plantnut_dat$Site_id))
summary(as.factor(plantnut_dat$TreatmentID))
summary(as.factor(plantnut_dat$MainPlotID))
summary(as.factor(plantnut_dat$PlotID))

# #Some plant nutrient data has multiple rows per plot|fraction where biomass was split between bags
# #And also some subplots were pooled when biomass was too low.
#Therefore we average out at site (mainplot ID)

plantnut_plotlevel<-plantnut_dat %>%
  group_by(MainPlotID,TreatmentID,`Plant-species`) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )
dim(plantnut_plotlevel)#15 plots, 4 fractions = 60

plantnut_long<-pivot_longer(
  plantnut_plotlevel,
  cols=c("TotC (%)","TotN (%)", "TotH (%)","CN","CP","NP", "B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
         "Fe (g/kg)" ,"Cu (mg/kg)", "Zn (g/kg)", "Mo (mg/kg)" ),
  names_to="Nutrient",
  values_to="Value")
dim(plantnut_long) #17 nutrients, 15 plots, 4 fractions =1020

summary(plantnut_long$Value)#26 NA values in nutrients
length(plantnut_long$Value[!is.na(plantnut_long$Value)])#994 non NAs

plantnut_wide<-pivot_wider(
  plantnut_plotlevel,
  id_cols = c(MainPlotID,TreatmentID),
  names_from =`Plant-species`,
  values_from = c("TotC (%)","TotN (%)", "TotH (%)","CN","CP", "NP","B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
                  "Fe (g/kg)" ,"Cu (mg/kg)", "Zn (g/kg)", "Mo (mg/kg)" )
)
dim(plantnut_wide)
summary(as.factor(plantnut_wide$MainPlotID))

#Nutrients
p_nuts<-ggplot(data=plantnut_long,aes(y=Value,x=TreatmentID,color=`Plant-species`))+
  geom_boxplot()+ scale_color_manual(name=NULL,values=c(
    "Dwarf_Shrub" = "goldenrod",
    "Graminoids"  = "darkgreen",
    "Herbs"       = "#e41a1c",
    "Litter_Bio"  = "#984ea3"))+
   
  facet_grid(
    rows = vars(Nutrient),
    cols = vars(`Plant-species`),
    labeller = labeller(
      `Plant-species` = c(
        "Graminoids" = "Graminoids",
        "Herbs" = "Forbs",
        "Litter_Bio" = "Litter"
      )
    ),
    scales = "free_y"
  )+
  theme_bw()+theme(axis.text.x = element_text(angle=45,vjust=0.5),
                   strip.text.y = element_text(angle = 0),
                   legend.position = "none")+
  ylab("Nutrient concentration")+xlab("")
p_nuts
ggsave("figures/plantnutconc.png",width=8,height=12,units="in")
saveRDS(p_nuts,"figures/p_nuts.rds")
#ggplot(data=plantnut_long,aes(y=Value,x=Nutrient,color=`Plant-species`))+geom_boxplot()+facet_grid(rows=vars(TreatmentID),cols=vars(`Plant-species`),scales="free_y")+theme_bw()+theme(axis.text.x = element_text(angle=45,vjust=0.5))


ggplot(data=plantnut_plotlevel,aes(x = `Ca (g/kg)`, y = `Cu (mg/kg)`,color = TreatmentID))+geom_point()+theme_bw()+scale_x_sqrt()+scale_y_sqrt()






########################################
#######################################
#PRS probe data - clean version from George
prs_dat<-read.csv("data/data_setesdal_prs.csv")
summary(prs_dat)

#Make treatment and plot ID columns that are consistent with biomass and plant nutrient data
prs_dat<-prs_dat %>%
  mutate(Treatment = case_when(
    island_main == "I" ~ "Island",
    island_main == "M" ~ "Mainland",
    ))

prs_dat$TreatmentID<-paste(prs_dat$Treatment,prs_dat$trt_grazing,sep="_")
prs_dat$TreatmentID<-factor(prs_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"),
                            labels = c("Grazed", "Exclosure", "Island"))

prs_dat$PlotID<-paste(prs_dat$site,prs_dat$Treatment,prs_dat$trt_grazing,prs_dat$plot,sep="_")
prs_dat$MainPlotID<-paste(prs_dat$site,prs_dat$Treatment,prs_dat$trt_grazing,sep="_")

#Add PRS to nutrient names to facilitate clarity when joined to plant nutrient data
prs_dat$Nutrient<-paste("PRS",prs_dat$nut,sep="_")

#Make a wide version (15 values of 15 nutrients)
prs_wide<-prs_dat %>%
  pivot_wider(names_from = Nutrient,
              values_from = val,
              id_cols=c("MainPlotID","PlotID","TreatmentID"))

#PRS
p_prs<-ggplot(data=prs_dat,aes(y=val,x=TreatmentID))+geom_boxplot()+
  ylab(expression("Plant root simulator adsorption ("*mu*"g / 10 cm"^2*")"))+xlab("")+
  facet_wrap(~nut,scales="free_y",ncol=4)+theme_bw()+theme(axis.text.x = element_text(angle=45,vjust=0.5))
p_prs
ggsave("figures/prs.png",width=8,height=6,units="in")
saveRDS(p_prs,"figures/p_prs.rds")
ggplot(data=prs_wide,aes(x=PRS_Ca,y=PRS_Cu,color=TreatmentID))+geom_point()+theme_bw()+scale_x_log10()+scale_y_log10()


########################################
#######################################
#Join together all wide datasets

#Plant data
setesdal_plantbiomcom<-
  full_join(biomass_wide_plot,plantPI_plotlevel,
            by=c("MainPlotID","TreatmentID"))

#Plant and plant nutrients
setesdal_plantdat<-
  full_join(setesdal_plantbiomcom,plantnut_wide,
            by=c("MainPlotID","TreatmentID"))

#Plant data and PRS 
setesdal_prsplant<-setesdal_plantdat %>%
  full_join(prs_wide,
            by = join_by(MainPlotID, TreatmentID))
View(setesdal_prsplant)


#Calculate nutrient pools
#CNH are %
#Others are mg or g/kg
setesdal_prsplant$`Ca (g/kg)_Dwarf_Shrub` * (setesdal_prsplant$JunBiomass_Dwarf_Shrub/1000)

library(stringr)
library(stringr)

biomass_cols <- grep("^JunBiomass_", names(setesdal_prsplant), value = TRUE)

conc_cols <- grep("\\((mg|g)/kg\\)|%", names(setesdal_prsplant), value = TRUE)

for (col in conc_cols) {
  
  frac <- str_extract(col, "Dwarf_Shrub|Graminoids|Herbs|Litter_Bio")
  biomass_col <- paste0("JunBiomass_", frac)
  
  if (!biomass_col %in% names(setesdal_prsplant)) next
  
  # -------------------------
  # % variables
  # -------------------------
  if (str_detect(col, "%")) {
    
    new_name <- str_replace(col, "\\(\\%\\)", "Pool_g_m2")
    
    setesdal_prsplant[[new_name]] <- setesdal_prsplant[[col]] * setesdal_prsplant[[biomass_col]] / 100
  }
  
  # -------------------------
  # mg/kg variables
  # -------------------------
  else if (str_detect(col, "\\(mg/kg\\)")) {
    
    new_name <- str_replace(col, "\\(mg/kg\\)", "Pool_mg_m2")
    
    setesdal_prsplant[[new_name]] <- setesdal_prsplant[[col]] * setesdal_prsplant[[biomass_col]] / 1000
  }
  
  # -------------------------
  # g/kg variables
  # -------------------------
  else if (str_detect(col, "\\(g/kg\\)")) {
    
    new_name <- str_replace(col, "\\(g/kg\\)", "Pool_g_m2")
    
    setesdal_prsplant[[new_name]] <- setesdal_prsplant[[col]] * setesdal_prsplant[[biomass_col]] / 1000
  }
}


#Make a long df for the pools so we can make a stacked barplot

df_sub <- setesdal_prsplant %>%
  select(TreatmentID, MainPlotID, contains("Pool"))

pool_long <- df_sub %>%
  pivot_longer(
    cols = matches("Pool"), 
    names_to = "variable",
    values_to = "value"
  ) %>%
    extract(
    variable,
    into = c("element", "unit", "veg"),
    regex = "^(.+)\\s(Pool_[^_]+_[^_]+)_(.+)$"
    
  )


ggplot(pool_long, aes(x = TreatmentID, y = value, fill = veg)) +
  geom_col(position = "stack") +
  facet_wrap(~ element, scales = "free_y") +
  labs(
    x = "Treatment",
    y = "Pool",
    fill = "Vegetation"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


write.csv(setesdal_prsplant,"data/combined_wide_dataset.csv")

