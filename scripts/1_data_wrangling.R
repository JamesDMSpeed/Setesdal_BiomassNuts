#Opening and formatting biomass, PRS and plant nutrient data
rm(list=ls())

library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)


#Biomass data
biomass_dat<-read_xlsx("data/biomass_2025.xlsx")

#Add a column for plot ID
biomass_dat$PlotID<-paste(biomass_dat$Site_id,biomass_dat$Treatment,biomass_dat$`G/NG`,biomass_dat$Plot,sep="_")
#Add a column forMain plot ID
biomass_dat$MainPlotID<-paste(biomass_dat$Site_id,biomass_dat$Treatment,biomass_dat$`G/NG`,sep="_")

#Add treatmentID
biomass_dat$TreatmentID<-paste(biomass_dat$Treatment,biomass_dat$`G/NG`,sep="_")
biomass_dat$TreatmentID<-factor(biomass_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"))

biomass_dat<-biomass_dat%>%
  mutate(TotalBiomass = Graminoids + Herbs+Dwarf_Shrub)

biomass_dat$Month<-format(as.Date(biomass_dat$Date),"%b")



biomass_long<-pivot_longer(
  biomass_dat,
  cols = c(Litter_Bio, Graminoids, Herbs, Dwarf_Shrub, TotalBiomass),
                names_to = "Fraction",
                values_to = "Biomass")

#Biomass
ggplot(data=biomass_dat,aes(y=Graminoids,x = TreatmentID))+geom_boxplot()+facet_wrap(~Date)+theme_bw()

ggplot(data=biomass_long[biomass_long$Fraction!="TotalBiomass" & biomass_long$Fraction!="Litter_Bio",],aes(x=Date,y=Biomass,fill=Fraction))+
  geom_bar(stat = "summary", fun = "mean")+facet_wrap(~TreatmentID)+theme_bw()

#Biomass proportion regrowth
biomass_regrowth <- biomass_long %>%
    select(PlotID,MainPlotID, TreatmentID,Month, Biomass,Fraction) %>%
    pivot_wider(
    names_from = Month,
    values_from = Biomass
  ) %>%
  mutate(
    August_regrowth = Aug / (Jun+0.0000001)#Add small amount to avoid div0
  )

biomass_wide<-biomass_regrowth %>%
  pivot_wider(values_from = c(Jun,Aug, August_regrowth),
              names_from = Fraction)


ggplot(data=biomass_regrowth[biomass_regrowth$Fraction!="TotalBiomass" & biomass_regrowth$Fraction!="Litter_Bio",]
       ,aes(x=TreatmentID,y=August_regrowth,fill=Fraction))+
  geom_bar(stat="summary",fun="mean",position="dodge")+theme_bw()+
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) 



########################################
#######################################

#Nutrient data
plantnut_dat<-read_xlsx("data/Setesdal-plant-samples-June-2025.xlsx")

#Add a column for plot ID
plantnut_dat$PlotID<-paste(plantnut_dat$Site_id,plantnut_dat$Treatment,plantnut_dat$`G/NG`,plantnut_dat$Plot,sep="_")
#Add a column forMain plot ID
plantnut_dat$MainPlotID<-paste(plantnut_dat$Site_id,plantnut_dat$Treatment,plantnut_dat$`G/NG`,sep="_")

#Add treatmentID
plantnut_dat$TreatmentID<-paste(plantnut_dat$Treatment,plantnut_dat$`G/NG`,sep="_")
plantnut_dat$TreatmentID<-factor(plantnut_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"))
plantnut_dat$CN <- plantnut_dat$`TotC (%)`/plantnut_dat$`TotN (%)`
summary(plantnut_dat)

summary(as.factor(plantnut_dat$Site_id))
summary(as.factor(plantnut_dat$TreatmentID))
summary(as.factor(plantnut_dat$MainPlotID))
summary(as.factor(plantnut_dat$PlotID))

#Some plant nutrient data has multiple rows per plot|fraction where biomass was split between bags
#And also some subplots were pooled when biomass was too low.
summary(as.factor(plantnut_dat$Notes))

#Some vegetation fractions are missing from some plots. We fill these in with NAs
plot_ids <- unique(biomass_dat$PlotID)
fractions <- unique(plantnut_dat$`Plant-species`)
#Valid combinations
template <- tidyr::crossing(
  PlotID = plot_ids,
  `Plant-species` = fractions
)
plantnut_dat_full <- template %>%
  left_join(plantnut_dat, by = c("PlotID", "Plant-species"))

plantnut_long<-pivot_longer(
  plantnut_dat_full,
  cols=c("TotC (%)","TotN (%)", "TotH (%)","CN", "B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
         "Fe (g/kg)" ,"Cu (mg/kg)", "Zn (g/kg)", "Mo (mg/kg)" ),
  names_to="Nutrient",
  values_to="Value")

plantnut_wide<-pivot_wider(
  plantnut_dat,
  id_cols = c(PlotID,Site_id,MainPlotID),
  names_from =`Plant-species`,
  values_from = c("TotC (%)","TotN (%)", "TotH (%)","CN", "B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
                  "Fe (g/kg)" ,"Cu (mg/kg)", "Zn (g/kg)", "Mo (mg/kg)" )
)
dim(plantnut_dat_full)
dim(plantnut_wide)
summary(as.factor(plantnut_wide$PlotID))

#Nutrients
ggplot(data=plantnut_long,aes(y=Value,x=TreatmentID))+geom_boxplot()+facet_wrap(~Nutrient,scales="free_y")+theme_bw()
ggplot(data=plantnut_dat,aes(x = `Ca (g/kg)`, y = `Cu (mg/kg)`,color = TreatmentID))+geom_point()+theme_bw()+scale_x_sqrt()+scale_y_sqrt()



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
prs_dat$TreatmentID<-factor(prs_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"))

prs_dat$PlotID<-paste(prs_dat$site,prs_dat$Treatment,prs_dat$trt_grazing,prs_dat$plot,sep="_")
prs_dat$MainPlotID<-paste(prs_dat$site,prs_dat$Treatment,prs_dat$trt_grazing,sep="_")

#Add PRS to nutrient names to facilitate clarity when joined to plant nutrient data
prs_dat$Nutrient<-paste("PRS",prs_dat$nut,sep="_")

#Make a wide version (15 values of 15 nutrients)
prs_wide<-prs_dat %>%
  pivot_wider(names_from = Nutrient,
              values_from = val,
              id_cols=c("PlotID","TreatmentID"))

#PRS
ggplot(data=prs_dat,aes(y=val,x=TreatmentID))+geom_boxplot()+facet_wrap(~nut,scales="free_y")+theme_bw()+ggtitle("PRS data")
ggplot(data=prs_wide,aes(x=PRS_Ca,y=PRS_Cu,color=TreatmentID))+geom_point()+theme_bw()+scale_x_log10()+scale_y_log10()


########################################
#######################################
#Join together all wide datasets
#Note that PRS data only for a subset of plots

#PlantDataonly
setesdal_plantdat<-biomass_wide %>%
  full_join(plantnut_wide)

ggplot(data=setesdal_plantdat,aes(x=Jun_Herbs,y=`TotN (%)_Herbs`))+geom_point()+scale_x_log10()+scale_y_log10()+theme_bw()+geom_smooth(method="lm")
ggplot(data=setesdal_plantdat,aes(x=CN_Graminoids,y=August_regrowth_Graminoids))+geom_point(data=setesdal_plantdat,aes(color=TreatmentID))+
  scale_x_log10()+scale_y_log10()+geom_smooth(method="lm")+theme_bw()

#Plant data and PRS (only for a subset of plots)
setesdal_prsplant<-setesdal_plantdat %>%
  full_join(prs_wide)
View(setesdal_prsplant)
write.csv(setesdal_prsplant,"data/combined_wide_dataset.csv")

#Averae at "main plot" level
setesdal_avg <- setesdal_prsplant %>%
  group_by(MainPlotID,TreatmentID) %>%
  summarise(
    across(
      where(is.numeric),
      \(x) if(all(is.na(x))) NA else mean(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
write.csv(setesdal_avg,"data/combined_plotnmean_wide_dataset.csv")
