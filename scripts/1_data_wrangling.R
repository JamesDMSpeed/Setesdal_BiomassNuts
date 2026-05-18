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
ggplot(data=biomass_dat,aes(y=Litter_Bio,x = as.factor(Date)))+geom_boxplot()+facet_wrap(~TreatmentID)+theme_bw()

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

biomass_wide<-biomass_regrowth %>%
  pivot_wider(values_from = c(Jun,Aug, August_regrowth),
              names_from = Fraction)

#Main plot average
biomass_wide_plot<-biomass_wide %>% 
  group_by(MainPlotID,TreatmentID) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )



ggplot(data=biomass_regrowth[biomass_regrowth$Fraction!="TotalBiomass" & biomass_regrowth$Fraction!="Litter_Bio",]
       ,aes(x=TreatmentID,y=August_regrowth,fill=Fraction))+
  geom_bar(stat="summary",fun="mean",position="dodge")+theme_bw()+
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0.2,
    position = position_dodge(width = 0.9))+
  geom_hline(yintercept=1,lty=2)    
   



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

# summary(as.factor(plantnut_dat$Notes))
# 
# #All plots
# plot_meta <- biomass_dat %>%
#   distinct(PlotID, TreatmentID)
# 
# # Species levels
# fractions <- unique(plantnut_dat$`Plant-species`)
# 
# # Template
# template <- crossing(
#   plot_meta,
#  Fraction = fractions
# )
# # Join nutrient data
# plantnut_dat_full <- template %>%
#   left_join(plantnut_dat,
#             by = c(
#               "PlotID",
#               "Fraction",
#               "TreatmentID"))
# dim(plantnut_dat_full)#45 plots * 4 fractions = 180




plantnut_long<-pivot_longer(
  plantnut_plotlevel,
  cols=c("TotC (%)","TotN (%)", "TotH (%)","CN", "B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
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
  values_from = c("TotC (%)","TotN (%)", "TotH (%)","CN", "B (mg/kg)", "Na (g/kg)", "Mg (g/kg)",  "Al (g/kg)","P (g/kg)", "S (g/kg)", "K (g/kg)","Ca (g/kg)", "Mn (g/kg)",
                  "Fe (g/kg)" ,"Cu (mg/kg)", "Zn (g/kg)", "Mo (mg/kg)" )
)
dim(plantnut_wide)
summary(as.factor(plantnut_wide$MainPlotID))

#Nutrients
ggplot(data=plantnut_long,aes(y=Value,x=TreatmentID,color=`Plant-species`))+
  geom_boxplot()+
  facet_grid(rows=vars(Nutrient),cols=vars(`Plant-species`),scales="free_y")+
  theme_bw()+theme(axis.text.x = element_text(angle=45,vjust=0.5))
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
prs_dat$TreatmentID<-factor(prs_dat$TreatmentID,levels=c("Mainland_G", "Mainland_NG","Island_NG"))

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
ggplot(data=prs_dat,aes(y=val,x=TreatmentID))+geom_boxplot()+facet_wrap(~nut,scales="free_y",nrow=3)+theme_bw()+ggtitle("PRS data")+theme(axis.text.x = element_text(angle=45,vjust=0.5))
ggplot(data=prs_wide,aes(x=PRS_Ca,y=PRS_Cu,color=TreatmentID))+geom_point()+theme_bw()+scale_x_log10()+scale_y_log10()


########################################
#######################################
#Join together all wide datasets

#Plant data
setesdal_plantdat<-
  full_join(biomass_wide_plot,plantnut_wide,
            by=c("MainPlotID","TreatmentID"))

#Plant data and PRS 
setesdal_prsplant<-setesdal_plantdat %>%
  full_join(prs_wide)
View(setesdal_prsplant)
write.csv(setesdal_prsplant,"data/combined_wide_dataset.csv")

