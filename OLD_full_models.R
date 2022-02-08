
```{r}

herring.basin.data.mod <- read_csv("herring_diet_basin_data_mod.csv") %>% 
  mutate(basin_id = as.factor(basin_id), atlantis_pred_group_id = as.factor(atlantis_pred_group_id))


# examine data
# see https://cran.r-project.org/web/packages/fitdistrplus/vignettes/FAQ.html#can-i-personalize-the-default-plot-given-for-an-object-of-class-fitdist-or-fitdistcens
#https://noamross.github.io/gams-in-r-course/chapter2

#models

#Gamma log link can only be used if there are non-zero values


#used lower K for year and lat,long    
#'gp' gaussian base 

base = "tp"
#gamma 1.4 to reduce overfitting

#the Variance Inflation Factor, which determines multicollinearity found values > 5 (Figure S5) for PDO, Harbour seals, steelhead salmon, pink salmon, Cherry point herring stock, and orcas. As a result, we eliminated these five variables from the final models.

herring.m1 <- gam(proportion_sc ~ s(year, bs='re', k = 20) +
                    s(longitude,latitude, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, bs='re') + 
                    s(predator_size, bs=base, k=20) + 
                    s(basin_id, bs = 're', k = 8) +
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    s(SOG.Herring, bs=base, k=6) +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    s(RR.SSTOut, bs=base, k=6) +
                    s(Chla.JDF, bs=base, k=6) +
                    s(FraserFlow.Out, bs=base, k=6) +
                    s(PNI, bs=base, k=6) +
                    s(NPGO, bs=base, k=6) +
                    s(UPW, bs=base, k=6) +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", gamma=1.4, select=TRUE, data=herring.basin.data.mod)

saveRDS(herring.m1,"herring_gam_m1.rds")

gam.check(herring.m1)

concurvity(herring.m1, full = TRUE)

#herringm.1 suggested that PNI should be parametric (~edf close to 1)

herring.m2 <- gam(proportion_sc ~ s(year, bs=base, k = 20) +
                    s(longitude,latitude, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    s(basin_id, bs = base, k = 8) +
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    s(SOG.Herring, bs=base, k=6) +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    s(RR.SSTOut, bs=base, k=6) +
                    s(Chla.JDF, bs=base, k=6) +
                    s(FraserFlow.Out, bs=base, k=6) +
                    PNI +
                    s(NPGO, bs=base, k=6) +
                    s(UPW, bs=base, k=6) +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m2,"herring_gam_m2.rds")

#UPW,SOG herring, year, basin as parametric (edf > 1 & significance)

herring.m3 <- gam(proportion_sc ~ year +
                    s(longitude,latitude, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    basin_id +
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m3,"herring_gam_m3.rds")



#test year as a by variable, means for constructing ‘varying-coefficient models’ (geographic regression models) and for letting smooths ‘interact’ with factors or parametric terms. They are also the key to specifying general linear functionals of smooths.

herring.m4 <- gam(proportion_sc ~ 
                    s(longitude,latitude, by=year, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=year, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    basin_id +
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m4,"herring_gam_m4.rds")

#test basin as a by variable, means for constructing ‘varying-coefficient models’ (geographic regression models) and for letting smooths ‘interact’ with factors or parametric terms. They are also the key to specifying general linear functionals of smooths.

herring.m5 <- gam(proportion_sc ~
                    year +
                    s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m5,"herring_gam_m5.rds")

# add year by basin
herring.m6 <- gam(proportion_sc ~
                    s(year, by=basin_id, bs= base, k= 15) +
                    s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m6,"herring_gam_m6.rds")

# add year by basin

herring.m7 <- gam(proportion_sc ~
                    s(year, by=basin_id, bs= base, k= 15) +
                    s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                    s(predator_size, bs=base, k=20) + 
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    s(PNI, bs=base, k=6) +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m7,"herring_gam_m7.rds")


herring.m8 <- gam(proportion_sc ~
                    s(longitude,latitude, by=year, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                    s(predator_size, by=basin_id, bs=base, k=20) + 
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m8,"herring_gam_m8.rds")



herring.m9 <- gam(proportion_sc ~
                    s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                    s(atlantis_pred_group_id, by=year, bs=base) + 
                    s(predator_size, by=year, bs=base, k=20) + 
                    #  s(predator_stage_id, bs = 'ts', k = 1) +
                    s(other_herring, bs=base, k=6) +
                    SOG.Herring +
                    s(chinook.ab, bs=base, k=6) +
                    s(chum.ab, bs=base, k=6) +
                    s(coho.ab, bs=base, k=6) +
                    s(sockeye.ab, bs=base, k=6) +
                    RR.SSTOut +
                    s(Chla.JDF, bs=base, k=6) +
                    FraserFlow.Out +
                    PNI +
                    NPGO +
                    UPW +
                    s(MEI, bs=base, k=6),
                  family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m9,"herring_gam_m9.rds")


herring.m10 <- gam(proportion_sc ~
                     s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=year, bs=base) + 
                     s(predator_size, by=year, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     s(chinook.ab, bs=base, k=6) +
                     s(chum.ab, bs=base, k=6) +
                     s(coho.ab, bs=base, k=6) +
                     sockeye.ab +
                     RR.SSTOut +
                     s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m10,"herring_gam_m10.rds")

herring.m10b <- gam(proportion_sc ~
                      s(longitude,latitude, by=atlantis_pred_group_id, bs=base, k = 50) + 
                      s(year, bs=base, k=20) + 
                      s(basin_id, by=atlantis_pred_group_id, bs=base, k=8) + 
                      s(predator_size, bs=base, k=20)+
                      # s(predator_stage_id, bs = 'ts', k = 1) +
                      s(other_herring, bs=base, k=6) +
                      s(SOG.Herring, bs=base, k=6) +
                      s(chinook.ab, bs=base, k=6) +
                      s(chum.ab, bs=base, k=6) +
                      coho.ab +
                      s(sockeye.ab, bs=base, k=6) +
                      s(RR.SSTOut, bs=base, k=6) +
                      s(Chla.JDF, bs=base, k=6) +
                      FraserFlow.Out +
                      s(PNI, bs=base, k=6) +
                      s(NPGO, bs=base, k=6) +
                      s(UPW, bs=base, k=6) +
                      s(MEI, bs=base, k=6),
                    family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)


saveRDS(herring.m10b,"herring_gam_m10b.rds")

anova.gam(herring.m1,herring.m2,herring.m3,herring.m4,herring.m5,herring.m6, herring.m7, herring.m8, herring.m9, herring.m10, test="F")

AIC(herring.m1,herring.m2,herring.m3,herring.m4,herring.m5,herring.m6,herring.m7,herring.m8, herring.m9, herring.m10)

# To examine the presence of parametric coefficients
# If the parametric coefficient was significant, so we modified made terms parametric that were significant and with effective degrees of freedom close to one
#https://www.frontiersin.org/articles/10.3389/fmars.2017.00149/full
#https://www.researchgate.net/profile/Marco_Helbich/publication/268153625_Do_Suburban_Areas_Impact_House_Prices/links/5463267d0cf2c0c6aec1e136/Do-Suburban-Areas-Impact-House-Prices.pdf

#same as model 8, uses ts as a basis 

base <- "ts"

herring.m11 <- gam(proportion_sc ~
                     s(longitude,latitude, by=year, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                     s(predator_size, by=basin_id, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     s(chinook.ab, bs=base, k=6) +
                     s(chum.ab, bs=base, k=6) +
                     s(coho.ab, bs=base, k=6) +
                     s(sockeye.ab, bs=base, k=6) +
                     RR.SSTOut +
                     s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m11,"herring_gam_m11.rds")


#same as m10, uses ts as a basis

base <- "ts"

herring.m12 <- gam(proportion_sc ~
                     s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=year, bs=base) + 
                     s(predator_size, by=year, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     s(chinook.ab, bs=base, k=6) +
                     s(chum.ab, bs=base, k=6) +
                     s(coho.ab, bs=base, k=6) +
                     sockeye.ab +
                     RR.SSTOut +
                     s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m12,"herring_gam_m12.rds")


#same as m12 but sockeye as a parametric term
# https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/

base <- "ts"

herring.m13 <- gam(proportion_sc ~
                     s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=year, bs=base) + 
                     s(predator_size, by=year, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     s(chinook.ab, bs=base, k=6) +
                     chum.ab +
                     s(coho.ab, bs=base, k=6) +
                     sockeye.ab +
                     RR.SSTOut +
                     s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m13,"herring_gam_m13.rds")


#model 13 with terms that are 0 removed iteratively

base <- "ts"

herring.m14 <- gam(proportion_sc ~
                     s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                     #s(atlantis_pred_group_id, by=year, bs=base) + 
                     s(predator_size, by=year, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     #      s(chinook.ab, bs=base, k=6) +
                     chum.ab +
                     s(coho.ab, bs=base, k=6) +
                     sockeye.ab +
                     RR.SSTOut +
                     #   s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m14,"herring_gam_m14.rds")

#same as herring m14 but all terms as smooths

herring.m14b <- gam(proportion_sc ~
                      s(longitude,latitude, by=basin_id, bs=base, k = 100) + 
                      s(atlantis_pred_group_id, by=year, bs=base) + 
                      s(predator_size, by=year, bs=base, k=20) + 
                      #  s(predator_stage_id, bs = 'ts', k = 1) +
                      s(other_herring, bs=base, k=6) +
                      s(SOG.Herring, bs=base, k=6) +
                      #      s(chinook.ab, bs=base, k=6) +
                      s(chum.ab, bs=base, k=6) +
                      s(coho.ab, bs=base, k=6) +
                      s(sockeye.ab, bs=base, k=6) +
                      s(RR.SSTOut, bs=base, k=6) +
                      #   s(Chla.JDF, bs=base, k=6) +
                      s(FraserFlow.Out, bs=base, k=6) +
                      s(PNI, bs=base, k=6) +
                      s(NPGO, bs=base, k=6) +
                      s(UPW, bs=base, k=6) +
                      s(MEI, bs=base, k=6),
                    family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)


#same as herring.m11 but chum as a parametric term

base <- "ts"

herring.m15 <- gam(proportion_sc ~
                     s(longitude,latitude, by=year, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                     s(predator_size, by=basin_id, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     s(chinook.ab, bs=base, k=6) +
                     chum.ab +
                     s(coho.ab, bs=base, k=6) +
                     s(sockeye.ab, bs=base, k=6) +
                     RR.SSTOut +
                     s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW +
                     s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

saveRDS(herring.m15,"herring_gam_m15.rds")

#same as 15 with all smooth terms
herring.m15b <- gam(proportion_sc ~
                      s(longitude,latitude, by=year, bs=base, k = 100) + 
                      s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                      s(predator_size, by=basin_id, bs=base, k=20) + 
                      #  s(predator_stage_id, bs = 'ts', k = 1) +
                      s(other_herring, bs=base, k=6) +
                      s(SOG.Herring, bs=base, k=6) +
                      s(chinook.ab, bs=base, k=6) +
                      s(chum.ab, bs=base, k=6) +
                      s(coho.ab, bs=base, k=6) +
                      s(sockeye.ab, bs=base, k=6) +
                      s(RR.SSTOut, bs=base, k=6) +
                      s(Chla.JDF, bs=base, k=6) +
                      s(FraserFlow.Out, bs=base, k=6) +
                      s(PNI, bs=base, k=6) +
                      s(NPGO, bs=base, k=6) +
                      s(UPW, bs=base, k=6) +
                      s(MEI, bs=base, k=6),
                    family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)

#model 15 with terms that are 0 removed iteratively


base <- "ts"

herring.m16 <- gam(proportion_sc ~
                     s(longitude,latitude, by=year, bs=base, k = 100) + 
                     s(atlantis_pred_group_id, by=basin_id, bs=base) + 
                     s(predator_size, by=basin_id, bs=base, k=20) + 
                     #  s(predator_stage_id, bs = 'ts', k = 1) +
                     s(other_herring, bs=base, k=6) +
                     SOG.Herring +
                     #  s(chinook.ab, bs=base, k=6) +
                     chum.ab +
                     s(coho.ab, bs=base, k=6) +
                     s(sockeye.ab, bs=base, k=6) +
                     RR.SSTOut +
                     #    s(Chla.JDF, bs=base, k=6) +
                     FraserFlow.Out +
                     PNI +
                     NPGO +
                     UPW ,
                   #   s(MEI, bs=base, k=6),
                   family=betar(link="logit"), method="REML", select=TRUE, gamma=1.4, data=herring.basin.data.mod)



saveRDS(herring.m16,"herring_gam_m16.rds")


anova.gam(herring.m1,herring.m2,herring.m3,herring.m4,herring.m5,herring.m6, herring.m7, herring.m8, herring.m9, herring.m10, herring.m11, herring.m12, herring.m13,herring.m14,herring.m15,herring.m16, test="F")

AIC(herring.m1,herring.m2,herring.m3,herring.m4,herring.m5,herring.m6,herring.m7,herring.m8, herring.m9, herring.m10, herring.m11, herring.m12, herring.m13,herring.m14,herring.m15,herring.m16)


#Visuals for final model  

#herring.m14 <- readRDS("herring_gam_m14.rds")

diag.plot <- appraise(herring.m14)

gratia::draw(herring.m14)

ggsave("diagnostic_plot.png",diag.plot,device="png",width=18,height=12,units="cm",dpi=300)

#https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html
herring.viz <- getViz(herring.m14)

print(plot(herring.viz, allTerms = T), pages = 1) # Calls print.plotGam()

#use to plot 2D
#plot(sm(herring.viz, 2)) + l_fitRaster() + l_fitContour() + l_points()

herring.viz.term.1 <- plot(sm(herring.viz, 1)) +
  l_fitRaster() + 
  l_fitContour() + 
  l_points() +
  theme_classic() +
  labs(tag = "A") +
  ggtitle("s(long,lat,2.8):basin")

#use sm to extract smooth terms from model

herring.viz.term.2 <- plot(pterm(herring.viz, 1)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #  l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "B")+
  labs(x="SOG herring", y = "f(SOG herring)")

herring.viz.term.3 <- plot(pterm(herring.viz, 2)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #  l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "C") +
  labs(x="Chum abundance", y = "f(Chum abundance)")


herring.viz.term.4 <- plot(pterm(herring.viz, 4)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "D") +
  labs(x="SST", y = "f(SST)")

herring.viz.term.5 <- plot(pterm(herring.viz, 5)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic()+
  labs(tag = "E") +
  labs(x="Fraser river discharge", y = "f(Fraser river discharge)")

herring.viz.term.6 <- plot(pterm(herring.viz, 7)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(tag = "F") +
  theme_classic()

herring.viz.term.7 <- plot(pterm(herring.viz, 8)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic()+
  labs(tag = "G") +
  labs(x="Annual Upwelling Index", y = "f(Annual Upwelling Index)")

grid.sample <- gridPrint(herring.viz.term.2, herring.viz.term.3,
                         herring.viz.term.4, herring.viz.term.5, ncol=2, nrow=2)

grid.2d <- gridPrint(herring.viz.term.1, ncol=1, nrow=1)

ggsave("all_pred_gam_plot_basin.png", grid.sample, device="png",width=10,height=8, dpi=350)

ggsave("smooth_interaction_plot.png",grid.2d, device="png",width=10,height=8, dpi=350)

#https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html
herring.viz <- getViz(herring.m16)

print(plot(herring.viz, allTerms = T), pages = 1) # Calls print.plotGam()

#use to plot 2D
#plot(sm(herring.viz, 2)) + l_fitRaster() + l_fitContour() + l_points()


herring.viz.term.1 <- plot(sm(herring.viz, 1)) +
  l_fitRaster() + 
  l_fitContour() + 
  l_points() +
  theme_classic() +
  labs(tag = "A") +
  ggtitle("s(long,lat,13.12):basin")

#use sm to extract smooth terms from model

herring.viz.term.2 <- plot(pterm(herring.viz, 1)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #  l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "B")+
  labs(x="SOG herring", y = "f(SOG herring)")

herring.viz.term.3 <- plot(pterm(herring.viz, 2)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #  l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "C") +
  labs(x="Chum abundance", y = "f(Chum abundance)")


herring.viz.term.4 <- plot(pterm(herring.viz, 3)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic() +
  labs(tag = "D") +
  labs(x="SST", y = "f(SST)")

herring.viz.term.5 <- plot(pterm(herring.viz, 4)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic()+
  labs(tag = "E") +
  labs(x="Fraser river discharge", y = "f(Fraser river discharge)")

herring.viz.term.6 <- plot(pterm(herring.viz, 6)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(tag = "F") +
  theme_classic()

herring.viz.term.7 <- plot(pterm(herring.viz, 7)) +
  l_fitLine(colour = "red") + 
  #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  #l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_classic()+
  labs(tag = "G") +
  labs(x="Annual Upwelling Index", y = "f(Annual Upwelling Index)")

grid.sample <- gridPrint(herring.viz.term.1, herring.viz.term.2, herring.viz.term.3,
                         herring.viz.term.4, herring.viz.term.5, herring.viz.term.6, herring.viz.term.7, ncol=3, nrow=3)

ggsave("all_pred_gam_plot_year.png", grid.sample, device="png",width=10,height=8, dpi=350)

# check(herring.viz,
#       a.qq = list(a.cipoly = list(fill = "light blue")), 
#       a.respoi = list(size = 0.5), 
#       a.hist = list(bins = 10))


#Similar methods exist for 2D smooth effect plots, for instance if we fit:
# b <- gam(y ~ s(x1, x2) + x3, data = dat, method = "REML")
# b <- getViz(b)
# 
# plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()

# b <- getViz(b)
# print(plot(b, allTerms = T), pages = 1) # Calls print.plotGam()


#compute semivariogram
herring.min <- gam(proportion_sc ~ s(year, bs=base, k = year.ks) +
                     s(longitude,latitude, bs=base, k = low.k.value), 
                   family=betar(link="logit"), data=herring.basin.data.mod)

data.coords <- herring.basin.data.mod %>% 
  dplyr::select(longitude, latitude) %>% 
  as.matrix()

res.data <- residuals(herring.min) %>% 
  as.matrix()

variog.data <- variog(coords=data.coords,data=res.data)

plot(variog.data)

acf(herring.basin.data.mod$year)
