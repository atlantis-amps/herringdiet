#' @title Generate models per species
#' @description  Functions to generate multiple GAM models
#' @details INPUT: 1) diet composition data
#' @details OUTPUT: 1) models and AICs
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




calc_gams <- function(eachpredator, herringdata){

thispredatordata <- herringdata %>% 
  filter(atlantis_pred_group==eachpredator)

# examine data


est_kvalue <- function(thiskvalue){
  
  year.ks <- thispredatordata %>% 
    distinct(year) %>% 
    pull(year) %>% 
    length(.)-1
  
  print(eachkvalue)
  
  herring.m2 <- gam(proportion_sc ~ s(year, bs="ts", k = year.ks) + s(longitude,latitude, bs="ts", k = thiskvalue), family=betar(link="logit"), data=thispredatordata)
  
  aic.value <- AIC(herring.m2)
  print(aic.value)
  
  result.frame <- tibble(k_value=eachkvalue,AIC=aic.value)
  return(result.frame)
}

kvalues <- seq(10,100, by=1)
k.results <- list()


for(eachkvalue in 1:length(kvalues)){
  
  thiskvalue <- kvalues[eachkvalue]
  
  this.result <- est_kvalue(thiskvalue)
  
  k.results[[eachkvalue]] <- this.result
}

k.values.res <- k.results %>% 
  bind_rows %>% 
  arrange(desc(AIC))

write_csv(k.values.res, paste0(eachpredator,"_kvalues.csv"))

low.k.value <- k.values.res[1,1] %>% pull(k_value)

year.ks <- thispredatordata %>% 
  distinct(year) %>% 
  pull(year) %>% 
  length(.)-1

if(year.ks > 10) year.ks = 10
#models

#Gamma log link can only be used if there are non-zero values

herring.m1 <- gam(proportion_sc ~ s(year, bs="ts", k = year.ks), family=betar(link="logit"),data=thispredatordata)

herring.m2 <- gam(proportion_sc ~ s(year, bs="cr", k = year.ks), family=betar(link="logit"),data=thispredatordata)

herring.m3 <- gam(proportion_sc ~ s(year, bs="tp", k = year.ks) + s(longitude,latitude, bs="ts", k = low.k.value), family=betar(link="logit"),data=thispredatordata)

if((AIC(herring.m12)<AIC(herring.m2)) & (AIC(herring.m1)<AIC(herring.m3))) base="ts"

if((AIC(herring.m2)<AIC(herring.m1)) & (AIC(herring.m2)<AIC(herring.m3))) base="cr"

if((AIC(herring.m3)<AIC(herring.m1)) & (AIC(herring.m3)<AIC(herring.m2))) base="tp"

herring.m4 <- gam(proportion_sc ~ s(year, bs=base, k = year.ks) + s(longitude,latitude, bs=base, k = low.k.value), family=betar(link="logit"), data=thispredatordata)

herring.m5 <- gam(proportion_sc ~ s(year, bs=base, k = year.ks) + s(all_herring, bs=base) + s(longitude,latitude, bs=base, k = low.k.value), family=betar(link="logit"), data=thispredatordata)

herring.m6 <- gam(proportion_sc ~ s(year, bs=base, k = year.ks) + s(all_herring, bs=base) + s(longitude,latitude, bs=base, k = low.k.value) + s(predator_size, bs=base), family=betar(link="logit"), data=thispredatordata)

#te terms produce smooths of multiple predictors from tensor productos of any bases available for use with s
#higher k can result in very complicated shapes, better to leave the default
herring.m7 <- gam(proportion_sc ~ te(year, bs="ts", k = year.ks) + s(all_herring, bs="ts") + s(longitude,latitude, bs="ts", k = low.k.value) + s(predator_size, bs="ts"), family=betar(link="logit"), data=thispredatordata)

# The m= argument allows one to specify different types of covariance functions.
#this model separates herring by stock
herring.m8 <- gam(proportion_sc ~ s(longitude,latitude, bs="ts", k = low.k.value, m=2)+ te(year, bs="ts", k = year.ks)+ s(cherry_point, bs="ts") +  s(other_herring, bs="ts") + s(predator_size, bs="ts"), family=betar(link="logit"),
                  data=thispredatordata)

#include basin as a random term
herring.m9 <- gam(proportion_sc ~ s(longitude,latitude, bs="tp", k = low.k.value)+ te(year, k = year.ks)+ s(cherry_point, bs="ts") +  s(other_herring, bs="ts") +  s(predator_size, bs="tp") + s(basin_id, bs = 're'), family=betar(link="logit"),
                  data=thispredatordata)

#add predator stage as a smoother
herring.m10 <- gam(proportion_sc ~ s(longitude,latitude, bs="tp", k = low.k.value)+ te(year, k = year.ks)+ s(cherry_point, bs="ts") +  s(other_herring, bs="ts") +  s(predator_size, bs="tp") + s(basin_id, bs = 're') + s(predator_stage_id, bs = 're'), family=betar(link="logit"),
                   data=thispredatordata)


#add predator stage as a smoother
herring.m11 <- gam(proportion_sc ~ s(longitude,latitude, bs="tp", k = low.k.value)+ te(year, k = year.ks)+ s(cherry_point, bs="ts") +  s(other_herring, bs="ts") +  s(predator_size, bs="tp") + s(basin_id, bs = 're') + s(predator_stage_id, bs = 're'), family=betar(link="logit"),
                   data=thispredatordata)

herring.m12 <- gam(proportion_sc ~ s(longitude,latitude, bs="tp", k = low.k.value)+ te(year, k = year.ks)+ s(all_herring, bs="ts") +  s(predator_size, bs="tp") + s(basin_id, bs = 're') + s(predator_stage_id, bs = 're'), family=betar(link="logit"),
                   data=thispredatordata)


herring.m13 <- gam(proportion_sc ~ s(longitude,latitude, bs="tp", k = low.k.value)+ te(year, k = year.ks)+ s(all_herring, bs="ts") +  s(predator_size, bs="tp") + s(basin_id, bs = 're'), family=betar(link="logit"),
                   data=thispredatordata)


AIC.summary <- c(AIC(herring.m1),AIC(herring.m2),AIC(herring.m3),AIC(herring.m4),AIC(herring.m5),AIC(herring.m6),AIC(herring.m7),
                 AIC(herring.m8),AIC(herring.m9),AIC(herring.m10),AIC(herring.m11),AIC(herring.m12),AIC(herring.m13)) %>% 
  as_tibble %>% 
  mutate(model=1:13) %>% 
  arrange(desc(value))

write_csv(AIC.summary, paste0(eachpredator,"_AIC_model_summary.csv"))

}