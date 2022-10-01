#' @title Generate GAM table
#' @description  Functions to make tible of GAM fits
#' @details INPUT: 1) GAM
#' @details OUTPUT: 1) table of GAM outputs
#' @details Uses function glance from package broom
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

gam_table <- function(this.no.fit, list.fits){
  
  this.fit <- list.fits[[this.no.fit]]
  
  print("summarising fit")
  print(this.fit)
  summary.fit <- summary(this.fit)
  
  REML <- summary.fit$sp.criterion
  R2 <- summary.fit$r.sq
  df <- glance(this.fit)$df
  AIC_val <- glance(this.fit)$AIC
  deviance <- (summary.fit$dev.expl)*100
  model.formula <- as.character(this.fit$formula)
  
  
  gam_tibble <- tibble(formula=model.formula, model_no=this.no.fit, REML=round(REML,1),R2=round(R2,3),df=round(df,1),
                       AIC=AIC_val,deviance_explained=round(deviance,1)) 
  
  return(gam_tibble)
}