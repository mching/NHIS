models_therapy <- function(therapy) {
  # calculates a glm model for each of the diganoses
  formula1 <- paste0(therapy, "~isASD_")
  formula2 <- paste0(therapy, "~isID_")
  formula3 <- paste0(therapy, "~isDD_")
  formula4 <- paste0(therapy, "~isASDDDID_")
  asd_model <- svyglm(as.formula(formula1), design = cam.design, family = quasibinomial)
  id_model <- svyglm(as.formula(formula2), design = cam.design, family = quasibinomial)
  dd_model <- svyglm(as.formula(formula3), design = cam.design, family = quasibinomial)
  asdddid_model <- svyglm(as.formula(formula4), design = cam.design, 
                          family = quasibinomial)
  return(list(asd = asd_model, id = id_model, dd = dd_model, asdddid = asdddid_model))
  # extract coefficient
  # return OR and 95% CI
}

OR_model <- function(model1) {
  # calculates the odds ratio and confidence interval given a glm model
  exp(cbind(OR = coef(model1), confint(model1)))
}

x <- models_therapy("MVI")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("ABCDE")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("MINERAL")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("ANYHERB")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("any_alt")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("chiro")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("MASSAGE")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("MEDITATION")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("YOGA")
lapply(x, summary)
lapply(x, OR_model)


#######
