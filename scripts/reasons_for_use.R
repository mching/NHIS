# Reason for use
# ctp1rea1-ctp1rea5
# general wellness or general disease prevention?

# Recode
ctp1rea1_r <- recode_yes_no_NA(x.sa$ctp1rea1)
ctp1rea2_r <- recode_yes_no_NA(x.sa$ctp1rea2)
ctp1rea3_r <- recode_yes_no_NA(x.sa$ctp1rea3)
ctp1rea4_r <- recode_yes_no_NA(x.sa$ctp1rea4)
ctp1rea5_r <- recode_yes_no_NA(x.sa$ctp1rea5)

# exploratory tables
dx_cross <- function(outcome) {
  list1 <- list(
    table(outcome           ),
    table(outcome, isASD    ),
    table(outcome, isDD     ),
    table(outcome, isID     ),
    table(outcome, isASDDDID)
    )
  return(list1)
}

# reason for use is general wellness
dx_cross(ctp1rea1_r) 
fisher.test(dx_cross(ctp1rea1_r)[[5]])

# reason for use is energy
dx_cross(ctp1rea2_r) 
fisher.test(dx_cross(ctp1rea2_r)[[5]])

# reason for use is immune function
dx_cross(ctp1rea3_r) 
fisher.test(dx_cross(ctp1rea3_r)[[5]])

# reason for use is athletics
dx_cross(ctp1rea4_r) 
fisher.test(dx_cross(ctp1rea4_r)[[5]])

# reason for use is focus
dx_cross(ctp1rea5_r)
fisher.test(dx_cross(ctp1rea5_r)[[5]])

# Update designs
cam.design <- update(cam.design, reason_well = 2 - ctp1rea1_r,
                     reason_energy = 2 - ctp1rea2_r,
                     reason_immune = 2 - ctp1rea3_r,
                     reason_athletics = 2 - ctp1rea4_r,
                     reason_focus = 2 - ctp1rea5_r)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# reason for use is general wellness
dx_cross(ctp1rea1_r) 
fisher.test(dx_cross(ctp1rea1_r)[[5]])
svymean(~reason_well, cam.design, na.rm = T)
svytable(~isASDDDID_+reason_well, cam.design)
svyby(~reason_well, ~isASDDDID_, cam.design, svymean, na.rm = T)
confint(svyby(~reason_well, ~isASDDDID_, cam.design, svymean, na.rm = T))
# svymean(~reason_well, ASDDDID.design, na.rm = T)  # svyby accomplishes the same thing
# confint(svymean(~reason_well, ASDDDID.design, na.rm = T))

# reason for use is energy
dx_cross(ctp1rea2_r) 
fisher.test(dx_cross(ctp1rea2_r)[[5]])
svychisq(~isASDDDID_+reason_energy, cam.design)
svyby(~reason_energy, ~isASDDDID_, cam.design, svymean, na.rm = T)
confint(svyby(~reason_energy, ~isASDDDID_, cam.design, svymean, na.rm = T))


# reason for use is immune function
dx_cross(ctp1rea3_r) 
fisher.test(dx_cross(ctp1rea3_r)[[5]])
svychisq(~isASDDDID_+reason_immune, cam.design)
svyby(~reason_immune, ~isASDDDID_, cam.design, svymean, na.rm = T)
confint(svyby(~reason_immune, ~isASDDDID_, cam.design, svymean, na.rm = T))


# reason for use is athletics
dx_cross(ctp1rea4_r) 
fisher.test(dx_cross(ctp1rea4_r)[[5]])
svychisq(~isASDDDID_+reason_athletics, cam.design)
svyby(~reason_athletics, ~isASDDDID_, cam.design, svymean, na.rm = T)
confint(svyby(~reason_athletics, ~isASDDDID_, cam.design, svymean, na.rm = T))


# reason for use is focus
dx_cross(ctp1rea5_r)
fisher.test(dx_cross(ctp1rea5_r)[[5]])
svychisq(~isASDDDID_+reason_focus, cam.design)
svyby(~reason_focus, ~isASDDDID_, cam.design, svymean, na.rm = T)
confint(svyby(~reason_focus, ~isASDDDID_, cam.design, svymean, na.rm = T))

############################
# Logistic regressions
x <- models_therapy("reason_well")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("reason_energy")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("reason_immune")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("reason_athletics")
lapply(x, summary)
lapply(x, OR_model)

x <- models_therapy("reason_focus")
lapply(x, summary)
lapply(x, OR_model)
