# # demographics.R
#
# This file creates estimates for demographic info from the child and CAM merged dataset
#
# # Prerequisites:
# Download childcam and samchild files and convert to RDA using this script:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# You also need to load and merge the sample child and child cam files using this script:
# https://github.com/mching/NHIS/blob/master/scripts/load_and_merge_data.R
# 

# The load_and_merge file creates a survey design object called cam.design
# This is replicated here for convenient access 
cam.design <- 
  svydesign(
    id = ~psu_p , 
    strata = ~strat_p ,
    nest = TRUE ,
    weights = ~wtfa_sc ,	# note the change in the weighting variable
    data = x.sa				# note the change in the source data frame
  )

# Number of children
nrow(x.sa)

svytotal(
  ~one ,
  cam.design 
)

# Gender breakdown
cam.design <- update(cam.design, sex_f_ = factor(x.sa$sex, labels = c("male", "female")))

# 
table(x.sa$sex)
prop.table(table(x.sa$sex))

svytotal(~sex_f_, design = cam.design)
confint(svytotal(~sex_f_, design = cam.design))

svymean(~I(sex_f_ == "male"), cam.design)
confint(svymean(~I(sex_f_ == "male"), cam.design))

# Age breakdown
table(x.sa$age_p)
prop.table(table(x.sa$age_p))

svymean(~age_p, cam.design)
svyquantile(~age_p, cam.design, quantiles=c(0.25,0.5,0.75))

# Race
race_names <- c("White only",
                "Black/African American only",
                "AIAN only", 
                "Asian only",
                "Race group not releasable",
                "Multiple race")
cam.design <-update(cam.design,
                    race_ = factor(x.sa$racerpi2, labels = race_names))

table(x.sa$racerpi2)

svytotal(~race_, cam.design)

# Helper function to automate the estimation of proportions in the population
race_extract <- function(race_name) {
  m <- svymean(~I(race_ == race_name), cam.design)
  print(race_name)
  print(m)
  print(confint(m))
}

sapply(race_names, race_extract)

# Hispanic
table(x.sa$hispan_i, useNA = "ifany")
x.sa$hispan_TF <- ifelse(x.sa$hispan==12, "Not Hispanic", "Hispanic")
x.sa$hispan_TF <- factor(x.sa$hispan_TF)
table(x.sa$hispan_TF, useNA = "ifany")
prop.table(table(x.sa$hispan_TF, useNA = "ifany"))

cam.design <- update(cam.design, hispanic_ = x.sa$hispan_TF)

svytotal(~hispanic_, cam.design)
confint(svytotal(~hispanic_, cam.design))

svymean(~I(hispanic_ == "Hispanic"), cam.design)
confint(svymean(~I(hispanic_ == "Hispanic"), cam.design))

# Diagnosis breakdown
# ADHD
table(x.sa$add2, useNA = "if")
x.sa$add2_r <- ifelse(x.sa$add2 > 2, NA, x.sa$add2)
table(x.sa$add2, x.sa$add2_r, useNA = "if")

cam.design <- update(cam.design,
                     ADHD_ = factor(x.sa$add2_r, labels = c("ADHD", "no ADHD")))
svytotal(~ADHD_, cam.design, na.rm = TRUE)

# This does not totally work yet. Object is not found when trying to do the ratios
svy_total_prop_CI <- function(var, true_label, design, ...) {
  print(svytotal(as.formula(paste("~", var)), design, ...))
  print(confint(svytotal(as.formula(paste("~", var)), design, ...)))
  
  print(svymean(~I(as.formula(var) == true_label), design, ...))
  print(confint(svymean(~I(as.formula(var) == true_label), design, ...)))
}
svy_total_prop_CI("ADHD_", "ADHD", cam.design, na.rm = T)
