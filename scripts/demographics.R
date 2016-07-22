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
svytotal(
  ~one ,
  cam.design 
)

# Gender breakdown
x.sa$sex <- factor(x.sa$sex, labels = c("male", "female"))
table(x.sa$sex)
prop.table(table(x.sa$sex))

svytotal(~sex, design = cam.design)
confint(svytotal(~sex, design = cam.design))

svytotal(~sex, design = cam.design)/sum(svytotal(~sex, design = cam.design))
confint(svytotal(~sex, design = cam.design))/sum(svytotal(~sex, design = cam.design))

# Age breakdown
table(x.sa$age_p)
prop.table(table(x.sa$age_p))

# Race
x.sa$racerpi2 <-factor(x.sa$racerpi2, labels = c("White only",
                                                 "Black/African American only",
                                                 "AIAN only", 
                                                 "Asian only",
                                                 "Race group not releasable",
                                                 "Multiple race"))
table(x.sa$racerpi2)

svytotal(~racerpi2)

# Hispanic

# Diagnosis breakdown