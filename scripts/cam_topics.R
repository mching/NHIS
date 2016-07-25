# cam_topics.R
# 
# This file identifies the frequency of cam use and use in children with
# specific health needs including developmental delay, autism, etc.

# # Prerequisites:
# Download childcam and samchild files and convert to RDA using this script:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# You also need to load and merge the sample child and child cam files using this script:
# https://github.com/mching/NHIS/blob/master/scripts/load_and_merge_data.R
# 
# You also need to run the demographics script to create some of the recoded disability and other demographic variables
# https://github.com/mching/NHIS/blob/master/scripts/demographics.R

# Chiropractic
# Ever used: cco_use
table(x.sa$cco_use, useNA = "if")
x.sa$cco_use_r <- ifelse(x.sa$cco_use > 2, NA, x.sa$cco_use)
table(x.sa$cco_use_r, x.sa$cco_use, useNA = "if")
table(chiro = x.sa$cco_use_r, asd = x.sa$ccondl6r_r, useNA = "if")
prop.table(table(x.sa$cco_use_r))

cam.design <- update(cam.design, sex_f_ = factor(x.sa$sex, labels = c("male", "female")))

svytotal(~sex_f_, design = cam.design)
confint(svytotal(~sex_f_, design = cam.design))

svymean(~I(sex_f_ == "male"), cam.design)
confint(svymean(~I(sex_f_ == "male"), cam.design))