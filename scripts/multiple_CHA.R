################################################################################
# FIGURE 3: NUMBER OF THERAPIES OTHER THAN VITAMINS OR MINERALS
################################################################################
# Number used: cal_cnt

# Update designs
cam.design <- update(cam.design, N_ALT = x.sa$cal_cnt, 
                     N_ALT2 = ifelse(x.sa$cal_cnt > 2, 3, x.sa$cal_cnt),
                     multiple_CHA = ifelse(x.sa$cal_cnt > 1, 1, 0))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

table(x.sa$cal_cnt, isASD)
table(x.sa$cal_cnt, isDD)
table(x.sa$cal_cnt, isID)
table(x.sa$cal_cnt, isASDDDID)

rm(n_plot_data)
n_plot_data <- data_frame(diagnosis = rep(c("ASD", "ID", "DD", "no DD"), each = 11), 
                          n_therapies = rep(0:10, 4), freq_est = 0, prev_se = 0)

for(i in 0:10) {
  n_plot_data$freq_est[i] <- svytotal(~N_ALT == i, ASD.design)[2]
}
for(i in 0:10) {
  n_plot_data$freq_est[12 + i] <- svytotal(~N_ALT == i, ID.design)[2]
}
for(i in 0:10) {
  n_plot_data$freq_est[23 + i] <- svytotal(~N_ALT == i, DD.design)[2]
}
for(i in 0:10) {
  n_plot_data$freq_est[34 + i] <- svytotal(~N_ALT == i, noASDDDID.design)[2]
}
as.data.frame(n_plot_data)

rm(n_plot_data)
n_plot_data <- data_frame(diagnosis = rep(c("ASD", "ID", "DD", "no DD"), each = 4), 
                          n_therapies = rep(0:3, 4), freq_est = 0, prev_se = 0)
for(i in 0:3) {
  n_plot_data$freq_est[1 + i] <- svytotal(~N_ALT2 == i, ASD.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[5 + i] <- svytotal(~N_ALT2 == i, ID.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[9 + i] <- svytotal(~N_ALT2 == i, DD.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[13 + i] <- svytotal(~N_ALT2 == i, noASDDDID.design)[2]
}

rm(n_plot_data)
n_plot_data <- data_frame(diagnosis = rep(c("ASD", "ID", "DD", "no DD"), each = 4), 
                          n_therapies = rep(0:3, 4), freq_est = 0, prev_se = 0)
for(i in 0:3) {
  n_plot_data$freq_est[1 + i] <- svymean(~N_ALT2 == i, ASD.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[5 + i] <- svymean(~N_ALT2 == i, ID.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[9 + i] <- svymean(~N_ALT2 == i, DD.design)[2]
}
for(i in 0:3) {
  n_plot_data$freq_est[13 + i] <- svymean(~N_ALT2 == i, noASDDDID.design)[2]
}


as.data.frame(n_plot_data)

n_plot_data <-n_plot_data %>% group_by(diagnosis) %>% 
  mutate(group_size = sum(freq_est)) %>% 
  mutate(rate = freq_est/group_size) 

ggplot(n_plot_data, aes(x = n_therapies, y = rate, color = diagnosis)) + 
  geom_point(shape = 0, position=position_jitter(width=.2,height=0)) + 
  scale_y_log10()

sum(n_plot_data$freq_est[25:33])

svytotal(~any_alt, ASD.design)
sv
svytotal(~N_ALT== 2, ASD.design)

svyratio(~N_ALT == 2, ~N_ALT, ASD.design)

svymean(~N_ALT == 0, cam.design)[2][1]


# Total population
table(N_ALT = x.sa$cal_cnt, useNA = "if")
tmp_tab <- svytable(~N_ALT, cam.design)
tbl_df(tmp_tab) %>% mutate(dx = "total", dx_total = svytotal(~N_ALT, cam.design))
prop.table(tmp_tab)
plot(prop.table(tmp_tab))
plot(log(tmp_tab))

# Create bar plot with number of therapies on bottom and frequency on the left
# If you use percentage on the left the numbers will be dwarfed by the 0 category
# If you keep the 0 category in you'll show how many use 0
# If not you will still have a 10-100 fold difference
# Group by diagnosis category.

# ASD
table(N_ALT = x.sa$cal_cnt, useNA = "if")
svytable(~N_ALT, asd.design)
plot(log(svytable(~N_ALT, cam.design)))

# ID
table(N_ALT = x.sa$cal_cnt, id = isID, useNA = "if")
svymean(~N_ALT, ID.design, na.rm = T)
confint(svymean(~N_ALT, ID.design, na.rm = T))

# DD
table(N_ALT = x.sa$cal_cnt, dd = isDD, useNA = "if")
svymean(~N_ALT, DD.design, na.rm = T)
confint(svymean(~N_ALT, DD.design, na.rm = T))

# ASD, ID, DD
table(N_ALT = x.sa$cal_cnt, asdddid = isASDDDID, useNA = "if")
svymean(~N_ALT, ASDDDID.design, na.rm = T)
confint(svymean(~N_ALT, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(N_ALT = x.sa$cal_cnt, asdddid = isASDDDID, useNA = "if")
svymean(~N_ALT, noASDDDID.design, na.rm = T)
confint(svymean(~N_ALT, noASDDDID.design, na.rm = T))

# multiple CHA is defined as x.sa$cal_cnt > 1
table(x.sa$cal_cnt >1, useNA = "if")
table(x.sa$cal_cnt >1, isASD)
table(x.sa$cal_cnt >1, isDD)
table(x.sa$cal_cnt >1, isID)
table(x.sa$cal_cnt >1, isASDDDID)

svymean(~multiple_CHA, cam.design)
confint(svymean(~multiple_CHA, cam.design))

svymean(~multiple_CHA, ASD.design)
confint(svymean(~multiple_CHA, ASD.design))

svymean(~multiple_CHA, DD.design)
confint(svymean(~multiple_CHA, DD.design))

svymean(~multiple_CHA, ID.design)
confint(svymean(~multiple_CHA, ID.design))

svymean(~multiple_CHA, ASDDDID.design)
confint(svymean(~multiple_CHA, ASDDDID.design))

svymean(~multiple_CHA, noASDDDID.design)
confint(svymean(~multiple_CHA, noASDDDID.design))

# Apply helper functions from logistic_regression.R first

x <- models_therapy("multiple_CHA")
lapply(x, summary)
lapply(x, OR_model)

