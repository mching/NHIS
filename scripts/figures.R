library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(scales)

palette_choice = "Greys"
# text_color = "#2D2DB9"
text_color = "black"

fig_data <- read.csv("./scripts/natural_products.csv")
fig_data$condition <- factor(fig_data$condition, levels = c("ASD", "ID", "DD", "ASD, ID, or DD", "US children", "no ASD, ID, or DD"))


################################################################################
# FIGURE 1: NATURAL PRODUCTS
################################################################################

# ggplot(fig_data, aes(x=cam_name, y=prevalence, fill=condition)) + 
#   geom_bar(position=position_dodge(), stat="identity") + 
#   scale_fill_brewer(palette=palette_choice) +
#   geom_errorbar(aes(ymin=lower_ci, ymax=higher_ci), width=.2, # Width of the error bars 
#                 position=position_dodge(.9), color = text_color) +
#   ggtitle("Figure 1: Natural Products") +
#   labs(x = "Therapy", y = "Prevalence") +
#   scale_y_continuous(labels = scales::percent) +
#   theme(text = element_text(family = "Avenir Next", size = 15, color = text_color),
#         axis.text.x = element_text(size = 9, color = text_color),
#         axis.text.y = element_text(color = text_color),
#         axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
#         axis.title.y = element_text(margin = margin(0, 10, 0, 0))
#         )

fig_data <- fig_data %>% filter(condition != c("ASD, ID, or DD")) %>% 
  filter(condition != c("US children")) 
fig_data$cam_name <- factor(fig_data$cam_name, levels = c("multivitamin",
                                                          "specific vitamin",
                                                          "mineral",
                                                          "any herb/supplement"))
ggplot(fig_data, aes(x=cam_name, y=prevalence, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity", color = "black") + 
  scale_fill_brewer(palette=palette_choice) +
  geom_errorbar(aes(ymin=lower_ci, ymax=higher_ci), width=.2, # Width of the error bars 
                position=position_dodge(.9), color = text_color) +
  ggtitle("Figure 1: Natural Products") +
  labs(x = "Therapy", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "Avenir Next", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, color = text_color),
        axis.text.y = element_text(color = text_color),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  )


################################################################################
# FIGURE 2: MIND AND BODY
################################################################################


fig_data <- read.csv("./scripts/mind_and_body.csv")
fig_data$condition <- factor(fig_data$condition, levels = c("ASD", "ID", "DD", "ASD, ID, or DD", "US children"))

# # 95% CI
# ggplot(fig_data, aes(x=cam_name, y=prevalence, fill=condition)) + 
#   geom_bar(position=position_dodge(), stat="identity") + 
#   scale_fill_brewer(palette="Set2") +
#   geom_errorbar(aes(ymin=lower_ci, ymax=higher_ci), width=.2, # Width of the error bars 
#                 position=position_dodge(.9)) +
# #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
#   ggtitle("Figure 3: Mind and Body Therapies") +
#   labs(x = "Therapy", y = "Prevalence") +
#   scale_y_continuous(labels = scales::percent)
# 
# # se
# ggplot(fig_data, aes(x=cam_name, y=prevalence, fill=condition)) + 
#   geom_bar(position=position_dodge(), stat="identity") + 
#   scale_fill_brewer(palette="Set2") +
#   geom_errorbar(aes(ymin=prevalence - se, ymax=prevalence + se), width=.2, # Width of the error bars 
#                 position=position_dodge(.9)) +
#   ggtitle("Figure 3: Mind and Body Therapies") +
#   labs(x = "Therapy", y = "Prevalence") +
#   scale_y_continuous(labels = scales::percent) 
# 
# Just ASD, ID, DD vs US total (Mind & Body)
fig_data <- read.csv("./scripts/mind_and_body.csv")
fig_data$condition <- factor(fig_data$condition, levels = c("ASD", "ID", "DD", "ASD, ID, or DD", "US children", "no ASD, ID, or DD"))

fig_data %>% filter(condition != c("US children")) %>% 
  filter(condition == "ASD, ID, or DD" | condition == "no ASD, ID, or DD") %>% 
  ggplot(aes(x=cam_name, y=prevalence, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity", color = "black") + 
  scale_fill_brewer(palette=palette_choice) +
  geom_errorbar(aes(ymin=lower_ci, ymax=higher_ci), width=.2, # Width of the error bars 
                position=position_dodge(.9), color = text_color) +
  ggtitle("Figure 2: Mind and Body Therapies") +
  labs(x = "Therapy", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "Avenir Next", size = 15, color = text_color),
        axis.text.x = element_text(size = 9, color = text_color),
        axis.text.y = element_text(color = text_color),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  )

################################################################################
# FIGURE 3: NUMBER OF THERAPIES OTHER THAN VITAMINS OR MINERALS
################################################################################
# Number used: cal_cnt

# Update designs
cam.design <- update(cam.design, N_ALT = x.sa$cal_cnt, N_ALT2 = ifelse(x.sa$cal_cnt > 2, 3, x.sa$cal_cnt))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)



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
  n_plot_data$freq_est[i] <- svymean(~N_ALT2 == i, ASD.design)[2]
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
