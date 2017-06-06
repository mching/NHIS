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

