library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(broom)
library(modelsummary) 
library(sjPlot)
library(showtext)
font_add(family = "noto", regular = "NotoSansDisplay-Regular.ttf")
font_add_google(name = "Noto Sans Display", family = "noto")
showtext_auto()


data <- read.csv("data_assignment_5.csv")
did_model <- lm(positive.attitude ~ treated * pre_post + 
                  log(gdp.pc) + age.elderly + trust.in.gov + gov.efficiency, data = data)
summary(did_model)

# Prediction
pred.data <- expand.grid(
  treated = c(0, 1),   
  pre_post = c(0, 1)  
)
pred.data <- pred.data %>%
  left_join(data %>%
              group_by(pre_post) %>%
              summarise(
                mean_gdp_pc = mean(gdp.pc, na.rm = TRUE),
                mean_age_elderly = mean(age.elderly, na.rm = TRUE),
                mean_trust.in.gov = mean(trust.in.gov, na.rm = TRUE),
                mean_gov.efficiency = mean(gov.efficiency, na.rm = TRUE)
              ),
            by = "pre_post") 
pred.data$gdp.pc <- pred.data$mean_gdp_pc
pred.data$age.elderly <- pred.data$mean_age_elderly
pred.data$trust.in.gov <- pred.data$mean_trust.in.gov
pred.data$gov.efficiency <- pred.data$mean_gov.efficiency
pred.data$attitude_pred <- predict(did_model, newdata = pred.data)

# DiD Visualization
ggplot(pred.data, aes(x = factor(pre_post), y = attitude_pred, group = factor(treated), color = factor(treated))) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3) + 
  labs(title = "Difference of AI Positive Attitude", x = "Time Period", y = "Positive Attitude (%)", color = "treated") +
  scale_x_discrete(breaks = c(0, 1), labels = c("2021", "2024"), expand = expansion(mult = 0.2)) +
  scale_y_continuous(breaks = seq(10, 80, by = 1)) +
  scale_color_manual(labels = c("Control", "Treatment"), values = c("blue", "orange")) +
  theme_minimal() +
  theme(plot.background = element_rect(color = NA, fill = "#F3EEE2"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid.major = element_line(linetype = "solid", color = "grey80"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_blank(), 
        legend.position.inside = c(1, 1),
        legend.justification = c("right", "top"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(size = 15, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
        axis.title.x = element_text(size = 15, hjust = 0.5, margin = margin(t = 15)),
        axis.text.x = element_text(face = "bold", size = 12, margin = margin(r = 10))
  )

# Counterfactual line
control_diff_multi <- pred.data$attitude_pred[pred.data$treated == 0 & pred.data$pre_post == 1] - 
  pred.data$attitude_pred[pred.data$treated == 0 & pred.data$pre_post == 0]

# Predict what the treatment group’s attitude would be without policy impact
pred.data$attitude_pred_counterfactual <- pred.data$attitude_pred
pred.data$attitude_pred_counterfactual[pred.data$treated == 1 & pred.data$pre_post == 1] <- 
  pred.data$attitude_pred[pred.data$treated == 1 & pred.data$pre_post == 0] + control_diff_multi

# Difference between the observed change and the expected change (counterfactual) in the treatment group 
DiD_effect <- pred.data$attitude_pred[pred.data$treated == 1 & pred.data$pre_post == 1] - 
  pred.data$attitude_pred_counterfactual[pred.data$treated == 1 & pred.data$pre_post == 1]

# Counterfactual Visualization
ggplot(pred.data, aes(x = factor(pre_post), y = attitude_pred, group = factor(treated), color = factor(treated))) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3) + 
  geom_line(data = subset(pred.data, treated == 1),
            aes(y = attitude_pred_counterfactual, group = treated, linetype = "Counterfactual"), color = "#999999", linewidth = 1.2) +    # Add predicted counterfactual line for treatment group
  # Add an arrow showing the difference in 2024
  geom_segment(data = subset(pred.data, treated == 1 & pre_post == 1), 
               aes(x = 2, xend = 2, y = attitude_pred_counterfactual, yend = attitude_pred),
               arrow = arrow(length = unit(0.2, "inches")), color = "#d62728", linewidth = 0.8) +
  # Add label for the effect
  geom_text_repel(data = subset(pred.data, treated == 1 & pre_post == 1),
                  aes(x = 2, y = 59.5, 
                      label = paste("Effect =", round(DiD_effect, 2))),
                  color = "#d62728", fontface = "bold", size = 4, hjust = -1) +
  labs(title = "Impact of AI Legislation on Public Attitudes", 
       x = "Time Period", 
       y = "Positive Attitude Towards AI (%)", color = "treated",
       caption = "Data: Special Eurobarometer 2021-2024, World Bank, Eurostat and IMF\nNote: Controlled for demographic, economic, and institutional variables",
       subtitle = "Difference-in-Differences Analysis with Counterfactual Scenario") +
  scale_x_discrete(breaks = c(0, 1), labels = c("2021", "2024"), expand = expansion(mult = 0.2)) +
  scale_y_continuous(breaks = seq(10, 80, by = 1)) +
  scale_color_manual(labels = c("Control", "Treatment"), values = c("#077BE1", "#B23030")) +
  scale_linetype_manual(values = c("Counterfactual" = "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(color = NA, fill = "#E6F4FF"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid.major = element_line(linetype = "solid", color = "grey80"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 1,, size = 10, color = "grey45"), 
        legend.title = element_blank(), 
        legend.position.inside = c(1, 1),
        legend.justification = c("right", "top"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(size = 13, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
        axis.title.x = element_text(size = 13, hjust = 0.5, margin = margin(t = 15)),
        axis.text.x = element_text(face = "bold", size = 12, margin = margin(r = 10))
  )



# Improved version
ggplot(pred.data, aes(x = factor(pre_post), y = attitude_pred, group = factor(treated), color = factor(treated))) +
  geom_line(linewidth = 1) + 
  geom_point(size = 6) + 
  
  # Add predicted counterfactual line for treatment group
  geom_line(data = subset(pred.data, treated == 1),
            aes(y = attitude_pred_counterfactual, group = treated, linetype = "Counterfactual"),
            color = "#EC7E7E", linewidth = 1.2) +
  
  # Dotted bracket showing DiD effect in 2024
  geom_segment(data = subset(pred.data, treated == 1 & pre_post == 1), 
               aes(x = 2.05, xend = 2.05,
                   y = attitude_pred_counterfactual, 
                   yend = attitude_pred),
               color = "#EC7E7E", linetype = "dotted", linewidth = 1) +
  geom_segment(aes(x = 2.01, xend = 2.09,
                   y = attitude_pred_counterfactual, 
                   yend = attitude_pred_counterfactual),
               data = subset(pred.data, treated == 1 & pre_post == 1),
               color = "#EC7E7E", linewidth = 0.8) +
  geom_segment(aes(x = 2.01, xend = 2.09,
                   y = attitude_pred, 
                   yend = attitude_pred),
               data = subset(pred.data, treated == 1 & pre_post == 1),
               color = "#EC7E7E", linewidth = 0.8) +
  
  # Label for the DiD effect
  geom_label(data = subset(pred.data, treated == 1 & pre_post == 1),
             aes(x = 2.11,
                 y = (attitude_pred + attitude_pred_counterfactual) / 2,
                 label = paste("Effect =", round(DiD_effect, 2))),
             fill = "#E6F4FF", color = "#EC7E7E", fontface = "bold", 
             size = 4, hjust = 0.5, label.size = NA) +
  
  # Labels and formatting
  labs(title = "Impact of AI Legislation on Public Attitudes", 
       x = "", 
       y = "Positive Attitude Towards AI (%)", color = "treated",
       caption = "Data: Special Eurobarometer 2021-2024, World Bank, Eurostat and IMF\nNote: Controlled for demographic, economic, and institutional variables",
       subtitle = "Difference-in-Differences Analysis with Counterfactual Scenario") +
  
  scale_x_discrete(breaks = c(0, 1), labels = c("2021", "2024"), expand = expansion(mult = 0.2)) +
  scale_y_continuous(breaks = seq(10, 80, by = 1)) +
  scale_color_manual(labels = c("Control", "Treatment"), values = c("#077BE1", "#B23030")) +
  scale_linetype_manual(values = c("Counterfactual" = "dashed")) +
  
  # Theme and layout
  theme_minimal() +
  theme(plot.background = element_rect(color = NA, fill = "#E6F4FF"),
        plot.margin = margin(t = 30, r = 30, b = 30, l = 30),
    text = element_text(family = "noto"),
    plot.title = element_text(hjust = 0, face = "bold", size = 25, family = "noto", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0, size = 14, family = "noto", margin = margin(b = 25)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey45", family = "noto"), 
    legend.title = element_blank(), 
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    axis.text = element_text(color = "black", family = "noto"),
    axis.title.y = element_text(size = 13, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0), family = "noto"),
    axis.text.y = element_text(face = "bold", size = 12, margin = margin(r = 10), family = "noto"),
    axis.title.x = element_text(size = 13, hjust = 0.5, margin = margin(t = 15), family = "noto"),
    axis.text.x = element_text(face = "bold", size = 12, margin = margin(r = 10), family = "noto")
  )+
  
  # Prevent label clipping
  coord_cartesian(clip = "off")

