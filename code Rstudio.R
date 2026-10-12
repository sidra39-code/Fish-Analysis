install.packages(c(
  "ggplot2",
  "dplyr",
  "readxl",
  "readr",
  "car",
  "emmeans",
  "multcompView"
))
Treatment <- c("T0","T0","T0",
               "T1","T1","T1",
               "T2","T2","T2")
Weight_Gain <- c(54.6,55.3,54.9,
                 57.9,58.3,57.3,
                 59.5,59.9,60.5)
data <- data.frame(Treatment, Weight_Gain)
aggregate(Weight_Gain ~ Treatment, data = data,
          FUN = function(x) sd(x)/sqrt(length(x)))
# One-way ANOVA
model <- aov(Weight_Gain ~ Treatment, data = data)
summary(model)
TukeyHSD(model)
library(ggplot2)

Treatment <- factor(
  c("T0","T0","T0","T1","T1","T1","T2","T2","T2"),
  levels = c("T0","T1","T2")
)

Weight_Gain <- c(
  54.6,55.3,54.9,
  57.9,58.3,57.3,
  59.5,59.9,60.5
)

data <- data.frame(Treatment, Weight_Gain)

ggplot(data, aes(Treatment, Weight_Gain, fill = Treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.18) +
  geom_jitter(aes(color = Treatment), width = 0.08, size = 3) +
  stat_summary(fun = mean, geom = "text",
               aes(label = round(after_stat(y), 2)),
               vjust = -1.2, color = "black", size = 4) +
  scale_fill_manual(values = c(
    "T0" = "#3498DB",
    "T1" = "#2ECC71",
    "T2" = "#E67E22"
  )) +
  scale_color_manual(values = c(
    "T0" = "#21618C",
    "T1" = "#196F3D",
    "T2" = "#BA4A00"
  )) +
  labs(
    title = "Weight Gain under Different Treatments",
    x = "Treatment",
    y = "Weight Gain"
  ) +
  theme_classic()
anova_result <- summary(model)[[1]]
write.csv(anova_result, "ANOVA_Results.csv")
tukey_result <- TukeyHSD(model)$Treatment
write.csv(tukey_result, "Tukey_Results.csv")

# Length gain data
Treatment <- factor(
  rep(c("T0", "T1", "T2"), each = 3),
  levels = c("T0", "T1", "T2")
)

Length_Gain <- c(
  4.1, 3.8, 4.4,
  4.9, 3.6, 4.2,
  5.1, 3.8, 4.3
)

length_data <- data.frame(Treatment, Length_Gain)

# Mean and Standard Error
aggregate(Length_Gain ~ Treatment, data = length_data,
          FUN = function(x) sd(x) / sqrt(length(x)))

length_model <- aov(Length_Gain ~ Treatment,
                    data = length_data)

summary(length_model)

TukeyHSD(length_model)

library(ggplot2)

p_length <- ggplot(length_data,
                   aes(x = Treatment, y = Length_Gain,
                       fill = Treatment)) +
  stat_summary(fun = mean, geom = "col",
               width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.18, linewidth = 0.8) +
  geom_jitter(aes(color = Treatment),
              width = 0.08, size = 3) +
  scale_fill_manual(values = c(
    "T0" = "#3498DB",
    "T1" = "#2ECC71",
    "T2" = "#E67E22"
  )) +
  scale_color_manual(values = c(
    "T0" = "#21618C",
    "T1" = "#196F3D",
    "T2" = "#BA4A00"
  )) +
  labs(title = "Length Gain under Different Treatments",
       x = "Treatment",
       y = "Length Gain (cm)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        legend.position = "none")

print(p_length)
length_anova <- summary(length_model)[[1]]

write.csv(length_anova,
          "Length_ANOVA.csv")
length_tukey <- TukeyHSD(length_model)$Treatment

write.csv(length_tukey,
          "Tukeys_Length_Analysis.csv")

library(ggplot2)

# SGR data
Treatment <- factor(
  rep(c("T0", "T1", "T2"), each = 3),
  levels = c("T0", "T1", "T2")
)

SGR <- c(
  2.19, 2.09, 2.11,
  2.32, 2.27, 2.23,
  2.41, 2.35, 2.38
)

SGR_data <- data.frame(Treatment, SGR)

# Mean and Standard Error
SGR_summary <- aggregate(
  SGR ~ Treatment,
  data = SGR_data,
  FUN = mean
)

SGR_SE <- aggregate(
  SGR ~ Treatment,
  data = SGR_data,
  FUN = function(x) sd(x) / sqrt(length(x))
)

SGR_summary$SE <- SGR_SE$SGR
print(SGR_summary)

# ANOVA
SGR_model <- aov(SGR ~ Treatment, data = SGR_data)
summary(SGR_model)

# Tukey's pairwise test
Tukeys_SGR <- TukeyHSD(SGR_model)
print(Tukeys_SGR)

# Colourful graph
p_SGR <- ggplot(SGR_data,
                aes(x = Treatment, y = SGR, fill = Treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.18, linewidth = 0.8) +
  geom_jitter(aes(color = Treatment), width = 0.08, size = 3) +
  scale_fill_manual(values = c(
    "T0" = "#3498DB",
    "T1" = "#2ECC71",
    "T2" = "#E67E22"
  )) +
  scale_color_manual(values = c(
    "T0" = "#21618C",
    "T1" = "#196F3D",
    "T2" = "#BA4A00"
  )) +
  labs(title = "SGR (%) under Different Treatments",
       x = "Treatment", y = "SGR (%)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

print(p_SGR)

# Save SGR ANOVA results
SGR_ANOVA <- summary(SGR_model)[[1]]
write.csv(SGR_ANOVA, "SGR_ANOVA_Analysis.csv",
          row.names = TRUE)

# Save SGR Tukey results
SGR_Tukey_Analysis <- as.data.frame(Tukeys_SGR$Treatment)
write.csv(SGR_Tukey_Analysis,
          "SGR_Tukeys_Pairwise_Analysis.csv",
          row.names = TRUE)

# Save colourful graph as PNG
ggsave("SGR_Colourful_Graph.png",
       plot = p_SGR, width = 7, height = 5, dpi = 300)

library(ggplot2)

# FCR data
Treatment <- factor(
  rep(c("T0", "T1", "T2"), each = 3),
  levels = c("T0", "T1", "T2")
)

FCR <- c(
  2.43, 2.38, 2.27,
  2.03, 1.98, 2.07,
  1.4, 2.2, 1.8
)

FCR_data <- data.frame(Treatment, FCR)

# Mean and Standard Error
FCR_summary <- aggregate(
  FCR ~ Treatment,
  data = FCR_data,
  FUN = mean
)

FCR_SE <- aggregate(
  FCR ~ Treatment,
  data = FCR_data,
  FUN = function(x) sd(x) / sqrt(length(x))
)

FCR_summary$SE <- FCR_SE$FCR
print(FCR_summary)

# ANOVA
FCR_model <- aov(FCR ~ Treatment, data = FCR_data)
summary(FCR_model)

# Tukey's pairwise test
Tukeys_FCR <- TukeyHSD(FCR_model)
print(Tukeys_FCR)

# Colourful graph
p_FCR <- ggplot(FCR_data,
                aes(x = Treatment, y = FCR, fill = Treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.18, linewidth = 0.8) +
  geom_jitter(aes(color = Treatment), width = 0.08, size = 3) +
  scale_fill_manual(values = c(
    "T0" = "#3498DB",
    "T1" = "#2ECC71",
    "T2" = "#E67E22"
  )) +
  scale_color_manual(values = c(
    "T0" = "#21618C",
    "T1" = "#196F3D",
    "T2" = "#BA4A00"
  )) +
  labs(title = "FCR under Different Treatments",
       x = "Treatment", y = "FCR") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

print(p_FCR)

# Save FCR ANOVA results
FCR_ANOVA <- summary(FCR_model)[[1]]
write.csv(FCR_ANOVA, "FCR_ANOVA_Analysis.csv",
          row.names = TRUE)

# Save FCR Tukey results
FCR_Tukey_Analysis <- as.data.frame(Tukeys_FCR$Treatment)
write.csv(FCR_Tukey_Analysis,
          "FCR_Tukeys_Pairwise_Analysis.csv",
          row.names = TRUE)

# Save colourful graph
ggsave("FCR_Colourful_Graph.png",
       plot = p_FCR, width = 7, height = 5, dpi = 300)