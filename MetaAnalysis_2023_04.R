### This code examines several statistical tests in loops, saves the output, and performs meta-analyses on the resulting test statistics.
### The data is obtained through identical replication studies in English and Dutch.

### Libraries
library(ggpubr)
library(ggplot2)
library(gplots)
library(psych)
library(tidyverse)
library(rstatix)
library(apaTables)
library(ez)
library(gtsummary)
library(car)
library(metafor)
library(plyr)
library(effsize)


# Setting the directory
curdir <- getwd()
setwd(curdir)


####  Data Processing
### Reading the data
full_df <- data.frame()
n_groups <- 65

for (i in 1:n_groups){
  file_name <- paste('./group',i,'.csv',sep = "")
  data <- read.csv(file_name, header = T)
  data$Experimental.Group <- i
  if(i == 1) {
    full_df <- data
  } else {
    common_cols <- intersect(colnames(data), colnames(full_df))
    full_df <- rbind(full_df[common_cols], data[common_cols])
  }
}
full_df <- full_df[, c("Experimental.Group", setdiff(names(full_df), "Experimental.Group"))]


### Data cleaning
idf <- subset(full_df, select=c("Experimental.Group", "ParticipantID", "Age", "Gender",
                                "Language", "Animate..average.interaction.rating.", "Animate..SD.",
                                "Inanimate..average.interaction.rating.", "Animate..SD.", "Filler.score",
                                "Total.number.of.words.recalled", "Number.of.Animate.words.recalled",
                                "Number.of.Inanimate.words.recalled", "Number.of.false.recalls",
                                "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12",
                                "I13", "I14", "I15", "I16", "I17","I18","I19", "I20", "I21", "I22", "I23","I24",
                                "W1..owl.", "W2..bee.", "W3..minister.", "W4..baby.", "W5..soldier.",
                                "W6..python.", "W7..wolf.", "W8..engineer.", "W9..trout.",
                                "W10..turtle.", "W11..spider.", "W12..duck.", "W13..doll.",
                                "W14..drum.", "W15..purse.", "W16..violin.", "W17..slippers.",
                                "W18..stove.", "W19..rake.", "W20..journal.", "W21..whistle.",
                                "W22..tent.", "W23..hat.", "W24..kite."))

### Missing data
sum(is.na(idf))


### Removing Outliers
quartiles <- quantile(idf$Number.of.Animate.words.recalled, probs=c(.25, .75), na.rm = F)
IQR <- IQR(idf$Number.of.Animate.words.recalled)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
wdf <- subset(idf, idf$Number.of.Animate.words.recalled > Lower & idf$Number.of.Animate.words.recalled < Upper)
dim(wdf)

quartiles <- quantile(wdf$Number.of.Inanimate.words.recalled, probs=c(.25, .75), na.rm = F)
IQR <- IQR(wdf$Number.of.Inanimate.words.recalled)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
wdf <- subset(wdf, wdf$Number.of.Inanimate.words.recalled > Lower & wdf$Number.of.Inanimate.words.recalled < Upper)
dim(wdf)





### Assumptions
Experimental.Group <- factor(wdf$Experimental.Group)

# Normality
wdf %>%
  group_by(Experimental.Group) %>%
  shapiro_test(Number.of.Animate.words.recalled)
#ggqqplot(wdf, "Number.of.Animate.words.recalled", facet.by = "Experimental.Group",
#         main = "Figure A. qq-plot of Animate Words Recalled (grouped by studies)" )

wdf %>%
  group_by(Experimental.Group) %>%
shapiro_test(Number.of.Inanimate.words.recalled)
#ggqqplot(wdf, "Number.of.Inanimate.words.recalled", facet.by = "Experimental.Group",
#         main = "Figure A. qq-plot of Inanimate Words Recalled (grouped by studies)" )

# Homoscedasticity
bartlett.test(Number.of.Animate.words.recalled ~ Experimental.Group, data = wdf)
bartlett.test(Number.of.Inanimate.words.recalled ~ Experimental.Group, data = wdf)

fligner.test(Number.of.Animate.words.recalled ~ Experimental.Group, data = wdf)
fligner.test(Number.of.Inanimate.words.recalled ~ Experimental.Group, data = wdf)


### Descriptives
ddf <- subset(wdf, select=c("Age", "Gender", "Language",
                            "Animate..average.interaction.rating.",
                            "Inanimate..average.interaction.rating.",
                            "Total.number.of.words.recalled", "Number.of.Animate.words.recalled",
                            "Number.of.Inanimate.words.recalled", "Number.of.false.recalls"))

describe(ddf)
table(ddf$Gender)
table(ddf$Language)



#### 1) Animacy Effect

### Visualizaion


wdf$Experimental.Group <- factor(wdf$Experimental.Group, levels = unique(wdf$Experimental.Group))

# Dim the colors for the plots
light_blue <- t_col("steelblue", percent = 50, name = "lightblue")
light_green <- t_col("seagreen", percent = 50, name = "lightgreen")


recall_proportions_plot <- ggplot(wdf, aes(x = Experimental.Group)) +
  geom_bar(aes(y = Number.of.Animate.words.recalled), stat = "identity",
           fill = light_green, alpha = 0.5) +
  geom_bar(aes(y = Number.of.Inanimate.words.recalled), stat = "identity",
           fill = light_blue, alpha = 0.5) +
  labs(title = "A. Histogram of Average Word Recall Frequency",
       x = "Animate (Green) vs. Inanimate (Blue)",
       y = "Recall Frequency") +
  theme_minimal() +
  scale_x_discrete(labels = NULL)

average_ratings_plot <- ggplot(wdf, aes(x = Experimental.Group)) +
  geom_bar(aes(y = Animate..average.interaction.rating./80), stat = "identity",
           fill = light_green, alpha = 0.5) +
  geom_bar(aes(y = Inanimate..average.interaction.rating./70), stat = "identity",
           fill = light_blue, alpha = 0.5) +
  labs(title = "B. Histogram of Average Ease of Interaction Ratings",
       x = "Animate (Green) vs. Inanimate (Blue)",
       y = "Average Interaction Rating") +
  theme_minimal() +
  scale_x_discrete(labels = NULL)

combined_plot <- gridExtra::grid.arrange(recall_proportions_plot,
                                         average_ratings_plot, nrow = 1)
combined_plot


# Calculate ParticipantID count per Experimental.Group
participant_counts <- wdf %>%
  group_by(Experimental.Group) %>%
  summarise(participant_count = n_distinct(ParticipantID))



### T-test
t_statistics <- t.test(wdf$Number.of.Animate.words.recalled, wdf$Number.of.Inanimate.words.recalled,
                       mu=0, alt="two.sided", paired=T, conf.level=0.99)
t_statistics


####  GROUP analysis I

# T-test Loop
df_smd <- data.frame()
T_df_A <- data.frame()

for (i in 1:n_groups){
  file_name <- paste('./group',i,'.csv',sep = "")
  data <- read.csv(file_name, header = T)
  t_value <- t.test(data$Number.of.Animate.words.recalled,
                    data$Number.of.Inanimate.words.recalled,
                    mu=0, alt="two.sided", paired=T, conf.level=0.99)
  N <- nrow(data)
  
  # Calculate yi, vi, sei, and weights
  yi <- t_value$statistic / sqrt(N)
  sei <- sd(c(data$Number.of.Animate.words.recalled,
              data$Number.of.Inanimate.words.recalled)) / sqrt(N)
  vi <- sei^2
  weights <- 1 / vi
  
  df_smd <- data.frame(
    npp = N,
    p_value = t_value$p.value,
    conf_up = t_value$conf.int[2],
    conf_low = t_value$conf.int[1],
    stderr = t_value$stderr,
    t_statistics = t_value$statistic,
    yi = yi,
    vi = vi,
    sei = sei,
    weights = weights
  )
  
  T_df_A <- rbind(T_df_A, df_smd)
}

write.csv(T_df_A, "T_table_A.csv", row.names=F)



#####  META-analysis I

# Random Effects
REML_model <- rma(yi, vi, method="EE", data=T_df_A)
summary(REML_model)


# Forest plot
T_df_A$t_statistics <- round(T_df_A$t_statistics, 2)
par(mfrow = c(1, 2))
figure2 <- forest(REML_model, main = "A. Forest Plot of the Animacy Effect Meta-Analysis",
                  header = "Experiments and Corresponding t-Statistics                                                                                                                Weights",
                  xlim = c(-1.3, 3.5),
                  refline = coef(REML_model), cex = .6, psize = 1,       
                  showweights = T,
                  addpred = T,
                  slab = paste0("Study ", 1:65, ": t(", npp-1, ") = ", T_df_A$t_statistics),
                  order = "obs",
                  xlab = "Standardized Mean Differences (Cohen's d)")
figure3 <- funnel(REML_model, main = "B. Funnel Plot of the Animacy Effect Meta-Analysis", header = "Experiment",
                  refline = coef(REML_model), level = c(90, 95, 99),
                  shade = c("white", "gray65", "gray25"), lty2 = 1, label = TRUE,
                  yaxis = "sei", legend = T, refline2 = coef(REML_model))


T_df_I$t_statistics <- round(T_df_I$t_statistics, 2)
par(mfrow = c(1, 2))



#### 2) Interactive Imagery Effect

### T-test
t_statistics <- t.test(wdf$Animate..average.interaction.rating.
                       ,wdf$Inanimate..average.interaction.rating.,
                       mu=0, alt="two.sided", paired=T, conf.level=0.99)
t_statistics


####  GROUP analysis II


# T-test Loop
df_smd <- data.frame()
T_df_I <- data.frame()

for (i in 1:n_groups){
  file_name <- paste('./group',i,'.csv',sep = "")
  data <- read.csv(file_name, header = T)
  t_value <- t.test(data$Animate..average.interaction.rating.,
                    data$Inanimate..average.interaction.rating.,
                    mu=0, alt="two.sided", paired=T, conf.level=0.99)
  N <- nrow(data)
  
  yi <- t_value$statistic / sqrt(N)
  sei <- sd(c(data$Number.of.Animate.words.recalled,
              data$Number.of.Inanimate.words.recalled)) / sqrt(N)
  
  # check if sei is zero or infinite
  if (sei == 0 || is.infinite(sei)) {
    vi <- NA
    weights <- NA
  } else {
    vi <- sei^2
    weights <- 1 / vi
  }
  
  df_smd <- data.frame(
    npp = N,
    p_value = t_value$p.value,
    conf_up = t_value$conf.int[2],
    conf_low = t_value$conf.int[1],
    stderr = t_value$stderr,
    t_statistics = t_value$statistic,
    yi = yi,
    vi = vi,
    sei = sei,
    weights = weights
  )
  
  T_df_I <- rbind(T_df_I, df_smd)
}

write.csv(T_df_I, "T_table_I.csv", row.names=F)




####  META-analysis II

# Random Effects
REML_model <- rma(yi, vi, method="REML", data=T_df_I)
summary(REML_model)


# Forest plot
T_df_I$t_statistics <- round(T_df_I$t_statistics, 2)
par(mfrow = c(1, 2))
figure2 <- forest(REML_model, main = "A. Forest Plot of the Ease of Interaction Meta-Analysis",
                  header="Experiments and Corresponding t-Statistics                                                                                                                Weigts",
                  xlim = c(-3, 2),
                  refline = coef(REML_model), cex=.6, psize=1,       
                  showweights = T,
                  addpred = T,
                  slab=paste0("Study ", 1:65,": t(", npp-1, ") = ", T_df_I$t_statistics),
                  order="obs",
                  xlab = "Standardized Mean Differences (Cohen's d)")

### Funnel plot
figure3 <- funnel(REML_model, main = "B. Funnel Plot of the Ease of Interaction Meta-Analysis", header="Experiment",
                  refline = coef(REML_model), level=c(90, 95, 99),
                  shade=c("white", "gray65", "gray25"),lty2 = 1, label = T,
                  yaxis="sei", legend = T, refline2 = coef(REML_model))






#### 3) Correlation (Ease of Interaction and Remembering)

### data frame
rdf <- subset(wdf, select=c("Experimental.Group", "ParticipantID", "I1", "I2", "I3",
                            "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12","I13", "I14",
                            "I15", "I16", "I17","I18","I19", "I20", "I21", "I22", "I23","I24",
                            "W1..owl.", "W2..bee.", "W3..minister.", "W4..baby.", "W5..soldier.",
                            "W6..python.", "W7..wolf.", "W8..engineer.", "W9..trout.",
                            "W10..turtle.", "W11..spider.", "W12..duck.", "W13..doll.",
                            "W14..drum.", "W15..purse.", "W16..violin.", "W17..slippers.",
                            "W18..stove.", "W19..rake.", "W20..journal.", "W21..whistle.",
                            "W22..tent.", "W23..hat.", "W24..kite."))

rdf = rdf[complete.cases(rdf), ]


# Calculate average ratings for variables I1 to I24
average_ratings <- colMeans(rdf[, grep("^I\\d+$", names(rdf))])

# Calculate proportions of participants recalling each word stimulus
recall_proportions <- colMeans(rdf[, grep("^W\\d+\\..+", names(rdf))])

# Perform correlation test
cor.test(average_ratings, recall_proportions)



correlation_results <- list()

# Iterate through each study
for (group in unique(rdf$Experimental.Group)) {
  group_data <- subset(rdf, Experimental.Group == group)
  
  # Calculate average ratings for variables I1 to I24
  average_ratings <- colMeans(group_data[, grep("^I\\d+$", names(group_data))])
  
  # Calculate proportions 
  recall_proportions <- colMeans(group_data[, grep("^W\\d+\\..+", names(group_data))])
  
  # Calculate correlation 
  correlation <- cor(average_ratings, recall_proportions)
  p_value <- cor.test(average_ratings, recall_proportions)$p.value
  yi <- correlation
  sei <- sqrt((1 - yi^2) / (length(group_data$ParticipantID)-2))
  weights <- yi / sei
  
  correlation_results[[as.character(group)]] <- c(correlation, p_value, yi, vi, sei, weights)
}

# Combine correlation results into a data frame
C_table <- data.frame(
  Experimental.Group = names(correlation_results),
  Correlation = sapply(correlation_results, "[[", 1),
  P_value = sapply(correlation_results, "[[", 2),
  yi = sapply(correlation_results, "[[", 3),
  vi = sapply(correlation_results, "[[", 4),
  sei = sapply(correlation_results, "[[", 5),
  weights = sapply(correlation_results, "[[", 6),
  stringsAsFactors = FALSE
)

write.csv(C_table, "C_table.csv", row.names = FALSE)




####  META-analysis III

### Effect size data frame
C_table =read.csv('C_table.csv')

# Random Effects
REML_model <- rma(yi, sei, method="REML", data=C_table)
summary(REML_model)


# Forest plot
C_table$Correlation <- round(C_table$Correlation, 2)
par(mfrow = c(1, 2))
figure2 <- forest(REML_model, main = "A. Forest Plot of the Correlation Effect Meta-Analysis",
                  header="Experiments and Corresponding Correlation Coeeficients                                                                                      Weigts",
                  xlim = c(-2.5, 2.8),
                  refline = coef(REML_model), cex=.6, psize=1,       
                  showweights = T,
                  addpred = T,
                  slab=paste0("Study ", C_table$Experimental.Group, ": r(22) = ", C_table$Correlation),
                  order="obs",
                  xlab = "Correlation Coeeficients (Pearson's R)")

### Funnel plot
figure3 <- funnel(REML_model, main = "B. Funnel Plot of the Correlation Effect Meta-Analysis", header="Experiment",
                  refline = coef(REML_model), level=c(90, 95, 99),
                  shade=c("white", "gray65", "gray25"),lty2 = 1, label = T,
                  yaxis="sei", legend = T, refline2 = coef(REML_model))




#### 4) Language

# 1st Language
t_lang1 <- t.test(wdf$Number.of.Animate.words.recalled[wdf$Language == 1], 
                      wdf$Number.of.Inanimate.words.recalled[wdf$Language == 1],
                      mu=0, alt="two.sided", paired=T, conf.level=0.99)

# 2nd Language
t_lang2 <- t.test(wdf$Number.of.Animate.words.recalled[wdf$Language == 2], 
                      wdf$Number.of.Inanimate.words.recalled[wdf$Language == 2],
                      mu=0, alt="two.sided", paired=T, conf.level=0.99)

t_lang1
t_lang2



#### Histogram2


# Standardize the values of I1 to I24 over participants
standardized_I <- lapply(wdf[, grep("^I\\d+$", names(wdf))], function(x) scale(x, center = TRUE, scale = TRUE))

# Scale the standardized values of I1 to I24 to a range of 0 to 1
scaled_I <- lapply(standardized_I, function(x) (x - min(x)) / (max(x) - min(x)))

# Calculate average ratings for scaled variables I1 to I12 and I13 to I24
average_ratingsA <- colMeans(do.call(cbind, scaled_I[1:12]))
average_ratingsB <- colMeans(do.call(cbind, scaled_I[13:24]))

# Calculate proportions of participants recalling each word stimulus for W1 to W12
recall_proportionsA <- colMeans(wdf[, grep("^W\\d+\\..+", names(wdf))[1:12]])
recall_proportionsB <- colMeans(wdf[, grep("^W\\d+\\..+", names(wdf))[13:24]])

# Modelling Data Frame
mdf <- data.frame(W = paste0("w", 1:12),
                  average_ratingsA, average_ratingsB,
                  recall_proportionsA, recall_proportionsB)
mdf$W <- as.character(mdf$W)

# Dim the colors for the plots
light_yellow <- t_col("yellow3", percent = 50, name = "yellow")
light_violet <- t_col("darkviolet", percent = 50, name = "violet")



# Plot
mdf$W <- factor(mdf$W, levels = unique(mdf$W))


# Create a bar plot for recall proportions with dimmed colors
recall_proportions_plot <- ggplot(mdf, aes(x = W)) +
  geom_bar(aes(y = average_ratingsA), stat = "identity",
           fill = light_violet, alpha = 0.5) +
  geom_bar(aes(y = recall_proportionsA), stat = "identity",
           fill = light_yellow, alpha = 0.5) +
  labs(title = "A. Animate Recall and Ease of Interaction",
       x = "Animate Words",
       y = "Ease of Interaction (Violet) vs. Recall (Yellow)") +
  theme_minimal()

# Create a bar plot for average ratings with dimmed colors
average_ratings_plot <- ggplot(mdf, aes(x = W)) +
  geom_bar(aes(y = average_ratingsB), stat = "identity",
           fill = light_violet, alpha = 0.5) +
  geom_bar(aes(y = recall_proportionsB), stat = "identity",
           fill = light_yellow, alpha = 0.5) +
  labs(title = "B. Inanimate Recall and Ease of Interaction",
       x = "Inanimate Words",
       y = "Ease of Interaction (Violet) vs. Recall (Yellow)") +
  theme_minimal()


combined_plot <- gridExtra::grid.arrange(recall_proportions_plot,
                                         average_ratings_plot, nrow = 1)
combined_plot





#### Scatter plot


# Initialize an empty dataframe
mdf <- data.frame()

# Loop through each Experimental.Group
for (i in 1:n_groups) {
  # Subset the data for the current Experimental.Group
  group_data <- subset(wdf, Experimental.Group == i)
  
  # Calculate average ratings for scaled variables I1 to I12 and I13 to I24
  average_ratingsA <- colMeans(group_data[, grep("^I\\d+$", names(group_data))[1:12]])
  average_ratingsB <- colMeans(group_data[, grep("^I\\d+$", names(group_data))[13:24]])
  
  # Standardize the average_ratingsA within the current Experimental.Group
  standardized_average_ratingsA <- scale(average_ratingsA)
  
  # Scale the standardized_average_ratingsA to a range between 0 and 1
  min_val_A <- min(standardized_average_ratingsA)
  max_val_A <- max(standardized_average_ratingsA)
  scaled_average_ratingsA <- (standardized_average_ratingsA - min_val_A) / (max_val_A - min_val_A)
  
  # Standardize the average_ratingsB within the current Experimental.Group
  standardized_average_ratingsB <- scale(average_ratingsB)
  
  # Scale the standardized_average_ratingsB to a range between 0 and 1
  min_val_B <- min(standardized_average_ratingsB)
  max_val_B <- max(standardized_average_ratingsB)
  scaled_average_ratingsB <- (standardized_average_ratingsB - min_val_B) / (max_val_B - min_val_B)
  
  # Calculate proportions of participants recalling each word stimulus for W1 to W12
  recall_proportionsA <- colMeans(group_data[, grep("^W\\d+\\..+", names(group_data))[1:12]])
  recall_proportionsB <- colMeans(group_data[, grep("^W\\d+\\..+", names(group_data))[13:24]])
  
  # Create a temporary dataframe for the current Experimental.Group
  temp_df <- data.frame(Experimental.Group = i,
                        W = paste0("w", 1:12),
                        average_ratingsA = scaled_average_ratingsA,
                        average_ratingsB = scaled_average_ratingsB,
                        recall_proportionsA,
                        recall_proportionsB)
  
  # Append the temporary dataframe to the mdf dataframe
  mdf <- rbind(mdf, temp_df)
}

# Reset row names of the mdf dataframe
row.names(mdf) <- NULL




# Fit the linear regression model for scatter plot A
model_A <- lm(recall_proportionsA ~ average_ratingsA, data = mdf)

# Create the scatter plot for average_ratingsA and recall_proportionsA with separate regression lines
scatter_plot_A <- ggplot(mdf, aes(x = average_ratingsA, y = recall_proportionsA, color = factor(Experimental.Group))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "black", size = 1.5, se = FALSE) +
  labs(title = "A. Scatter Plot of Correlation Effects in Animate Stimuli",
       x = "Average Ease of Interaction Rating for Animate Stimuli",
       y = "Recall Proportion of Animate Words") +
  theme(legend.position = "none")

# Add regression formula to scatter plot A
scatter_plot_A <- scatter_plot_A +
  annotate("text", x = max(mdf$average_ratingsA), y = max(mdf$recall_proportionsA),
           label = paste0("y = ", round(coef(model_A)[1], 2), " + ", round(coef(model_A)[2], 2), "x"),
           hjust = 1, vjust = -0.5, color = "black", size = 6)

# Fit the linear regression model for scatter plot B
model_B <- lm(recall_proportionsB ~ average_ratingsB, data = mdf)

# Create the scatter plot for average_ratingsB and recall_proportionsB with separate regression lines
scatter_plot_B <- ggplot(mdf, aes(x = average_ratingsB, y = recall_proportionsB, color = factor(Experimental.Group))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "black", size = 1.5, se = FALSE) +
  labs(title = "B. Scatter Plot of Correlation Effects in Inanimate Stimuli",
       x = "Average Ease of Interaction Rating for Inanimate Stimuli",
       y = "Recall Proportions of Inanimate Words") +
  theme(legend.position = "none")

# Add regression formula to scatter plot B
scatter_plot_B <- scatter_plot_B +
  annotate("text", x = max(mdf$average_ratingsB), y = max(mdf$recall_proportionsB),
           label = paste0("y = ", round(coef(model_B)[1], 2), " + ", round(coef(model_B)[2], 2), "x"),
           hjust = 1, vjust = -0.5, color = "black", size = 6)

combined_plots <- scatter_plot_A + scatter_plot_B

combined_plots
