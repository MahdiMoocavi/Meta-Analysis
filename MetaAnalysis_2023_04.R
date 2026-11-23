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


# Compute a bias-corrected standardized mean change and its sampling
# variance for a paired comparison. Pairwise missing observations are removed
# before the effect size, t-test, and sample size are calculated.
paired_smcc <- function(x, y) {
  paired <- data.frame(
    x = suppressWarnings(as.numeric(x)),
    y = suppressWarnings(as.numeric(y))
  )
  paired <- paired[stats::complete.cases(paired), , drop = FALSE]
  n <- nrow(paired)

  if (n < 3 || stats::sd(paired$x - paired$y) == 0) {
    return(data.frame(
      npp = n, p_value = NA_real_, conf_up = NA_real_,
      conf_low = NA_real_, stderr = NA_real_, t_statistics = NA_real_,
      yi = NA_real_, vi = NA_real_, sei = NA_real_, weights = NA_real_
    ))
  }

  test <- stats::t.test(paired$x, paired$y, paired = TRUE, conf.level = 0.99)
  within_correlation <- suppressWarnings(stats::cor(paired$x, paired$y))

  if (!is.finite(within_correlation)) {
    return(data.frame(
      npp = n, p_value = test$p.value, conf_up = test$conf.int[2],
      conf_low = test$conf.int[1], stderr = test$stderr,
      t_statistics = unname(test$statistic), yi = NA_real_,
      vi = NA_real_, sei = NA_real_, weights = NA_real_
    ))
  }

  effect <- metafor::escalc(
    measure = "SMCC",
    m1i = mean(paired$x),
    m2i = mean(paired$y),
    sd1i = stats::sd(paired$x),
    sd2i = stats::sd(paired$y),
    ri = within_correlation,
    ni = n,
    vtype = "LS"
  )

  yi <- as.numeric(effect$yi[1])
  vi <- as.numeric(effect$vi[1])

  data.frame(
    npp = n,
    p_value = test$p.value,
    conf_up = test$conf.int[2],
    conf_low = test$conf.int[1],
    stderr = test$stderr,
    t_statistics = unname(test$statistic),
    yi = yi,
    vi = vi,
    sei = sqrt(vi),
    weights = 1 / vi
  )
}


#### GROUP analysis I: paired animacy-recall effects

T_df_A <- do.call(rbind, lapply(seq_len(n_groups), function(i) {
  file_name <- paste0("./group", i, ".csv")
  study_data <- read.csv(file_name, header = TRUE)

  effect <- paired_smcc(
    study_data$Number.of.Animate.words.recalled,
    study_data$Number.of.Inanimate.words.recalled
  )
  cbind(Experimental.Group = i, effect)
}))

write.csv(T_df_A, "T_table_A.csv", row.names = FALSE)

##### META-analysis I: equal-effects model

meta_A_data <- subset(T_df_A, is.finite(yi) & is.finite(vi) & vi > 0)
EE_model_A <- metafor::rma(yi, vi, method = "EE", data = meta_A_data)
summary(EE_model_A)

meta_A_data$t_statistics <- round(meta_A_data$t_statistics, 2)
par(mfrow = c(1, 2))
figure2 <- forest(
  EE_model_A,
  main = "A. Equal-Effects Meta-Analysis of the Animacy Recall Difference",
  header = "Study, sample size, and paired t statistic",
  xlim = c(-1.3, 3.5),
  refline = coef(EE_model_A),
  cex = 0.6,
  psize = 1,
  showweights = TRUE,
  slab = paste0(
    "Study ", meta_A_data$Experimental.Group,
    " (n=", meta_A_data$npp,
    "; t=", meta_A_data$t_statistics, ")"
  ),
  order = "obs",
  xlab = "Hedges' g (standardized mean change)"
)
figure3 <- funnel(
  EE_model_A,
  main = "B. Funnel Plot of the Animacy Recall Meta-Analysis",
  header = "Study",
  refline = coef(EE_model_A),
  level = c(90, 95, 99),
  shade = c("white", "gray65", "gray25"),
  lty2 = 1,
  label = TRUE,
  yaxis = "sei",
  legend = TRUE
)


#### 2) Interactive Imagery Effect

### T-test
t_statistics <- t.test(wdf$Animate..average.interaction.rating.
                       ,wdf$Inanimate..average.interaction.rating.,
                       mu=0, alt="two.sided", paired=T, conf.level=0.99)
t_statistics


#### GROUP analysis II: paired interaction-rating effects

T_df_I <- do.call(rbind, lapply(seq_len(n_groups), function(i) {
  file_name <- paste0("./group", i, ".csv")
  study_data <- read.csv(file_name, header = TRUE)

  effect <- paired_smcc(
    study_data$Animate..average.interaction.rating.,
    study_data$Inanimate..average.interaction.rating.
  )
  cbind(Experimental.Group = i, effect)
}))

write.csv(T_df_I, "T_table_I.csv", row.names = FALSE)

#### META-analysis II: random-effects model

meta_I_data <- subset(T_df_I, is.finite(yi) & is.finite(vi) & vi > 0)
RE_model_I <- metafor::rma(yi, vi, method = "REML", data = meta_I_data)
summary(RE_model_I)

meta_I_data$t_statistics <- round(meta_I_data$t_statistics, 2)
par(mfrow = c(1, 2))
figure2 <- forest(
  RE_model_I,
  main = "A. Random-Effects Meta-Analysis of Interaction Ratings",
  header = "Study, sample size, and paired t statistic",
  xlim = c(-3, 2),
  refline = coef(RE_model_I),
  cex = 0.6,
  psize = 1,
  showweights = TRUE,
  addpred = TRUE,
  slab = paste0(
    "Study ", meta_I_data$Experimental.Group,
    " (n=", meta_I_data$npp,
    "; t=", meta_I_data$t_statistics, ")"
  ),
  order = "obs",
  xlab = "Hedges' g (standardized mean change)"
)
figure3 <- funnel(
  RE_model_I,
  main = "B. Funnel Plot of the Interaction-Rating Meta-Analysis",
  header = "Study",
  refline = coef(RE_model_I),
  level = c(90, 95, 99),
  shade = c("white", "gray65", "gray25"),
  lty2 = 1,
  label = TRUE,
  yaxis = "sei",
  legend = TRUE
)


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



correlation_results <- lapply(unique(rdf$Experimental.Group), function(group) {
  group_data <- subset(rdf, Experimental.Group == group)

  # Each group contributes one item-level correlation across the 24 stimuli.
  item_ratings <- colMeans(group_data[, grep("^I\\d+$", names(group_data))])
  item_recall <- colMeans(group_data[, grep("^W\\d+\\..+", names(group_data))])

  complete_items <- stats::complete.cases(item_ratings, item_recall)
  item_ratings <- item_ratings[complete_items]
  item_recall <- item_recall[complete_items]

  if (length(item_ratings) < 4 ||
      stats::sd(item_ratings) == 0 ||
      stats::sd(item_recall) == 0) {
    return(data.frame(
      Experimental.Group = group,
      Participants = nrow(group_data),
      Items = length(item_ratings),
      Correlation = NA_real_,
      P_value = NA_real_,
      yi = NA_real_,
      vi = NA_real_,
      sei = NA_real_
    ))
  }

  correlation_test <- stats::cor.test(item_ratings, item_recall)
  correlation <- unname(correlation_test$estimate)

  # Fisher's r-to-z is the standard scale for pooling correlations.
  effect <- metafor::escalc(
    measure = "ZCOR",
    ri = correlation,
    ni = length(item_ratings)
  )

  data.frame(
    Experimental.Group = group,
    Participants = nrow(group_data),
    Items = length(item_ratings),
    Correlation = correlation,
    P_value = correlation_test$p.value,
    yi = as.numeric(effect$yi[1]),
    vi = as.numeric(effect$vi[1]),
    sei = sqrt(as.numeric(effect$vi[1]))
  )
})

C_table <- do.call(rbind, correlation_results)
write.csv(C_table, "C_table.csv", row.names = FALSE)

#### META-analysis III: random-effects model of Fisher-z correlations

meta_C_data <- subset(C_table, is.finite(yi) & is.finite(vi) & vi > 0)
RE_model_C <- metafor::rma(yi, vi, method = "REML", data = meta_C_data)
summary(RE_model_C)

meta_C_data$Correlation_label <- round(meta_C_data$Correlation, 2)
par(mfrow = c(1, 2))
figure2 <- forest(
  RE_model_C,
  main = "A. Meta-Analysis of Item-Level Interaction–Recall Correlations",
  header = "Study (number of items; raw correlation)",
  xlim = c(-2.5, 2.8),
  refline = coef(RE_model_C),
  cex = 0.6,
  psize = 1,
  showweights = TRUE,
  addpred = TRUE,
  slab = paste0(
    "Study ", meta_C_data$Experimental.Group,
    " (items=", meta_C_data$Items,
    "; r=", meta_C_data$Correlation_label, ")"
  ),
  order = "obs",
  xlab = "Fisher's z-transformed correlation"
)
figure3 <- funnel(
  RE_model_C,
  main = "B. Funnel Plot of the Item-Level Correlation Meta-Analysis",
  header = "Study",
  refline = coef(RE_model_C),
  level = c(90, 95, 99),
  shade = c("white", "gray65", "gray25"),
  lty2 = 1,
  label = TRUE,
  yaxis = "sei",
  legend = TRUE
)


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
