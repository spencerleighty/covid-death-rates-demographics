# Data Preprocessing

## Changed the missing data entries from subgroup2 to None since they were reported as missing if not double stratisfied. Due to the reccomendation of the note tab, observations that included <20 deaths but greater than 0 deaths were dropped. This is due to their sensitivity and unreliability. The note column was then dropped.

library(dplyr)
library(readr)
library(agricolae)
df <- read.csv("D:/UCFFL25/STA4173/CovidDeathRateData.csv")

head(df)
df_clean <- df

df_clean$subgroup2 <- ifelse(df_clean$subgroup2 == "", "None", df_clean$subgroup2)
df_clean$subgroup2 <- factor(df_clean$subgroup2)
table(df_clean$subgroup2, useNA = "ifany")

any(is.na(df_clean))
colSums(is.na(df_clean))

df_clean <- df_clean[df_clean$note == "", ]

df_clean$note <- NULL

df_clean <- df_clean[complete.cases(df_clean), ]
colSums(is.na(df_clean))

summary(df_clean$crude_rate)


# Summary stats for crude death rates


hist(df_clean$crude_rate,
     main = "Histogram of Crude Death Rates",
     xlab = "Crude Death Rate",
     col = "blue")

hist(log(df_clean$crude_rate + 1),
     main = "Histogram of Log Crude Death Rates",
     xlab = "Log Crude Death Rate",
     col = "red")



# Summary stats and One-Way ANOVA for Sex


# Subset Sex group data
sex_data <- df_clean[df_clean$group == "Sex", ]

tapply(sex_data$crude_rate, sex_data$subgroup1, mean)      # mean
tapply(sex_data$crude_rate, sex_data$subgroup1, median)    # median
tapply(sex_data$crude_rate, sex_data$subgroup1, sd)        # standard deviation
tapply(sex_data$crude_rate, sex_data$subgroup1, min)       # min
tapply(sex_data$crude_rate, sex_data$subgroup1, max)       # max

boxplot(crude_rate ~ subgroup1, data = sex_data,
        main = "COVID-19 Death Rates by Sex",
        xlab = "Sex Group",
        ylab = "Death Rate per 100,000",
        las = 2,  # rotate x-axis labels
        col = "lightgreen",
        ylim = c(0,20))

anova_sex <- aov(crude_rate ~ subgroup1, data = sex_data)
summary(anova_sex)
tukey_sex <- TukeyHSD(anova_sex)
tukey_df <- as.data.frame(tukey_sex$subgroup1)
tukey_df_sig <- tukey_df[tukey_df$`p adj` < 0.05, ]
tukey_df_sig

qqnorm(residuals(anova_sex), main = "QQ Plot for Sex") 
qqline(residuals(anova_sex), col = "lightgreen")




# Summary stats and One-Way ANOVA for Age


# Subset Age group data
age_data <- df_clean[df_clean$group == "Age", ]

#MEANS
age_means <- tapply(age_data$crude_rate, age_data$subgroup1, mean)
age_means <- round(age_means, 5)
age_means_sorted <- sort(age_means)
age_means_sorted

#SD
age_sd <- tapply(age_data$crude_rate, age_data$subgroup1, sd)
age_sd <- round(age_sd, 5)
age_sd_sorted <- sort(age_sd)
age_sd_sorted


median(age_data$crude_rate)
sd(age_data$crude_rate)

tapply(age_data$crude_rate, age_data$subgroup1, median)    # median
tapply(age_data$crude_rate, age_data$subgroup1, sd)        # standard deviation
tapply(age_data$crude_rate, age_data$subgroup1, min)       # min
tapply(age_data$crude_rate, age_data$subgroup1, max)       # max

boxplot(crude_rate ~ subgroup1, data = age_data,
        main = "COVID-19 Death Rates by Age Group",
        xlab = "Age Group",
        ylab = "Death Rate per 100,000",
        las = 2,  # rotate x-axis labels
        col = "blue",
        ylim = c(0,20))

anova_age <- aov(crude_rate ~ subgroup1, data = age_data)
summary(anova_age)

tukey_age <- TukeyHSD(anova_age)
tukey_df <- as.data.frame(tukey_age$subgroup1)
tukey_df_sig <- tukey_df[tukey_df$`p adj` < 0.05, ]
tukey_df_sig

qqnorm(residuals(anova_age), main = "QQ Plot for Age")
qqline(residuals(anova_age), col = "blue")

tukey_age_cld <- HSD.test(anova_age, "subgroup1", group = TRUE)
cld_age <- tukey_age_cld$groups   
cld_age


# Summary stats and One-Way ANOVA for Race



# Subset Race group data
race_data <- df_clean[df_clean$group == "Race", ]

race_means <- tapply(race_data$crude_rate, race_data$subgroup1, mean)
race_means <- round(race_means, 5)
race_means_sorted <- sort(race_means)
race_means_sorted

race_sd <- tapply(race_data$crude_rate, race_data$subgroup1, sd)
race_sd <- round(race_sd, 5)
race_sd_sorted <- sort(race_sd)
race_sd_sorted


tapply(race_data$crude_rate, race_data$subgroup1, mean)      # mean
tapply(race_data$crude_rate, race_data$subgroup1, median)    # median
tapply(race_data$crude_rate, race_data$subgroup1, sd)        # standard deviation
tapply(race_data$crude_rate, race_data$subgroup1, min)       # min
tapply(race_data$crude_rate, race_data$subgroup1, max)       # max

boxplot(crude_rate ~ subgroup1, data = race_data,
        main = "COVID-19 Death Rates by Race Group",
        xlab = "Race Group",
        ylab = "Death Rate per 100,000",
        las = 2,  # rotate x-axis labels
        col = "red",
        ylim=c(0,50))

anova_race <- aov(crude_rate ~ subgroup1, data = race_data)
summary(anova_race)
tukey_race <- TukeyHSD(anova_race)
tukey_df <- as.data.frame(tukey_race$subgroup1)
tukey_df_sig <- tukey_df[tukey_df$`p adj` < 0.05, ]
tukey_df_sig

qqnorm(residuals(anova_race), main = "QQ Plot for Race")
qqline(residuals(anova_race), col = "red")
plot(TukeyHSD(anova_race))




tukey_race_cld <- HSD.test(anova_race, "subgroup1", group = TRUE)
cld_race <- tukey_race_cld$groups
cld_race



# Data Preparation for Logistic Regression


df_clean_bin <- df_clean
df_clean_bin$death_present <- ifelse(df_clean_bin$covid_deaths > 0, 1, 0)


# Logistic Regression for Sex



sex_data_bin <- df_clean_bin[df_clean_bin$group == "Sex", ]
logmodel_sex <- glm(death_present ~ subgroup1, data = sex_data_bin, family = binomial)
summary(logmodel_sex)


# Logistic Regression for Age


age_data_bin <- df_clean_bin[df_clean_bin$group == "Age", ]
logmodel_age <- glm(death_present ~ subgroup1, data = age_data_bin, family = binomial)
summary(logmodel_age)


# Logistic Regression for Race


race_data_bin <- df_clean_bin[df_clean_bin$group == "Race", ]
logmodel_race <- glm(death_present ~ subgroup1, data = race_data_bin, family = binomial)
summary(logmodel_race)


# Coefficients


exp(coef(logmodel_age))
exp(coef(logmodel_sex))
exp(coef(logmodel_race))

# Confidence Intervals


exp(confint(logmodel_age))
exp(confint(logmodel_sex))
exp(confint(logmodel_race))

# Kruskal and Wilcox

## Since the normality assumption was not verified for ANOVA, a Kruskal test was done comparing the medians and then a post-hoc pairwise Wilcox test was done. Although outside of the scope of this course, this was a non-parametric option for our zero-inflated and skewed data.


kruskal.test(crude_rate ~ subgroup1, data = age_data)
pairwise.wilcox.test(age_data$crude_rate,
                     age_data$subgroup1,
                     p.adjust.method = "bonferroni")

kruskal.test(crude_rate ~ subgroup1, data = sex_data)
pairwise.wilcox.test(sex_data$crude_rate,
                     sex_data$subgroup1,
                     p.adjust.method = "bonferroni")

kruskal.test(crude_rate ~ subgroup1, data = race_data)
pairwise.wilcox.test(race_data$crude_rate,
                     race_data$subgroup1,
                     p.adjust.method = "bonferroni")




library(ggplot2)
library(broom)

tidy_model_age <- broom::tidy(logmodel_age, exponentiate = TRUE, conf.int = TRUE)
ggplot(tidy_model_age[-1, ], aes(x = term, y = estimate)) +  # remove intercept
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  
  labs(x = "Age Group", y = "Odds Ratio (logistic regression)", title = "Odds of COVID-19 Death by Age Group") +
  theme_minimal()

tidy_model_sex <- broom::tidy(logmodel_sex, exponentiate = TRUE, conf.int = TRUE)

ggplot(tidy_model_sex[-1, ], aes(x = term, y = estimate)) +  # remove intercept
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  
  labs(x = "Sex Group", y = "Odds Ratio (logistic regression)", title = "Odds of COVID-19 Death by Sex") +
  theme_minimal()


tidy_model_race <- broom::tidy(logmodel_race, exponentiate = TRUE, conf.int = TRUE)

ggplot(tidy_model_race[-1, ], aes(x = term, y = estimate)) +  # remove intercept
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  
  labs(x = "Race Group", y = "Odds Ratio (logistic regression)", title = "Odds of COVID-19 Death by Race Group") +
  theme_minimal()


