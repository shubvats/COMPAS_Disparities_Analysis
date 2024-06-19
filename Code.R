brary(dplyr)
library(ggplot2)
raw_data <- read.csv("./compas-scores-two-years.csv")
nrow(raw_data)

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
filter(days_b_screening_arrest <= 30) %>%
filter(days_b_screening_arrest >= -30) %>%
filter(is_recid != -1) %>%
filter(c_charge_degree != "O") %>%
filter(score_text != 'N/A')
nrow(df)

df <- df %>%
mutate(
age_cat = factor(age_cat),
c_charge_degree = factor(c_charge_degree),
race = factor(race),
score_text = factor(score_text),
sex = factor(sex),
is_recid = factor(is_recid),
two_year_recid = factor(two_year_recid)
)

df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
cor(df$length_of_stay, df$decile_score)

summary(df$age_cat)

summary(df$race)

cat("Black defendants: ", sprintf("%.2f%%\n", (3175 / 6172 * 100)))
cat("White defendants: ", sprintf("%.2f%%\n", (2103 / 6172 * 100)))
cat("Hispanic defendants: ", sprintf("%.2f%%\n", (509 / 6172 * 100)))
cat("Asian defendants: ", sprintf("%.2f%%\n", (31 / 6172 * 100)))
cat("Native American defendants: ", sprintf("%.2f%%\n", (11 / 6172 * 100)))

summary(df$score_text)

xtabs(~ sex + race, data=df)

summary(df$sex)

cat("Men: ", sprintf("%.2f%%\n", (4997 / 6172 * 100)))
cat("Women: ", sprintf("%.2f%%\n", (1175 / 6172 * 100)))

nrow(filter(df, two_year_recid == 1))

nrow(filter(df, two_year_recid == 1)) / nrow(df) * 100

library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) +
geom_bar() + xlab("Decile Score") +
ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) +
geom_bar() + xlab("Decile Score") +
ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
grid.arrange(pblack, pwhite, ncol = 2)

xtabs(~ decile_score + race, data=df)

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
mutate(age_factor = as.factor(age_cat)) %>%
within(age_factor <- relevel(age_factor, ref = 1)) %>%
mutate(race_factor = factor(race)) %>%
within(race_factor <- relevel(race_factor, ref = 3)) %>%
mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))
model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
priors_count + crime_factor + two_year_recid, family="binomial", data=df)
summary(model)

control <- exp(-1.52554) / (1 + exp(-1.52554))
exp(0.47721) / (1 - control + (control * exp(0.47721)))

exp(0.22127) / (1 - control + (control * exp(0.22127)))

exp(1.30839) / (1 - control + (control * exp(1.30839)))

raw_data <- read.csv("./compas-scores-two-years-violent.csv")
nrow(raw_data)

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count,
days_b_screening_arrest, v_decile_score, is_recid, two_year_recid) %>%
filter(days_b_screening_arrest <= 30) %>%
filter(days_b_screening_arrest >= -30) %>%
filter(is_recid != -1) %>%
filter(c_charge_degree != "O") %>%
filter(v_score_text != 'N/A')
nrow(df)

df <- df %>%
mutate(
age_cat = factor(age_cat),
c_charge_degree = factor(c_charge_degree),
race = factor(race),
sex = factor(sex),
is_recid = factor(is_recid),
two_year_recid = factor(two_year_recid),
v_score_text = factor(v_score_text),
v_decile_score = factor(v_decile_score)
)
summary(df$age_cat)

summary(df$race)

summary(df$v_score_text)

nrow(filter(df, two_year_recid == 1)) / nrow(df) * 100

nrow(filter(df, two_year_recid == 1))

library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(v_decile_score))) +
geom_bar() + xlab("Violent Decile Score") +
ylim(0, 700) + ggtitle("Black Defendant's Violent Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(v_decile_score))) +
geom_bar() + xlab("Violent Decile Score") +
ylim(0, 700) + ggtitle("White Defendant's Violent Decile Scores")
grid.arrange(pblack, pwhite, ncol = 2)

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
mutate(age_factor = as.factor(age_cat)) %>%
within(age_factor <- relevel(age_factor, ref = 1)) %>%
mutate(race_factor = factor(race,
labels = c("African-American",
"Asian",
"Caucasian",
"Hispanic",
"Native American",
"Other"))) %>%
within(race_factor <- relevel(race_factor, ref = 3)) %>%
mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
mutate(score_factor = factor(v_score_text != "Low", labels = c("LowScore","HighScore")))
model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
priors_count + crime_factor + two_year_recid, family="binomial", data=df)
summary(model)

control <- exp(-2.24274) / (1 + exp(-2.24274))
exp(0.65893) / (1 - control + (control * exp(0.65893)))

exp(3.14591) / (1 - control + (control * exp(3.14591)))

library(survival)
library(ggfortify)

data <- filter(filter(read.csv("./cox-parsed.csv"), score_text != "N/A"), end > start) %>%
mutate(race_factor = factor(race,
labels = c("African-American",
"Asian",
"Caucasian",
"Hispanic",
"Native American",
"Other"))) %>%
within(race_factor <- relevel(race_factor, ref = 3)) %>%
mutate(score_factor = factor(score_text)) %>%
within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]
nrow(grp)

summary(grp$score_factor)

summary(grp$race_factor)

f <- Surv(start, end, event, type="counting") ~ score_factor
model <- coxph(f, data=data)
summary(model)

decile_f <- Surv(start, end, event, type="counting") ~ decile_score
dmodel <- coxph(decile_f, data=data)
summary(dmodel)

f2 <- Surv(start, end, event, type="counting") ~ race_factor + score_factor + race_factor * score_factor
model <- coxph(f2, data=data)
print(summary(model))

cat("Black High Hazard: ", sprintf("%.2f\n", exp(-0.18976 + 1.28350)))
cat("White High Hazard: ", sprintf("%.2f\n", exp(1.28350)))
cat("Black Medium Hazard: ", sprintf("%.2f\n", exp(0.84286 - 0.17261)))
cat("White Medium Hazard: ", sprintf("%.2f\n", exp(0.84286)))

#Fit the survival curves
fit <- survfit(Surv(start, end, event, type = "counting") ~ score_factor, data = data)

Plot the survival curves using base R plotting

plot(fit, col = c("blue", "red"), lty = c(1, 2), lwd = 2, conf.int = TRUE)
title("Overall Survival")
legend("topright", legend = levels(data$score_factor), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

f <- Surv(start, end, event, type = "counting") ~ score_factor

Fit the survival curves for White defendants

white <- subset(data, race_factor == "Caucasian")
white_fit <- survfit(f, data = white)

Fit the survival curves for Black defendants

black <- subset(data, race_factor == "African-American")
black_fit <- survfit(f, data = black)

Plot the survival curves

par(mfrow = c(1, 2)) # Set up a 1x2 grid for side-by-side plots

plot(white_fit, col = "blue", lty = 1, lwd = 2, conf.int = TRUE)
title("White Defendants Survival")

plot(black_fit, col = "red", lty = 2, lwd = 2, conf.int = TRUE)
title("Black Defendants Survival")

Reset the plot layout

par(mfrow = c(1, 1))

summary(fit, times=c(730))

summary(black_fit, times=c(730))

summary(white_fit, times=c(730))

summary(coxph(f, data=white))

summary(coxph(f, data=black))

violent_data <- filter(filter(read.csv("cox-violent-parsed.csv"), score_text != "N/A"), end > start) %>%
mutate(race_factor = factor(race,
labels = c("African-American",
"Asian",
"Caucasian",
"Hispanic",
"Native American",
"Other"))) %>%
within(race_factor <- relevel(race_factor, ref = 3)) %>%
mutate(score_factor = factor(score_text)) %>%
within(score_factor <- relevel(score_factor, ref=2))

vf <- Surv(start, end, event, type="counting") ~ score_factor
vmodel <- coxph(vf, data=violent_data)
vgrp <- violent_data[!duplicated(violent_data$id),]
print(nrow(vgrp))
summary(vmodel)

vf2 <- Surv(start, end, event, type="counting") ~ race_factor + race_factor * score_factor
vmodel <- coxph(vf2, data=violent_data)
summary(vmodel)

summary(coxph(vf, data=filter(violent_data, race == "African-American")))

summary(coxph(vf, data=filter(violent_data, race == "Caucasian")))

Define the violent survival formula

vf <- Surv(start, end, event, type = "counting") ~ score_factor

Fit the survival curves for White defendants in violent data

white <- subset(violent_data, race_factor == "Caucasian")
white_fit <- survfit(vf, data = white)

Fit the survival curves for Black defendants in violent data

black <- subset(violent_data, race_factor == "African-American")
black_fit <- survfit(vf, data = black)

Plot the survival curves

par(mfrow = c(1, 2)) # Set up a 1x2 grid for side-by-side plots

plot(white_fit, col = "blue", lty = 1, lwd = 2, conf.int = TRUE)
title("White Defendants Survival (Violent)")

plot(black_fit, col = "red", lty = 2, lwd = 2, conf.int = TRUE)
title("Black Defendants Survival (Violent)")

Reset the plot layout

par(mfrow = c(1, 1))

Assuming 'start', 'end', and 'event' are columns in your dataset

f <- Surv(start, end, event, type="counting") ~ score_factor

Assuming 'score_factor' is a column in your dataset

data <- read.csv("cox-parsed.csv")

data <- data %>%
mutate(crime_factor = factor(c_charge_degree)) %>%
mutate(age_factor = as.factor(age_cat)) %>%
within(age_factor <- relevel(age_factor, ref = 1)) %>%
mutate(race_factor = factor(race)) %>%
within(race_factor <- relevel(race_factor, ref = 3)) %>%
mutate(gender_factor = factor(sex, labels = c("Female", "Male"))) %>%
within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
mutate(score_factor = factor(score_text !="Low", labels = c("LowScore", "HighScore")))

Filter data for female and male

female <- subset(data, sex == "Female" & !is.na(start) & !is.na(end) & !is.na(event) )
male <- subset(data, sex == "Male" & !is.na(start) & !is.na(end) & !is.na(event) )

female <- subset(female, end > start)
male <- subset(male, end > start)

Fit survival models

male_fit <- survfit(f, data = male)
female_fit <- survfit(f, data = female)

Display summaries at specific times

summary(male_fit, times = c(730))
summary(female_fit, times = c(730))

Plot survival curves

plot(male_fit, col = "blue", lty = 1, lwd = 2, main = "Male Female", xlab = "Time", ylab = "Survival Probability")
lines(female_fit, col = "red", lty = 2, lwd = 2, main = "Female", xlab = "Time", ylab = "Survival Probability")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = c(1, 2))
Properties
Assignees
