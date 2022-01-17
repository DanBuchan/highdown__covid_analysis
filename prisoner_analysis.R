library(tidyr)
library(corrplot)
library(dplyr)
library(caret)
library(Hmisc)

# edit significance offset
# https://stackoverflow.com/questions/49695113/changing-the-position-of-the-signifigance-pch-symbols-in-corrplot

prisoner_experience_data <- read.csv("/Users/dbuchan/Projects/prison_analysis/Prisoner_experience_database.csv")
staff_experience_data <- read.csv("/Users/dbuchan/Projects/prison_analysis/Staff_experience_database.csv")

prisoner_experience_data <- read.csv("/home/dbuchan/Projects/radha_analysis/Prisoner_experience_database.csv")
staff_experience_data <- read.csv("/home/dbuchan/Projects/radha_analysis/Staff_experience_database.csv")

# drop columns that we won't be needing, mostly free text fields
prisoner_experience_data$OTHER <- NULL
prisoner_experience_data$Better <- NULL
prisoner_experience_data$Same <- NULL
prisoner_experience_data$Worse <- NULL
prisoner_experience_data$DIFFICULT <- NULL
prisoner_experience_data$HELPFUL <- NULL
prisoner_experience_data$In_prison_comments <- NULL
prisoner_experience_data$Outside_prison_comments <- NULL
prisoner_experience_data$OTHER_final <- NULL
# explanatory variables (predictors): prisoner_experience_data[,c(2:19,36:41)]
# response variables (outcomes): prisoner_experience_data[,20:35]

staff_experience_data$Year_questionnaire_completed <- NULL
staff_experience_data$START_DATE <- NULL
staff_experience_data$OTHER <- NULL
staff_experience_data$OTHER_symptoms <- NULL
staff_experience_data$OTHER_lifestyle <- NULL
staff_experience_data$POSITIVE_CONSEQUENCES <- NULL
staff_experience_data$BETTER_SUPPORTED <- NULL
staff_experience_data$OTHER_COMMENTS <- NULL
staff_experience_data$FURTHER_CONTACT <- NULL
staff_experience_data$email <- NULL
# explanatory variables (predictors): staff_experience_data[,c(2:12,21:25)]
# response variables (outcomes): staff_experience_data[,13:20]

# Now tidy data
# Prisoner question/dichotomous, we leave the NAs.
# Prisoner outcomes/symptoms/response vars: NAs become 2, blanks become NA
# For staff all variables and responses are binary so NAs remain as is.
prisoner_experience_data[,20:35][is.na(prisoner_experience_data[,20:35])] <- 2

# Now cast variables to the right types
prisoner_experience_data$ETHNICITY <- as.factor(prisoner_experience_data$ETHNICITY)
#prisoner_experience_data[,5:35] <- lapply(prisoner_experience_data[,5:35],as.factor)
staff_experience_data$ETHNICITY <- as.factor(staff_experience_data$ETHNICITY)
#staff_experience_data[,5:21] <- lapply(staff_experience_data[,5:21],as.factor)

# Look for anything obvious
staff_complete_subset <- na.omit(staff_experience_data)
colnames(staff_complete_subset)<-c("ID", "AGE","ETHNICITY", "Length_of_service", "JOB","Catching_covid", "Infecting_FF", "Worry_prisoners", "Feeling_frustrated", "Coping_changes", "Worry_colleagues", "Coping_staff_shortages", "LOW_MOOD", "ANXIETY", "WORRY", "FRUSTRATION", "SLEEP", "APPETITE", "DRINKING", "SMOKING", "Support_FF", "Support_Managers,", "Support_Colleagues", "Exercise", "Hobbies") 
#cor(staff_complete_subset[,6:21])
staff_correlation <- rcorr(as.matrix(staff_complete_subset[,6:21]))
corrplot(staff_correlation$r, p.mat = staff_correlation$P, sig.level = c(.001, .01, .05), insig="label_sig", pch.cex = 1.5, method='number', type="upper", number.cex=0.70, tl.col="black", tl.cex = 0.5, is.corr = FALSE, col="black", cl.pos="n")

prisoner_complete_subset <- na.omit(prisoner_experience_data)
colnames(prisoner_complete_subset)<-c("NUMBER", "AGE", "ETHNICITY", "ARRIVAL", "FIRST_TIME", "Court_changes", "More_time_in_cell", "Less_showers", "Cancelled_visits", "Less_staff", "No_education", "Less_work", "Less_MH_support", "Less_PH_support", "Changes_in_mealtimes", "More_time_cell_mates", "Changes_in_timetable", "Less_gym", "Difficult_busy_in_cell", "LOW_MOOD", "ANXIOUS", "SCARED", "WORRY", "V_AND_V", "SLEEP", "EATING", "DRUGS", "UPSETTING_THOUGHTS", "UPSETTING_MEMORIES", "NIGHTMARES", "FRUSTRATION_ANGER", "ARGUMENTS", "DSH", "SUICIDAL_THOUGHTS", "FEELING_SAFE", "Worried_covid_in_prison", "Worried_covid_in_community", "Worried_housing_on_release", "Worried_probation_on_release", "Worried_MH_on_release","Worried_PH_on_release")
# cor(prisoner_complete_subset[,6:35])
prisoner_correlation <- rcorr(as.matrix(prisoner_complete_subset[,6:35]))
par(xpd = TRUE)
corrplot(prisoner_correlation$r, p.mat = prisoner_correlation$P, sig.level = c(.001, .01, .05), insig="label_sig", pch.cex = 1.5, method='number', type="upper", number.cex=0.50, tl.col="black", tl.cex = 0.5, is.corr = FALSE, col="black", cl.pos="n", mar = c(1, 1, 1, 1))
corrplot(prisoner_correlation$r, p.mat = prisoner_correlation$P, sig.level = .05, insig="blank", method='number', type="upper", number.cex=0.45, tl.col="black", tl.cex = 0.5, is.corr = FALSE, col="black", cl.pos="n", mar = c(1, 1, 1, 1))

write.table(format(prisoner_correlation$r, digits=1), file="/Users/dbuchan/Projects/prison_analysis/prisoner_correlations.csv", sep=",")
write.table(format(prisoner_correlation$P, scientific=F), file="/Users/dbuchan/Projects/prison_analysis/prisoner_corr_pvalues.csv", sep=",")
write.table(format(staff_correlation$r, digits=1), file="/Users/dbuchan/Projects/prison_analysis/staff_correlations.csv", sep=",")
write.table(format(staff_correlation$P, scientific=F), file="/Users/dbuchan/Projects/prison_analysis/staff_corr_pvalues.csv", sep=",")

# look at the bias of the answers
sapply(prisoner_complete_subset[,6:19], function(x) table(factor(x, levels=c(0,1), ordered=TRUE)))
sapply(prisoner_complete_subset[,20:35], function(x) table(factor(x, levels=c(1,2,3), ordered=TRUE)))
sapply(prisoner_complete_subset[,36:41], function(x) table(factor(x, levels=c(0,1), ordered=TRUE)))

sapply(staff_complete_subset[,6:12], function(x) table(factor(x, levels=c(0,1), ordered=TRUE)))
sapply(staff_complete_subset[,13:25], function(x) table(factor(x, levels=c(0,1), ordered=TRUE)))

# Now we're happy send everything to factors
prisoner_complete_subset[,c(5:19,36:41)] <- lapply(prisoner_complete_subset[,c(5:19,36:41)],as.factor)
staff_complete_subset[,c(5:12,21:25)] <- lapply(staff_complete_subset[,c(5:12,21:25)],as.factor)

#### FEATURE SELECTION!

trainingRows <- createDataPartition(prisoner_complete_subset$LOW_MOOD, p=0.7, list=F)
trainingSubset <- prisoner_complete_subset[trainingRows, ]

rfeConfig = rfeControl(functions=rfFuncs, method="repeatedcv", repeats=5, verbose=F)
# lmProfile$optVariables[1:6]
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$LOW_MOOD, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$ANXIOUS, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$SCARED, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$WORRY, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$V_AND_V, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$SLEEP, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$EATING, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$DRUGS, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$UPSETTING_THOUGHTS, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$UPSETTING_MEMORIES, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$NIGHTMARES, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$FRUSTRATION_ANGER, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$ARGUMENTS, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$DSH, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$SUICIDAL_THOUGHTS, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=prisoner_complete_subset[, c(5:19,36:41)], y=prisoner_complete_subset$FEELING_SAFE, sizes=c(3:7), rfeControl=rfeConfig)

lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$LOW_MOOD, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$ANXIETY, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$WORRY, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$FRUSTRATION, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$SLEEP, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$APPETITE, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$DRINKING, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$SMOKING, sizes=c(3:7), rfeControl=rfeConfig)
lmProfile <- rfe(x=staff_complete_subset[, c(6:12,21:25)], y=staff_complete_subset$DRINKING, sizes=c(3:7), rfeControl=rfeConfig)
# 70:30 Worrying_about_colleagues, Support_Managers, Coping_with_ongoing_changes, Exercise, Worry_about_prisoners Feeling_frustrated_cant_help_prisoners Coping_with_staff_shortages

# library(rpart)
# library(rpart.plot)

# model.rpart <- rpart(staff_complete_subset[, c(6:12,19,21:25)]$Drinking ~ .,method = "class", data = staff_complete_subset[, c(6:12,19,21:25)]plot(model.rpart)
# plot(model.rpart)
# text(model.rpart)