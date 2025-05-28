# LOADING REQUIRED PACKAGES AND READING PREPARED DATASET ----- 
library(readxl) #reading data from excel file 
library(performance)
library(lme4) # analyses
library(interactions) # simple slopes and others
library(misty) # centering for analyses
library(lavaan)
library(dplyr) # data wrangling
library(tidyr)
library(ggplot2)
library(splithalfr) # attempt at computing Spearman-Brown coefficient in
library(psych) # multilevel correlations
library(patchwork)# for multiple figures in one
library(report)   # for reporting
library(cowplot) #for get_legend() function
library(gridExtra) # for grid.arrange() function
df <- read_excel("~/Alba/Dataset Paradoxical Effect of Emotional Acculturation")
# RELIABILTIY OF SCALES -----
## Perceived discrimination
Perceived_Discrimination <- df[,c("y1_dissch1_rev", "y1_dissch2_rev", "y1_dissch3_rev", "y1_dissch4_rev")]
psych::alpha(Perceived_Discrimination)

## Majority contact
y1_contact <- df[,c("rev_y1_bgfr1", "rev_y1_rescs1")]
y1_contact <- y1_contact[complete.cases(y1_contact), ]
spearman_brown(y1_contact$rev_y1_bgfr1, y1_contact$rev_y1_rescs1) 

y2_contact <- df[,c("rev_y2_bgfr1", "rev_y2_rescs1")]
y2_contact <- y2_contact[complete.cases(y2_contact), ]
spearman_brown(y2_contact$rev_y2_bgfr1, y2_contact$rev_y2_rescs1) 

## Motivation at school
y1_Motivation_School <- df[,c("y1_eff11_r", "y1_eff12_r", "y1_eff9_r")]
psych::alpha(y1_Motivation_School) 
y2_Motivation_School <- df[,c("y2_eff11_r", "y2_eff12_r", "y2_eff9_r")]
psych::alpha(y2_Motivation_School) 

## Behavioral engagement
y1_Behavioral_engagement <- df[,c("y1_eff3_r", "y1_eff4_r", "y1_eff6_r")]
psych::alpha(y1_Behavioral_engagement) 
y2_Behavioral_engagement <- df[,c("y2_eff3_r", "y2_eff4_r", "y2_eff6_r")]
psych::alpha(y2_Behavioral_engagement) 

## Behavioral disengagement
y1_Behavioral_disengagement <- df[,c("y1_eff10_r", "y1_eff5_r", "y1_eff8_r")]
psych::alpha(y1_Behavioral_disengagement)
y2_Behavioral_disengagement <- df[,c("y2_eff10_r", "y2_eff5_r", "y2_eff8_r")]
psych::alpha(y2_Behavioral_disengagement) 

## School non-compliance
y1_School_noncompliance <- df[,c("y1_pbsch2_rev","y1_pbsch3_rev", "y1_pbsch4_rev")]
psych::alpha(y1_School_noncompliance) 
y2_School_noncompliance <- df[,c("y2_pbsch2_rev", "y2_pbsch3_rev", "y2_pbsch4_rev")]
psych::alpha(y2_School_noncompliance) 

# DESCRIPTIVE TABLE
correlation <- df[, c("schno_revised", "y1_em_fit", "y1_PerceivedDisSch", 
                      "y2_direct_contact", "y2_emot_engagement", "y2_behav_engage_short", 
                      "y2_behav_disengage_short", "y2_sch_noncompl", 
                      "age", "sex", "y1_direct_contact", "y1_emot_engagement", "y1_behav_engage_short", 
                      "y1_behav_disengage_short", "y1_sch_noncompl")]
colMeans(correlation, na.rm = TRUE)
sapply(correlation, sd, na.rm = TRUE)
sapply(correlation, range, na.rm = TRUE)

mlcors <- statsBy(correlation, group = "schno_revised")
mlcors$ci.wg ### CI
mlcors$pwg < .001 ### ***p
mlcors$pwg < .01 & mlcors$pwg > .001
mlcors$pwg < .05 & mlcors$pwg > .01

# ANALYSES -------
## MAJORITY CONTACT -------
dfF01<- df[which(complete.cases(df[,c("sex", "age", "y2_direct_contact", "y1_direct_contact", "y1_PerceivedDisSch", "y1_em_fit" )])),]

# Centering with misty package
dfF01$y1_em_fit <- misty::center(dfF01$y1_em_fit, type = c("CGM"))
dfF01$age <- center(dfF01$age, type = c("CGM"))
dfF01$y1_PerceivedDisSch <- misty::center(dfF01$y1_PerceivedDisSch, type = c("CGM"))
dfF01$y1_direct_contact <- misty::center(dfF01$y1_direct_contact, type = c("CGM"))

### H1 ----
summary(model_null_contact <- lmerTest::lmer(y2_direct_contact ~ (1|schno_revised), data = dfF01, REML=F))
confint(model_null_contact)

summary(model_control_contact <- lmerTest::lmer(y2_direct_contact ~ sex + age + y1_direct_contact+ (1|schno_revised), data = dfF01, REML=F))
confint(model_control_contact)

summary(model_pred_contact <- lmerTest::lmer(y2_direct_contact ~ sex + age + y1_em_fit + y1_direct_contact + (1|schno_revised), data = dfF01, REML=F))
confint(model_pred_contact)

#model comparison
anova(model_null_contact, model_control_contact) 
anova(model_control_contact, model_pred_contact)

### H2 ----
summary(model_EmFitxPercDisc_contact <- lmerTest::lmer(
  y2_direct_contact ~ sex + age + y1_direct_contact + 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF01, 
  REML=FALSE
))
confint(model_EmFitxPercDisc_contact)
anova(model_pred_contact, model_EmFitxPercDisc_contact)

# Simple Slopes
sim_slopes(model_EmFitxPercDisc_contact, pred = y1_em_fit, modx = y1_PerceivedDisSch)

### VISUALIZATION ----
plot1 <- interact_plot(model_EmFitxPercDisc_contact, pred = y1_em_fit, 
                       modx = y1_PerceivedDisSch, 
                       plot.points = TRUE, colors = "Blues", 
                       legend.main = "Perceived discrimination at school")
# modify the plot's theme to match APA style guidelines
plot1 <- plot1 + theme_bw() +
  theme(text = element_text(family = "TT Times New Roman", size = 11)) +
  theme(axis.title = element_text(size = 11)) + theme(plot.title = element_text(size = 11)) +
  theme(axis.text = element_text(size = 11)) + theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) + theme(
    axis.line = element_line(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
# add a title and labels to the x and y axes
plot1 <- plot1 + labs(title = "", x = "Emotional fit", y = "Direct contact")

### SUPPLEMENTARY ANALYSES AND VISUALIZATION ----
summary(model_EmFitxPercDisc_contact_SCHmoderator <- lmerTest::lmer(
  y2_direct_contact ~ sex + age + y1_direct_contact + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1 | schno_revised),
  data = dfF01,
  REML = FALSE
))
confint(model_EmFitxPercDisc_contact_SCHmoderator)
anova(model_pred_contact, model_EmFitxPercDisc_contact_SCHmoderator)
anova(model_EmFitxPercDisc_contact, model_EmFitxPercDisc_contact_SCHmoderator)

summary(model_EmFitxPercDisc_contact_combined <- lmerTest::lmer(
  y2_direct_contact ~ sex + age + y1_direct_contact + 
    y1_PerceivedDisSch * y1_em_fit + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1 | schno_revised),
  data = dfF01,
  REML = FALSE
))
confint(model_EmFitxPercDisc_contact_combined)
anova(model_pred_contact, model_EmFitxPercDisc_contact_combined)
anova(model_EmFitxPercDisc_contact, model_EmFitxPercDisc_contact_combined)

# Simple Slopes
sim_slopes(model_EmFitxPercDisc_contact_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch)

plot1_supp <- interact_plot(model_EmFitxPercDisc_contact_combined, 
                            pred = y1_em_fit, modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                            plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                            legend.main = "Perceived Discrimination at School Wave 1")
# add a title and labels to the x and y axes
plot1_supp <- plot1_supp + labs(title = "", x = "Emotional fit", y = "Direct contact")

## MOTIVATION -------------
dfF02<- df[which(complete.cases(df[,c("sex", "age", "y2_emot_engagement", "y1_emot_engagement", "y1_PerceivedDisSch", "y1_em_fit" )])),]

# Centering with misty package
dfF02$y1_em_fit <- misty::center(dfF02$y1_em_fit, type = c("CGM"))
dfF02$age <- misty::center(dfF02$age, type = c("CGM"))
dfF02$y1_PerceivedDisSch <- misty::center(dfF02$y1_PerceivedDisSch, type = c("CGM"))
dfF02$y1_emot_engagement <- misty::center(dfF02$y1_emot_engagement, type = c("CGM"))

### H1 ----
summary(model_null_motivation <- lmerTest::lmer(y2_emot_engagement ~ (1|schno_revised), data = dfF02, REML=F))
confint(model_null_motivation)

summary(model_control_motivation <- lmerTest::lmer(y2_emot_engagement ~ sex + age + y1_emot_engagement+ (1|schno_revised), data = dfF02, REML=F))
confint(model_control_motivation)

summary(model_pred_motivation <- lmerTest::lmer(y2_emot_engagement ~ sex + age + y1_em_fit + y1_emot_engagement + (1|schno_revised), data = dfF02, REML=F))
confint(model_pred_motivation)

#model comparison
anova(model_null_motivation, model_control_motivation) 
anova(model_control_motivation, model_pred_motivation)

### H2 ----
summary(model_EmFitxPercDisc_motivation <- lmerTest::lmer(
  y2_emot_engagement ~ sex + age + y1_emot_engagement+ 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF02, 
  REML=F))
confint(model_EmFitxPercDisc_motivation)
anova(model_pred_motivation, model_EmFitxPercDisc_motivation)

sim_slopes(model_EmFitxPercDisc_motivation, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

###VISUALIZATION----
plot2 <- interact_plot(model_EmFitxPercDisc_motivation, pred = y1_em_fit, 
                       modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                       plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                       legend.main = "Perceived Discrimination at School Wave 1")
# modify the plot's theme to match APA style guidelines
plot2 <- plot2 + theme_bw() +
  theme(text = element_text(family = "Arial", size = 11)) +
  theme(axis.title = element_text(size = 11)) + theme(plot.title = element_text(size = 11)) +
  theme(axis.text = element_text(size = 11)) + theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) + theme(
    axis.line = element_line(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
# add a title and labels to the x and y axes
plot2 <- plot2 + labs(x = "Emotional Fit Wave 1", y = "Motivation at School Wave 2")

### SUPPLEMENTARY ANALYSES AND VISUALIZATION ----
summary(model_EmFitxPercDisc_motivation_SCHmoderator <- lmerTest::lmer(
  y2_emot_engagement ~ sex + age + y1_emot_engagement + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1 | schno_revised),
  data = dfF02,
  REML = FALSE
))
confint(model_EmFitxPercDisc_motivation_SCHmoderator)
anova(model_pred_motivation, model_EmFitxPercDisc_motivation_SCHmoderator)
anova(model_EmFitxPercDisc_motivation, model_EmFitxPercDisc_motivation_SCHmoderator)

summary(model_EmFitxPercDisc_motivation_combined <- lmerTest::lmer(
  y2_emot_engagement ~ sex + age + y1_emot_engagement + 
    y1_PerceivedDisSch * y1_em_fit + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1 | schno_revised),
  data = dfF02,
  REML = FALSE
))
confint(model_EmFitxPercDisc_motivation_combined)
anova(model_pred_motivation, model_EmFitxPercDisc_motivation_combined)
anova(model_EmFitxPercDisc_motivation, model_EmFitxPercDisc_motivation_combined)

sim_slopes(model_EmFitxPercDisc_motivation_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

plot2_supp <- interact_plot(model_EmFitxPercDisc_motivation_combined, pred = y1_em_fit, 
                            modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                            plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                            legend.main = "Perceived Discrimination at School Wave 1")
# add a title and labels to the x and y axes
plot2_supp <- plot2_supp + labs(x = "Emotional Fit Wave 1", y = "Motivation at School Wave 2")

## BEHAVIORAL ENGAGEMENT ----------
dfF03<- df[which(complete.cases(df[,c("sex", "age", "y2_behav_engage_short", "y1_behav_engage_short", "y1_PerceivedDisSch", "y1_em_fit" )])),]

# Centering with misty package
dfF03$y1_em_fit <- misty::center(dfF03$y1_em_fit, type = c("CGM"))
dfF03$age <- misty::center(dfF03$age, type = c("CGM"))
dfF03$y1_PerceivedDisSch <- misty::center(dfF03$y1_PerceivedDisSch, type = c("CGM"))
dfF03$y1_behav_engage_short <- misty::center(dfF03$y1_behav_engage_short, type = c("CGM"))

### H1 ----
summary(model_null_beheng <- lmerTest::lmer(y2_behav_engage_short ~ (1|schno_revised), data = dfF03, REML=F))
confint(model_null_beheng)
summary(model_control_beheng <- lmerTest::lmer(y2_behav_engage_short ~ sex + age + y1_behav_engage_short+ (1|schno_revised), data = dfF03, REML=F))
confint(model_control_beheng)
summary(model_pred_beheng <- lmerTest::lmer(y2_behav_engage_short ~ sex + age + y1_em_fit + y1_behav_engage_short + (1|schno_revised), data = dfF03, REML=F))
confint(model_pred_beheng)

#model comparison
anova(model_null_beheng, model_control_beheng) 
anova(model_control_beheng, model_pred_beheng)

### H2 ----
summary(model_EmFitxPercDisc_beheng <- lmerTest::lmer(
  y2_behav_engage_short ~ sex + age + y1_behav_engage_short + 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF03, 
  REML=F))
confint(model_EmFitxPercDisc_beheng)
anova(model_pred_beheng, model_EmFitxPercDisc_beheng)

sim_slopes(model_EmFitxPercDisc_beheng, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

###VISUALIZATION----
plot3 <- interact_plot(model_EmFitxPercDisc_beheng, pred = y1_em_fit, 
                       modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                       plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                       legend.main = "Perceived Discrimination at School Wave 1")
# modify the plot's theme to match APA style guidelines
plot3 <- plot3 + theme_bw() +
  theme(text = element_text(family = "Arial", size = 11)) +
  theme(axis.title = element_text(size = 11)) + theme(plot.title = element_text(size = 11)) +
  theme(axis.text = element_text(size = 11)) + theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) + theme(
    axis.line = element_line(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
# add a title and labels to the x and y axes
plot3 <- plot3 + labs(x = "Emotional Fit Wave 1", y = "Behavioral Engagement Wave 2")

### SUPPLEMENTARY ANALYSES AND VISUALIZATION ----
summary(model_EmFitxPercDisc_beheng_SCH <- lmerTest::lmer(
  y2_behav_engage_short ~ sex + age + y1_behav_engage_short + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1|schno_revised), 
  data = dfF03, 
  REML=F))
confint(model_EmFitxPercDisc_beheng_SCH)
anova(model_pred_beheng, model_EmFitxPercDisc_beheng_SCH)
anova(model_EmFitxPercDisc_beheng, model_EmFitxPercDisc_beheng_SCH)


summary(model_EmFitxPercDisc_beheng_combined <- lmerTest::lmer(
  y2_behav_engage_short ~ sex + age + y1_behav_engage_short+ 
    y1_PerceivedDisSch * y1_em_fit + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1|schno_revised), 
  data = dfF03, 
  REML=F))
confint(model_EmFitxPercDisc_beheng_combined)
anova(model_pred_beheng, model_EmFitxPercDisc_beheng_combined)
anova(model_EmFitxPercDisc_beheng, model_EmFitxPercDisc_beheng_combined)
anova(model_EmFitxPercDisc_beheng_SCH, model_EmFitxPercDisc_beheng_combined)

sim_slopes(model_EmFitxPercDisc_beheng_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

plot3_supp <- interact_plot(model_EmFitxPercDisc_beheng_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch, line.thickness = 1.5, plot.points = TRUE, colors = "Blues", point.size = 0.5, legend.main = "Perceived Discrimination at School Wave 1")
# add a title and labels to the x and y axes
plot3_supp <- plot3_supp + labs(x = "Emotional Fit Wave 1", y = "Behavioral Engagement Wave 2")

## BEHAVIORAL DISENGAGEMENT ----

dfF04<- df[which(complete.cases(df[,c("sex", "age", "y2_behav_disengage_short", "y1_behav_disengage_short", "y1_PerceivedDisSch", "y1_em_fit" )])),]

# Centering with misty package
dfF04$y1_em_fit <- misty::center(dfF04$y1_em_fit, type = c("CGM"))
dfF04$age <- misty::center(dfF04$age, type = c("CGM"))
dfF04$y1_PerceivedDisSch <- misty::center(dfF04$y1_PerceivedDisSch, type = c("CGM"))
dfF04$y1_behav_disengage_short <- misty::center(dfF04$y1_behav_disengage_short, type = c("CGM"))

### H1 ----
summary(model_null_behdiseng <- lmerTest::lmer(y2_behav_disengage_short ~ (1|schno_revised), data = dfF04, REML=F))
confint(model_null_behdiseng)
summary(model_control_behdiseng <- lmerTest::lmer(y2_behav_disengage_short ~ sex + age + y1_behav_disengage_short+ (1|schno_revised), data = dfF04, REML=F))
confint(model_control_behdiseng)
summary(model_pred_behdiseng <- lmerTest::lmer(y2_behav_disengage_short ~ sex + age + y1_em_fit + y1_behav_disengage_short +   (1|schno_revised), data = dfF04, REML=F))
confint(model_pred_behdiseng)

#model comparison
anova(model_null_behdiseng, model_control_behdiseng) 
anova(model_control_behdiseng, model_pred_behdiseng)

### H2 ----
summary(model_EmFitxPercDisc_behdiseng <- lmerTest::lmer(
  y2_behav_disengage_short ~ sex + age + y1_behav_disengage_short + 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF04, 
  REML=F))
confint(model_EmFitxPercDisc_behdiseng)

anova(model_pred_behdiseng, model_EmFitxPercDisc_behdiseng)

sim_slopes(model_EmFitxPercDisc_behdiseng, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

#VISUALIZATION
plot4 <- interact_plot(model_EmFitxPercDisc_behdiseng, pred = y1_em_fit, 
                       modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                       plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                       legend.main = "Perceived Discrimination at School Wave 1")
# modify the plot's theme to match APA style guidelines
plot4 <- plot4 + theme_bw() +
  theme(text = element_text(family = "Arial", size = 11)) +
  theme(axis.title = element_text(size = 11)) + theme(plot.title = element_text(size = 11)) +
  theme(axis.text = element_text(size = 11)) + theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) + theme(
    axis.line = element_line(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
# add a title and labels to the x and y axes
plot4 <- plot4 + labs(x = "Emotional Fit Wave 1", y = "Behavioral Disengagement Wave 2")

### SUPPLEMENTARY ANALYSES AND VISUALIZATION ----
summary(model_EmFitxPercDisc_behdiseng_SCH <- lmerTest::lmer(
  y2_behav_disengage_short ~ sex + age + y1_behav_disengage_short + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1|schno_revised), 
  data = dfF04, 
  REML=F))
confint(model_EmFitxPercDisc_behdiseng_SCH)
anova(model_pred_behdiseng, model_EmFitxPercDisc_behdiseng_SCH)
anova(model_EmFitxPercDisc_behdiseng, model_EmFitxPercDisc_behdiseng_SCH)


summary(model_EmFitxPercDisc_behdiseng_combined <- lmerTest::lmer(
  y2_behav_disengage_short ~ sex + age + y1_behav_disengage_short+ 
    y1_PerceivedDisSch * y1_em_fit + 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1|schno_revised), 
  data = dfF04, 
  REML=F))
confint(model_EmFitxPercDisc_behdiseng_combined)
anova(model_pred_behdiseng, model_EmFitxPercDisc_behdiseng_combined)
anova(model_EmFitxPercDisc_behdiseng, model_EmFitxPercDisc_behdiseng_combined)
anova(model_EmFitxPercDisc_behdiseng_SCH, model_EmFitxPercDisc_behdiseng_combined)

sim_slopes(model_EmFitxPercDisc_behdiseng_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

plot4_supp <- interact_plot(model_EmFitxPercDisc_behdiseng_combined, pred = y1_em_fit, 
                            modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                            plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                            legend.main = "Perceived Discrimination at School Wave 1")
# add a title and labels to the x and y axes
plot4_supp <- plot4_supp + labs(x = "Emotional Fit Wave 1", y = "Behavioral Disengagement Wave 2")

## NONCOMPLIANCE AT SCHOOL ----------
dfF05<- df[which(complete.cases(df[,c("sex", "age", "y2_sch_noncompl", "y1_sch_noncompl", "y1_PerceivedDisSch", "y1_em_fit" )])),]

# Centering with misty package
dfF05$y1_em_fit <- misty::center(dfF05$y1_em_fit, type = c("CGM"))
dfF05$age <- misty::center(dfF05$age, type = c("CGM"))
dfF05$y1_PerceivedDisSch <- misty::center(dfF05$y1_PerceivedDisSch, type = c("CGM"))
dfF05$y1_sch_noncompl <- misty::center(dfF05$y1_sch_noncompl, type = c("CGM"))

### H1 ----
summary(model_null_noncb <- lmerTest::lmer(y2_sch_noncompl ~ (1|schno_revised), data = dfF05, REML=F))
confint(model_null_noncb)

summary(model_control_noncb <- lmerTest::lmer(y2_sch_noncompl ~ sex + age + y1_sch_noncompl+ (1|schno_revised), data = dfF05, REML=F))
confint(model_control_noncb)

summary(model_pred_noncb <- lmerTest::lmer(y2_sch_noncompl ~ sex + age + y1_em_fit + y1_sch_noncompl + (1|schno_revised), data = dfF05, REML=F))
confint(model_pred_noncb)

#model comparison
anova(model_null_noncb, model_control_noncb) 
anova(model_control_noncb, model_pred_noncb)

### H2 ----
summary(model_EmFitxPercDisc_noncb <- lmerTest::lmer(
  y2_sch_noncompl ~ sex + age + y1_sch_noncompl+ 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF05, 
  REML=F))
confint(model_EmFitxPercDisc_noncb)
anova(model_pred_noncb, model_EmFitxPercDisc_noncb)

sim_slopes(model_EmFitxPercDisc_noncb, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

#VISUALIZATION
plot5 <- interact_plot(model_EmFitxPercDisc_noncb, pred = y1_em_fit, 
                       modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                       plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                       legend.main = "Perceived Discrimination at School Wave 1")
# modify the plot's theme to match APA style guidelines
plot5 <- plot5 + theme_bw() +
  theme(text = element_text(family = "Arial", size = 11)) +
  theme(axis.title = element_text(size = 11)) + theme(plot.title = element_text(size = 11)) +
  theme(axis.text = element_text(size = 11)) + theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) + theme(
    axis.line = element_line(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
# add a title and labels to the x and y axes
plot5 <- plot5 + labs(x = "Emotional Fit Wave 1", y = "Non-Compliant Behavior Wave 2")

### SUPPLEMENTARY ANALYSES AND VISUALIZATION ----
summary(model_EmFitxPercDisc_noncb_SCH <- lmerTest::lmer(
  y2_sch_noncompl ~ sex + age + y1_sch_noncompl+ 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    (1|schno_revised), 
  data = dfF05, 
  REML=F))
confint(model_EmFitxPercDisc_noncb_SCH)
anova(model_pred_noncb, model_EmFitxPercDisc_noncb_SCH)
anova(model_EmFitxPercDisc_noncb, model_EmFitxPercDisc_noncb_SCH)

summary(model_EmFitxPercDisc_noncb_combined <- lmerTest::lmer(
  y2_sch_noncompl ~ sex + age + y1_sch_noncompl+ 
    y1_SCHDISTMIN_PercMC_correctvariable * y1_em_fit + 
    y1_PerceivedDisSch * y1_em_fit + 
    (1|schno_revised), 
  data = dfF05, 
  REML=F))
confint(model_EmFitxPercDisc_noncb_combined)
anova(model_pred_noncb, model_EmFitxPercDisc_noncb_combined)
anova(model_EmFitxPercDisc_noncb, model_EmFitxPercDisc_noncb_combined)
anova(model_EmFitxPercDisc_noncb_SCH, model_EmFitxPercDisc_noncb_combined)

sim_slopes(model_EmFitxPercDisc_noncb_combined, pred = y1_em_fit, modx = y1_PerceivedDisSch, confint = TRUE)

plot5_supp <- interact_plot(model_EmFitxPercDisc_noncb_combined, pred = y1_em_fit, 
                            modx = y1_PerceivedDisSch, line.thickness = 1.5, 
                            plot.points = TRUE, colors = "Blues", point.size = 0.5, 
                            legend.main = "Perceived Discrimination at School Wave 1")
# add a title and labels to the x and y axes
plot5_supp <- plot5_supp + labs(x = "Emotional Fit Wave 1", y = "Non-Compliant Behavior Wave 2")

## COMBINED PLOTS ----
### MAIN ANALYSES ----
# Remove the legends from the individual plots
plot2_no_legend <- plot2 + theme(legend.position = "none")
plot3_no_legend <- plot3 + theme(legend.position = "none")
plot4_no_legend <- plot4 + theme(legend.position = "none")
plot5_no_legend <- plot5 + theme(legend.position = "none")

# Create labels for the plots
plot2_labeled <- plot2_no_legend + ggtitle("A")
plot3_labeled <- plot3_no_legend + ggtitle("B")
plot4_labeled <- plot4_no_legend + ggtitle("C")
plot5_labeled <- plot5_no_legend + ggtitle("D")

# Generate the legend separately
legend <- get_legend(plot2)  

# Arrange the plots in a 2x2 layout and add the legend on top
grid.arrange(
  grobs = list(plot2_labeled, plot3_labeled, plot4_labeled, plot5_labeled),
  ncol = 2,
  nrow = 2,
  heights = c(1, 1),  
  top = legend  
)

### SUPPLEMENTARY ANALYSES ----
# First, remove the legends from the individual plots
plot2_supp_no_legend <- plot2_supp + theme(legend.position = "none")
plot3_supp_no_legend <- plot3_supp + theme(legend.position = "none")
plot4_supp_no_legend <- plot4_supp + theme(legend.position = "none")
plot5_supp_no_legend <- plot5_supp + theme(legend.position = "none")

# Create labels for the plots
plot2_supp_labeled <- plot2_supp_no_legend + ggtitle("A")
plot3_supp_labeled <- plot3_supp_no_legend + ggtitle("B")
plot4_supp_labeled <- plot4_supp_no_legend + ggtitle("C")
plot5_supp_labeled <- plot5_supp_no_legend + ggtitle("D")

# Arrange the plots in a 2x2 grid
# Use grid.arrange to combine the plots
# First, we'll generate the legend separately
legend <- get_legend(plot2_supp)  # Get the legend from one of the plots (e.g., plot2)

# Arrange the plots in a 2x2 layout and add the legend on top
grid.arrange(
  grobs = list(plot2_supp_labeled, plot3_supp_labeled, plot4_supp_labeled, plot5_supp_labeled),
  ncol = 2,
  nrow = 2,
  heights = c(1, 1),  # Adjust the height ratio to create space for the legend
  top = legend  # Place the legend on top
)
