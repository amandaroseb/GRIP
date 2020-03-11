##Testing assumptions of HLM Model##
library(car)
library(lattice)

##Testing Linearity - plot model residuals against outcome variable 'maxGrip'
##Testing Heteroscedaticity - model squared residuals predicted by subjects? sticks?
#and test for significant differences using Levene's test
##Testing normality of residuals - plot residuals versus fitted
#One group and task at a time
#Using the model: lme4::lmer(maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj), data = input)


#Action - Speakers
eng_action <- onegroup_function(ENG_fins, "ACTION")
plot(model) #Equal variance? Even distribution around the center line?
ENG_A$residuals <- residuals(model)
ENG_A_test <- ENG_A %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
plot_resid <- plot(resid(model), ENG_A$maxGrip) #Is it linear? Random distribution?
qqmath(model, id = 0.05) #Normally distributed residuals?
data <- ENG_A_test
levene_model <- lm(sqr~stickcmCentered, data = data)
anova(levene_model)

#Estimation  - Speakers
eng_estim <- onegroup_function(ENG_fins, "ESTIMATION")
plot(model)
ENG_E$residuals <- residuals(model)
ENG_E_test <- ENG_E %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
plot_resid <- plot(resid(model), ENG_E$maxGrip)
qqmath(model, id = 0.05)
data <- ENG_E_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)

#Description  - Speakers
eng_descr <- onegroup_function(ENG_fins, "DESCRIPTION")
plot(model)
ENG_D$residuals <- residuals(model)
ENG_D_test <- ENG_D %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
plot_resid <- plot(resid(model), ENG_D$maxGrip)
qqmath(model, id = 0.05)
data <- ENG_D_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)

#Action - Signers
asl_action <- onegroup_function(ASL_fins, "ACTION")
plot(model)
ASL_A$residuals <- residuals(model)
ASL_A_test <- ASL_A %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
plot_resid <- plot(resid(model), ASL_A$maxGrip)
qqmath(model, id = 0.05)
data <- ASL_A_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)

#Estimation  - Signers
asl_estim <- onegroup_function(ASL_fins, "ESTIMATION")
plot_resid <- plot(resid(model), ASL_E$maxGrip)
ASL_E$residuals <- residuals(model)
ASL_E_test <- ASL_E %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
qqmath(model, id = 0.05)
data <- ASL_E_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)

#Description  - Signers
asl_descr <- onegroup_function(ASL_fins, "DESCRIPTION")
plot_resid <- plot(resid(model), ASL_D$maxGrip)
ASL_D$residuals <- residuals(model)
ASL_D_test <- ASL_D %>%
  mutate(abs = abs(residuals),
         sqr = abs^2)
qqmath(model, id = 0.05)
data <- ASL_D_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)


#Comparing Tasks
#Model: mod_Task <<- lmer(maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = input)

#Action-Estimation comparison
eng_act_est <- compare2_tasks(ENG_fins, "Speakers", "ACTION", "ESTIMATION")
plot(mod_Task)
plot_resid <- plot(resid(mod_Task), input$maxGrip)
qqmath(mod_Task, id = 0.05)
input_test <- input %>%
  mutate(residuals = residuals(mod_Task),
         abs = abs(residuals),
         sqr = abs^2)
data = input_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)
  

asl_act_est <- compare2_tasks(ASL_fins, "Signers", "ACTION", "ESTIMATION")
plot(mod_Task)
plot_resid <- plot(resid(mod_Task), input$maxGrip)
qqmath(mod_Task, id = 0.05)
input_test <- input %>%
  mutate(residuals = residuals(mod_Task),
         abs = abs(residuals),
         sqr = abs^2)
data = input_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)

#Action-Estimation-Description comparison
eng_3task <- compare3_tasks(ENG_fins, "Speakers")
plot(mod_Task)
plot_resid <- plot(resid(mod_Task), input$maxGrip)
qqmath(mod_Task, id = 0.05)
input_test <- input %>%
  mutate(residuals = residuals(mod_Task),
         abs = abs(residuals),
         sqr = abs^2)
data = input_test
levene_model <- lm(sqr~obsisSubj, data = data)
anova(levene_model)
  
asl_3task <- compare3_tasks(ASL_fins, "Signers")

##Using summarized data 
data_anova_3 <- Both_fins %>%
  group_by(obsisSubj, Lcat, type, stick, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))
anova1 <- aov(avg_maxGrip ~ Lcat*type, data = data_anova_3)
Anova(anova1, type = "III")

##Using Summarized data to test significance for ACT-EST differences between groups
ACT_EST <- bind_rows(ASL_A, ASL_E, ENG_A, ENG_E)
data_anova_2 <- ACT_EST %>%
  group_by(obsisSubj, Lcat, type, stick, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))
anova2 <- aov(avg_maxGrip ~ Lcat*type, data = data_anova_2)
Anova(anova2, type = "III")
#Action v Estimation in both Language groups - only types differ, not languages

ACT_EST_ASL <- bind_rows(ASL_A, ASL_E)
data_anova_2_1 <- ACT_EST_ASL %>%
  group_by(obsisSubj, Lcat, type, stick, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))
anova2_1 <- aov(avg_maxGrip ~ type*fins, data = data_anova_2_1)
Anova(anova2_1, type = "III")



ACT_EST_ENG <- bind_rows(ENG_A, ENG_E)
data_anova_2_2 <- ACT_EST_ENG %>%
  group_by(obsisSubj, Lcat, type, stick, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))
anova2_2 <- aov(avg_maxGrip ~ type*fins, data = data_anova_2_2)
Anova(anova2_2, type = "III")


data_anova_4 <- ENG_fins %>%
  group_by(obsisSubj, type, stick, stickcmCentered, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))
anova4 <- aov(avg_maxGrip ~ type*fins, data = data_anova_4)
Anova(anova4, type = "III")

data_anova_5 <- ASL_fins %>%
  group_by(obsisSubj, type, stickFiveRef, stickcmCentered, fins) %>%
  summarise(avg_maxGrip = mean(maxGrip))

anova5 <- aov(avg_maxGrip ~ type*fins, data = data_anova_5)
Anova(anova5, type = "III")

lm2 <- lmer(avg_maxGrip ~ type*stickFiveRef + type * fins +(1+ stickFiveRef |obsisSubj), data = data_anova_5)
summary(lm2)
#lmer(maxGrip ~ +1 + type*stickcmCentered + type*fins + fins*stickcmCentered + (1 + stickcmCentered + fins + type | obsisSubj)