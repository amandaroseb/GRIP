####Heteroskedasticity####
library(car)
library(nlme)

#See 'CGSL_Revised.R' for import scripts to create data: 
#ASL_fins, ENG_fins, Both_fins and Both_none

#Levene's test for homogeneity of variance
test <- leveneTest(maxGrip ~ stick, Both_fins)

mod <- lm(maxGrip ~ stick, Both_fins)
sticks <- Both_fins %>%
  group_by(stick) %>%
  summarise(var = var(maxGrip))

apply(sticks, 2, function(x){ var(x, na.rm=T) })

#linear models are fairly robust to heterogeneity of variance#
#so long as the maximum variance is no more than 4X greater than the minimum variance#
#Our prop = 1.94

leveneTest(maxGrip ~ stick, Both_none)

mod <- lm(maxGrip ~ stick, Both_none)
sticks <- Both_none %>%
  group_by(stick) %>%
  summarise(var = var(maxGrip))

#Our prop = 1.86

##Comparing 3 tasks
compare3_tasks_weight <- function(data, string) {
  data$type <- factor(data$type, levels = c("DESCRIPTION", "ACTION", "ESTIMATION"))
  input <<- data
  mod_Task <<- lme(maxGrip ~ stickcmCentered * fins*type, random=~1 + stickcmCentered*fins | obsisSubj, weights = varIdent(form =~ 1 | stickcmCentered), data = input)
  results <<- summary(mod_Task)
  print(results)
}

compare3_tasks_Noweight <- function(data, string) {
  data$type <- factor(data$type, levels = c("DESCRIPTION", "ACTION", "ESTIMATION"))
  input <<- data
  mod_Task <<- lme(maxGrip ~ stickcmCentered * fins*type, random=~1 + stickcmCentered | obsisSubj, data = input)
  results <<- summary(mod_Task)
  print(results)
}
compare3_tasks_weight(ENG_fins, "Speakers")
mods1weight <- mod_Task

compare3_tasks_Noweight(ENG_fins, "Speakers")
mods2noweight <- mod_Task

anova(mods1weight, mods2noweight)

compare3_tasks_weight(ASL_fins, "Signers")
mods1weight <- mod_Task

compare3_tasks_Noweight(ASL_fins, "Signers")
mods2noweight <- mod_Task

anova(mods1weight, mods2noweight)

anova(mod_Task)
plot(ranef(mods1weight))
var1 <- varFunc(~stickcmCentered)
summary(var1)
