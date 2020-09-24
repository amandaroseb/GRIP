#Re-do models using summarized data
library(car)
library(lattice)

lm1 <- lmer(avg_maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = data_anova_4)

lm2 <- lmer(avg_maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered + fins + type | obsisSubj), data = data_anova_4)

lm3 <- lmer(avg_maxGrip ~ type*stick + type * fins +(1+ stick |obsisSubj), data = data_anova_4)

model <- lm1
data <- data_anova_4

mod_test <- function(model, data){
  summary(model)
  plot(model)
  plot_resid <- plot(resid(model), data$avg_maxGrip)
  qqmath(model, id = 0.05)
  data$residuals <- residuals(model)
  data <- data %>%
    mutate(abs = abs(residuals),
           sqr = abs^2)
  levene_model <- lm(sqr~obsisSubj, data = data)
  anova(levene_model)
}

mod_test(lm1, data_anova_4)
