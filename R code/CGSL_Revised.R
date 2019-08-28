#### Revised MocapGrip Analysis ####
####Packages####
library(tidyverse)
library(lme4)
library(glmm)
library(effects)
library(broom)
library(lmerTest)
library(plotly)
library(ggbeeswarm)


####Function to find file paths and import files####
import_files_fins <- function(string) {
  files <<- dir("data/", pattern = string, full.names = TRUE)
  map(files, read_csv) %>%
    bind_rows() %>%
    type_convert() %>%
    filter(fins != "none") %>%
    mutate(Lcat = string, fins = factor(fins), type = factor(type))%>%
    mutate(type=recode(type, GESTURE = "DESCRIPTION"))
}


####Import Files with open/closed fins####
ASL_fins <- import_files_fins("ASL")

ENG_fins <- import_files_fins("ENG") 

Both_fins <- bind_rows(ASL_fins, ENG_fins)%>%
  mutate(Lcat=recode(Lcat, ASL="Signers", ENG="Speakers"))


####Function to find file paths and import files####
import_files_none <- function(string) {
  files <<- dir("data/", pattern = string, full.names = TRUE)
  map(files, read_csv) %>%
    bind_rows() %>%
    type_convert() %>%
    filter(fins == "none") %>%
    mutate(Lcat = string, fins = factor(fins), type = factor(type))%>%
    mutate(type=recode(type, GESTURE = "DESCRIPTION"))
}


####Import Files with no fins####
ASL_none <- import_files_none("ASL")

ENG_none <- import_files_none("ENG") 

Both_none <- bind_rows(ASL_none, ENG_none)

Both_none <- Both_none %>%
  mutate(Lcat=recode(Lcat, ASL="Signers", ENG="Speakers"))

####Linear Models compared for data analysis####
#fullmodel <- lmerTest::lmer(maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj), data = ASL_fins)
#emptymodel <- lmer(maxGrip ~ (1 | obsisSubj), data = data)
#nofinsmodel <- lmer(maxGrip ~ stickcmCentered + (1 + stickcmCentered | obsisSubj), data = data)
#norandomfinsmodel <- lmer(maxGrip ~ stickcmCentered*fins + (1 + stickcmCentered | obsisSubj), data = data)

#### Required Function for model fitting####
pred <- function(fitModel) {
  
  # create a data frame with various levels represented

  
    vars <- stringr::str_split(as.character(fitModel@call$formula), stringr::fixed(" "))
  outcomeVarName <- vars[[2]]
  allPredictors <- attributes(stats::terms(fitModel@call$formula))$term.labels
  isInteraction <- stringr::str_detect(attributes(stats::terms(fitModel@call$formula))$term.labels, ":")
  isLevel <- stringr::str_detect(attributes(stats::terms(fitModel@call$formula))$term.labels, "\\|")
  predictorVars <- subset(allPredictors, !isInteraction & !isLevel)
  levelVars <- subset(allPredictors, isLevel)
  levelVars <- stringr::str_replace(levelVars, ".* \\| ", "") # remove all slope levels
  
  
  newdat <- fitModel@frame
  newdat[,outcomeVarName] <- NA
  newdat <- unique(newdat)
  
  # generate model values
  mm<-stats::model.matrix(stats::terms(fitModel),newdat)
  
  # generate predictions
  newdat[,outcomeVarName] <- (mm %*% lme4::fixef(fitModel))
  
  # generate variance from the fixed effects (add in random effects?)
  newdat$pvar1 <- diag(mm %*% tcrossprod(stats::vcov(fitModel),mm))
  
  # add the random effects into the estimate
  for(var in levelVars){
    #     print(ranef(fitModel)[[var]][as.character(newdat[,var]),'(Intercept)'])
    newdat[,outcomeVarName] <- newdat[,outcomeVarName]+lme4::ranef(fitModel)[[var]][as.character(newdat[,var]),'(Intercept)']
    
  }
  
  # make high and low estimates
  newdat <- data.frame(
    newdat
    , plo = newdat[,outcomeVarName]-2*sqrt(newdat$pvar1)
    , phi = newdat[,outcomeVarName]+2*sqrt(newdat$pvar1)
  )
  
  # collapse groups that are irrelevant using ddply(...,summarise,...) (ver_letter removed!)
  
  eval(parse(text = paste0("data.modeled <- plyr::ddply(newdat, predictorVars, plyr::summarise, ", outcomeVarName, " = mean(", outcomeVarName,"), plo=mean(plo), phi=mean(phi))")))
}


####Functions for analysis####
####Examining One Task in each Language Group####
onegroup_function <- function(data, string) {
  group <- data$Lcat[1]
  input <<- filter(data, type == string)
  model <<- lmer(maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj), data = input)
  mod_Action1 <- model
  results <<- summary(mod_Action1)
  print(results)
}

onegroup_function_graph <- function(data, string) {
  group <- data$Lcat[1]
  input <<- filter(data, type == string)
  model <<- lme4::lmer(maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj), data = input)
  mod_Action1 <- model
  results <<- summary(mod_Action1)
  print(results)
  graph1 <- ggplot(pred(mod_Action1)) + aes(x=stickcmCentered+8, y=maxGrip, ymin=plo, ymax=phi, group=fins, color=fins) + 
    geom_pointrange(position = position_dodge(width=0.5)) + 
    labs(title = paste0("Model predictions for ", string, ": ", group), 
         x="stick size (in cm)", y="the maximum grip aperture (in mm)")
  ggsave(graph1, file = paste0("graph", string, group, ".pdf"))
  print(graph1)
  eff_Action1 <- effect(c("stickcmCentered:fins"), mod_Action1)
  graph2 <<- ggplot(as.data.frame(eff_Action1), 
                   aes(x=(stickcmCentered+8)*10, y=fit, color=fins)) +
    geom_line()+ 
    labs(title = paste0("Model predictions for ", string, ": ", group), 
         x="stick size (mm)", y="the maximum grip aperture (mm)")
  ggsave(graph2, file = paste0("linegraph", string, group, ".pdf"))
  print(graph2)
}

onegroup_function(ENG_fins, "ACTION")
onegroup_function(ENG_fins, "ESTIMATION")
onegroup_function(ENG_fins, "DESCRIPTION")
onegroup_function(ASL_fins, "ACTION")
onegroup_function(ASL_fins, "ESTIMATION")
onegroup_function(ASL_fins, "DESCRIPTION")

####Comparing Language Groups on a Single Task####
compare_groups_graphs <- function(data, string) {
  data$Lcat <- factor(data$Lcat)
  input <<- filter(data, type == string)
  mod_Action3 <<- lme4::lmer(maxGrip ~ stickcmCentered * fins*Lcat + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Action3)
  print(results)
  graph1 <- ggplot(pred(mod_Action3)) + 
    aes(x=(stickcmCentered+8)*10, y=maxGrip, ymin=plo, ymax=phi, group=Lcat, color=fins) + 
    geom_pointrange(position = position_dodge(width=0.5)) + 
    labs(title = paste0("Model predictions for ", string, ": Compare Groups"), 
         x="stick size (in cm)", y="the maximum grip aperture (in mm)")
  print(graph1)
  eff_Action3 <- effect(c("stickcmCentered:fins:Lcat"), mod_Action3)
  graph2 <- ggplot(as.data.frame(eff_Action3), 
                    aes(x=stickcmCentered+8, y=fit, color=fins)) +
  geom_line(aes(color=fins)) +
  labs(title = paste0("Model predictions for ", string, ": Compare Groups"), 
       x="stick size (mm)", 
       y="the maximum grip aperture (mm)") +
    facet_wrap(~Lcat)
  print(graph2)
}

compare_groups_graphs(Both_fins, "ACTION")

compare_groups <- function(data, string) {
  data$Lcat <- factor(data$Lcat)
  input <<- filter(data, type == string)
  mod_Action3 <<- lmer(maxGrip ~ stickcmCentered * fins*Lcat + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Action3)
  print(results)
}

compare_groups(Both_fins, "ACTION")
compare_groups(Both_fins, "ESTIMATION")
compare_groups(Both_fins, "DESCRIPTION")
compare_groups_graphs(Both_fins, "DESCRIPTION")


####Comparing 3 tasks within Language Group####
compare3_tasks <- function(data, string) {
  data$type <- factor(data$type, levels = c("DESCRIPTION", "ACTION", "ESTIMATION"))
  input <<- data
  mod_Task <<- lmer(maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Task)
  print(results)
}

compare3_tasks(ENG_fins, "Speakers")
compare3_tasks(ASL_fins, "Signers")

range_maxGrip <- ASL_fins %>%
  group_by(type)%>%
  summarise(min=min(maxGrip), max=max(maxGrip))%>%
  mutate(diff=max-min)

range_maxGrip <- ENG_fins %>%
  group_by(type)%>%
  summarise(min=min(maxGrip), max=max(maxGrip))%>%
  mutate(diff=max-min)

####Graphing 3 Tasks within Language Group####
compare3_tasks_graphs <- function(data, string) {
  data$type <- factor(data$type, levels = c("DESCRIPTION", "ACTION", "ESTIMATION"))
  input <<- data
  mod_Task <<- lme4::lmer(maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Task)
  print(results)
  graph1 <- ggplot(pred(mod_Task)) +
    aes((stickcmCentered+8)*10, y=maxGrip, ymin=plo, ymax=phi, group=type, color=fins) +
    geom_pointrange(position = position_dodge(width=0.5)) +
    labs(title = paste0("Model predictions for ", string, ": Compare"),
         x="stick size (mm)", y="the maximum grip aperture (mm)")
  print(graph1)
  eff_Task <<- effect(c("stickcmCentered:fins:type"), mod_Task)
  graph2 <- ggplot() +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = "dotted", color="gray46")+
  geom_line(as.data.frame(eff_Task), mapping = aes(x=(stickcmCentered+8)*10, y=fit, color=fins))+
    geom_jitter(data, mapping = aes(x = stickcm*10, y = maxGrip, group = fins), size= 0.3, alpha = .1)+
  labs(title = paste("Model predictions for Comparing Tasks:", string),
         x="stick size (mm)", y="the maximum grip aperture (mm)") +
  facet_wrap(~type)
  ggsave(graph2, file = paste0("Compare3", string, ".jpg"))
  print(graph2)
}


#### Graphing 3-task plot for both Language Groups in One Plot####
compare3_tasks_graphs(ENG_fins, "Speakers")

eff_Task_sp <- as.data.frame(eff_Task) %>%
  mutate(Lcat = "Speakers")


compare3_tasks_graphs(ASL_fins, "Signers")

eff_Task_si <-as.data.frame(eff_Task) %>%
  mutate(Lcat = "Signers")


all_graph <- ggplot() +
  geom_line(as.data.frame(eff_Task_si), mapping = aes(x=(stickcmCentered+8)*10, y=fit, color=fins))+
  geom_line(as.data.frame(eff_Task_sp), mapping = aes(x=(stickcmCentered+8)*10, y=fit, color=fins))+
  geom_jitter(Both_fins, mapping = aes(x = stickcm*10, y = maxGrip, color = fins), size= 0.3, alpha = .3)+
  labs(title = "Model predictions for Comparing Tasks", x="stick size (mm)", y="the maximum grip aperture (mm)") +
  facet_grid(Lcat ~ type)

print(all_graph)

####Comparing 2 tasks within Language Group####
compare2_tasks <- function(data, string, task1, task2) {
  data$type <- factor(data$type)
  tasks <- c(task1, task2)
  input <<- filter(data, type %in% tasks)
  mod_Task <<- lmer(maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Task)
  print(results)
}

compare2_tasks(ENG_fins, "Speakers", "ACTION", "ESTIMATION")

compare2_tasks(ASL_fins, "Signers", "ACTION", "ESTIMATION")

compare2_tasks_graphs <- function(data, string, task1, task2) {
  data$type <- factor(data$type)
  tasks <- c(task1, task2)
  input <<- filter(data, type %in% tasks)
  mod_Task <<- lme4::lmer(maxGrip ~ stickcmCentered * fins*type + (1 + stickcmCentered * fins | obsisSubj), data = input)
  results <<- summary(mod_Task)
  print(results)
  graph1 <- ggplot(pred(mod_Task)) + 
    aes((stickcmCentered+8)*10, y=maxGrip, ymin=plo, ymax=phi, group=type, color=fins) + 
    geom_pointrange(position = position_dodge(width=0.5)) + 
    labs(title = paste0("Model predictions for ", string, ": Compare"), 
         x="stick size (mm)", y="the maximum grip aperture (mm)")
  eff_Task <- effect(c("stickcmCentered:fins:type"), mod_Task)
  print(graph1)
  graph2 <<- ggplot(as.data.frame(eff_Task), 
                    aes(x=(stickcmCentered+8)*10, y=fit, color=fins)) +
    geom_line(aes(color=fins))+ 
    labs(title = paste("Model predictions for Comparing Tasks:", string), 
         x="stick size (mm)", y="the maximum grip aperture (mm)") +
    facet_wrap(~type)
  print(graph2)
}

compare2_tasks_graphs(ENG_fins, "Speakers", "ACTION", "ESTIMATION")

compare2_tasks_graphs(ASL_fins, "Signers", "ACTION", "ESTIMATION")

