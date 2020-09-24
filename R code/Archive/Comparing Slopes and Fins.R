####Comparing slopes and fins####
####Packages####
library(lmerTest)

####Plotting relations of grip aperture slope to illusion effect####

slope_function <- function(data, string, group){
  name <<- paste0(group, "_", string)
  input <<- filter(data, type == string)
  mod_Gesture1 <<- lmer(maxGrip ~ stickcmCentered * fins + (1 + stickcmCentered * fins | obsisSubj), data = input)
  summary(mod_Gesture1)
  coef(summary(mod_Gesture1))
  fix <- fixef(mod_Gesture1)
  print(c(fix[2], fix[3]))
  slopes <- ranef(mod_Gesture1)
  slopes <- as.data.frame(slopes$obsisSubj)
  slopes$obsisSubj <- rownames(slopes)
  slopes$stick <- slopes$stickcmCentered + fix[2]
  slopes$illusion <- slopes$finsopen + fix[3]
  slopes$task <- string
  assign(quo_name(enquo(name)), slopes, envir=.GlobalEnv)
}

Speakers_GESTURE <- slope_function(ENG_fins, "DESCRIPTION", "Speakers")
Speakers_ACTION <- slope_function(ENG_fins, "ACTION", "Speakers")
Speakers_ESTIMATION <- slope_function(ENG_fins, "ESTIMATION", "Speakers")

Signers_GESTURE <- slope_function(ASL_fins, "DESCRIPTION", "Signers")
Signers_ACTION <- slope_function(ASL_fins, "ACTION", "Signers")
Signers_ESTIMATION <- slope_function(ASL_fins, "ESTIMATION", "Signers")

Signers <- rbind(Signers_ACTION, Signers_ESTIMATION, Signers_GESTURE)
Speakers <- rbind(Speakers_ACTION, Speakers_ESTIMATION, Speakers_GESTURE)

####Function for slopes predicted by no illusion####
slope_function_none <- function(data, string, group){
  name <<- paste0(group, "_", string)
  col_name <- enquo(name)
  input <- filter(data, type == string)
  mod_Gesture1 <<- lmer(maxGrip ~ stickcmCentered + (1 + stickcmCentered | obsisSubj), data = input)
  summary(mod_Gesture1)
  coef(summary(mod_Gesture1))
  fix <- fixef(mod_Gesture1)
  print(fix[2])
  slopes <- ranef(mod_Gesture1)
  slopes <- as.data.frame(slopes$obsisSubj)
  slopes$obsisSubj <- rownames(slopes)
  slopes$slope <- slopes$stickcmCentered + fix[2]
  slopes$task <- string
  #slopes$illusion <- "none"
  #slopes <- slopes %>% rename(!!col_name := slope)
  assign(quo_name(enquo(name)), slopes, envir=.GlobalEnv)
}

ENG_none_GESTURE <- slope_function_none(ENG_none, "DESCRIPTION", "ENG_none")
ENG_none_ACTION <- slope_function_none(ENG_none, "ACTION", "ENG_none")
ENG_none_ESTIMATION <- slope_function_none(ENG_none, "ESTIMATION", "ENG_none")

ASL_none_GESTURE <- slope_function_none(ASL_none, "DESCRIPTION", "ASL_none")
ASL_none_ACTION <- slope_function_none(ASL_none, "ACTION", "ASL_none")
ASL_none_ESTIMATION <- slope_function_none(ASL_none, "ESTIMATION", "ASL_none")

ENG_none_slopes <- rbind(ENG_none_ACTION, ENG_none_ESTIMATION, ENG_none_GESTURE)
ASL_none_slopes <- rbind(ASL_none_ACTION, ASL_none_ESTIMATION, ASL_none_GESTURE)


#Create data frames with no illusion slopes and fins
Signers_All <- Signers %>%
  left_join(ASL_none_slopes, by = c("obsisSubj", "task")) %>%
  mutate(LCAT = "Signers")


Speakers_All <- Speakers %>%
  left_join(ENG_none_slopes, by = c("obsisSubj", "task")) %>%
  mutate(LCAT = "Speakers")

All_data <- rbind(Speakers_All, Signers_All)

#examine ranges
range_slope <- Signers_All%>%
  group_by(task)%>%
  summarise(mean=mean(slope), median=median(slope), min=min(slope), max=max(slope))%>%
  mutate(diff=max-min)

range_slope <- Speakers_All%>%
  group_by(task)%>%
  summarise(mean=mean(slope), median=median(slope), min=min(slope), max=max(slope))%>%
  mutate(diff=max-min)

range_illusion <- Signers_All %>%
  group_by(task)%>%
  summarise(mean=mean(illusion), median=median(illusion), sd=sd(illusion), min=min(illusion), max=max(illusion))%>%
  mutate(diff=max-min)

range_illusion <- Speakers_All %>%
  group_by(task)%>%
  summarise(mean=mean(illusion), median=median(illusion), sd=sd(illusion), min=min(illusion), max=max(illusion))%>%
  mutate(diff=max-min)

slope_graph <- All_data %>%
  ggplot(aes(x=slope, y=illusion))+
  geom_point(aes(color = LCAT)) +
  geom_smooth(method = "lm", aes(color = LCAT))+
  #geom_text(aes(label = obsisSubj), nudge_y = .3) +
  facet_wrap(~task) +
  scale_color_discrete(name = "Group")+
  labs(title = paste("Relation of grip aperture slopes to illusion effect"), 
       x = "Slope", y = "Illusion effect")
print(slope_graph)

####Graphing slopes_none by fins_open per task and group####
#Log transformation
graph_function_task_log <- function(task){
  task_text <- enquo(task)
  slope_graph <- All_data %>%
    filter(task == !!task_text) %>%
    ggplot(aes(x=log(slope), y=log(illusion)))+
    geom_point() +
    geom_smooth(method = "lm")+
    #scale_color_discrete(name = "Group")+
    geom_abline(aes(intercept = 0, slope = 1))+
    geom_text(aes(label = obsisSubj), nudge_y = .3) +
    #facet_wrap(~task) +
    labs(title = paste("Relation of grip aperture slopes to illusion effect:", task), 
         x = "Log of Slope", y = "Log of Illusion effect")
  print(slope_graph)
}

#graph_function_task_log("DESCRIPTION")
#graph_function_task("ACTION")
#graph_function_task("ESTIMATION")

graph_function_task <- function(task){
  task_text <- enquo(task)
  slope_graph <<- All_data %>%
    filter(task == !!task_text) %>%
    ggplot(aes(x=slope, y=illusion))+
    geom_point(aes(color = LCAT)) +
    geom_smooth(method = "lm", aes(color = LCAT))+
    scale_color_discrete(name = "Group")+
    #geom_text(aes(label = obsisSubj), nudge_y = .3) +
    #facet_wrap(~task) +
    labs(title = paste("Illusion effect as a function of Grip Scaling Slope by participant:", task),
         x = "Participant Grip Scaling Slope in a Neutral Context (mm)", y = "Participant Illusion effect (mm)")
  print(slope_graph)
}

graph_function_task("DESCRIPTION")
sd <- ggplotly(slope_graph) %>%
  layout(legend = list(x=1, y=.5))
sd[['x']][['layout']][['annotations']][[1]][['y']] <- .6
print(sd)

###graphing slopes and fins by Language group###
graph_function_group <- function(group){
  group_text <- enquo(group)
  slope_graph <- All_data %>%
    filter(LCAT == !!group_text) %>%
    ggplot(aes(x=slope, y=illusion))+
    geom_point(aes(color = task)) +
    geom_smooth(method = "lm", aes(color = task))+
    #geom_text(aes(label = obsisSubj), nudge_y = .3) +
    #facet_wrap(~task) +
    labs(title = paste("Relation of grip aperture slopes to illusion effect:", group), 
         x = "Slope", y = "Illusion effect")+
    scale_color_manual(labels = c("ACTION", "DESCRIPTION", "ESTIMATION"), values = c("#F8766D", "#00BA38", "#619CFF"))
  
  print(slope_graph)
}

slope_graph <- All_data %>%
  filter(LCAT == "Speakers", task == "DESCRIPTION") %>%
  ggplot(aes(x=slope, y=illusion))+
  geom_point(aes(color = task)) +
  geom_smooth(method = "lm", aes(color = task))+
  #geom_text(aes(label = obsisSubj), nudge_y = .3) +
  labs(title = paste("Illusion effect as a function of Grip Scaling Slope: Speakers"), 
       x = "Participant Grip Scaling Slope for Descriptions in a Neutral Context (mm)", y = "Participant Illusion effect")+
  scale_color_manual(values = "#00BFC4") +
  theme(legend.position = "none")

print(slope_graph)

sg <- ggplotly(slope_graph)%>%
  layout(showlegend = F)

#str(gp[['x']][['layout']][['annotations']]) 
#str(gp[['x']][['layout']][['legend']]) 
#gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.06
#gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
#gp[['x']][['layout']][['annotations']][[8]][['x']] <- 1.05
#gp[['x']][['layout']][['annotations']][[8]][['y']] <- .6

print(sg)

graph_function_group("Speakers")
graph_function_group("Signers")

Speakers_Compare_Gest <- Speakers_All %>%
  dplyr::filter(task == "DESCRIPTION")

Signers_Compare_Gest <- Signers_All %>%
  dplyr::filter(task == "DESCRIPTION")

####Modeling linear relation of slopes to fins####
mod1 <- lm(slope ~ illusion, data = Speakers_Compare_Gest)
summary(mod1)

mod2 <- lm(slope ~ illusion, data = Signers_Compare_Gest)
summary(mod2)

####Variance on gestures at each stick size####

variances <- Both_none %>%
  filter(type == "DESCRIPTION") %>%
  group_by(Lcat, obsisSubj, stick) %>%
  summarise(mean = mean(maxGrip), sd=sd(maxGrip), var = var(maxGrip))%>%
  mutate(stick = factor(stick, levels = c("five", "seven", "nine", "eleven"))) #%>%
  group_by(Lcat, stick) %>%
  summarise(meanvar = mean(var, na.rm = T), mean(sd, na.rm = T))

mod_var <- lmer(sd ~ Lcat + (1 | obsisSubj), data = variances)
summary(mod_var)
  

####Testing Variances of grip apertures by Language Group and at different stick sizes####
#run model one group function, then ranef as dataframe, then test variance of finsopen
onegroup_function(ENG_fins, "DESCRIPTION")
values <- ranef(model)
values <- as.data.frame(values$obsisSubj)
values$Lcat <- "Speakers"

onegroup_function(ASL_fins, "DESCRIPTION")
values_asl <- ranef(model)
values_asl <- as.data.frame(values_asl$obsisSubj)
values_asl$Lcat <- "Signers"

values_both <- rbind(values, values_asl)

mod_var <- lm(finsopen ~ Lcat, data = values_both)
summary(mod_var)

variances <- Both_fins %>%
  filter(type == "DESCRIPTION") %>%
  group_by(obsisSubj, stick, fins) %>%
  summarize(diff = mean(maxGrip))
  
  group_by(Lcat, obsisSubj, stick) %>%
  summarise(mean = mean(maxGrip), sd=sd(maxGrip), var = var(maxGrip))%>%
  mutate(stick = factor(stick, levels = c("five", "seven", "nine", "eleven"))) #%>%
group_by(Lcat, stick) %>%
  summarise(meanvar = mean(var, na.rm = T), mean(sd, na.rm = T))

mod_var <- lm(var ~ stick + Lcat, data = variances)
summary(mod_var)
