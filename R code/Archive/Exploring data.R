library(tidyverse)
library(checkpoint)
checkpoint("2018-09-28", R.version = "3.5.1",
           project = book_directory,
           checkpointLocation = checkpoint_directory,
           scanForPackages = FALSE,
           scan.rnw.with.knitr = TRUE, use.knitr = TRUE)
library(knitr)
library(ggplot2)
library(cowplot)
library(viridis)
library(JWileymisc)
library(data.table)
library(lme4)
library(lmerTest)
library(chron)
library(zoo)
library(pander)
library(texreg)
options(width = 70, digits = 2)

set.seed(1234)
ex.data.1 <- tibble(
  ID = factor(rep(1:4, each = 10)),
  time = rep(1:10, times = 4),
  y = rnorm(40, rep(1:4, each = 10), .2))
ex.data.2 <- tibble(
  ID = factor(rep(1:4, each = 10)),
  time = rep(1:10, times = 4),
  y = rnorm(40, 2.5, 1))
plot_grid(
  ggplot(ex.data.1,
         aes(time, y, colour = ID, shape = ID)) +
    stat_smooth(method = "lm", formula = y ~ 1, se=FALSE) +
  geom_point() +
    scale_color_viridis(discrete = TRUE),
  ggplot(ex.data.2,
         aes(time, y, colour = ID, shape = ID)) +
    stat_smooth(method = "lm", formula = y ~ 1, se=FALSE) +
    geom_point() +
    scale_color_viridis(discrete = TRUE),
  ncol = 1,
  labels = c(
    "High Between Variance",
    "Low Between Variance"),
  align = "hv")

indices <- c("063", "064", "070", "071")

grid <- ENG_fins %>%
  select(obsisSubj, stickcmCentered, stick, fins, maxGrip, type) %>%
  filter(obsisSubj %in% indices,
         type == "DESCRIPTION")

plot_grid(ggplot(data = grid, aes(x=stick, y=maxGrip, colour = obsisSubj, shape = fins)) +
              stat_smooth(method = "lm", formula = y ~ 1, se=FALSE) +
              geom_point() +
              scale_color_viridis(discrete = T),
          ncol = 1,
          align = "hv")

