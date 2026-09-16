library(tidyverse)
library(factoextra)
library(FactoMineR)
library(forcats)
library(lubridate)
library(ggh4x)

# Approximate disturbance date for each example point. `name` is duplicated
# across strata in Results/FINAL_verifpts_landt_vs_ccdc_2000-2025_all.gpkg
# (see compare_disturbance_dates.R), so a lookup by name there is not
# reliable for these two illustrative points - set directly instead.
detected_dates <- tibble(idpt = c(1, 68), date = c(2021.75, 2015.5))

get_detected_date <- function(id) {
  detected_dates |>
    dplyr::filter(idpt == id) |>
    dplyr::pull(date)
}

# The exact CCDC break date to plot: this point's own tBreak_* value (from
# its fitted coefficients) closest to the approximate date above, rather
# than the approximate date itself.
get_nearest_tbreak <- function(mods_row, target) {
  tbreaks <- mods_row |>
    dplyr::select(dplyr::matches("^tBreak_\\d+$")) |>
    unlist(use.names = FALSE) |>
    as.numeric()
  tbreaks <- tbreaks[!is.na(tbreaks)]
  tbreaks[which.min(abs(tbreaks - target))]
}

# id 1----
# Files read
idpt <- 1
df <- read.csv(paste0("Data/fitted_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  select(-.geo)
dfobs <- read.csv(paste0("Data/obs_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  mutate(across(system.time_start, ~mdy(.x)))

mods <- df |>
  # select(matches("LC_Class|COS|COS[0-9]+|SIN|SIN[0-9]+|INTP|SLP")) |>
  mutate(id = row_number())

ccdc_date <- get_nearest_tbreak(mods[1, ], get_detected_date(idpt))
# summarise(across(matches("COS|COS[0-9]+|SIN|SIN[0-9]+|INTP|SLP"),
#                  mean),
#           .by = c(LC_Class))
# # pivot_longer(cols = -LC_Class,
#              names_to = c("band", "var"),
#              names_pattern = c("([A-z]+)_([A-z]+[0-9]|[A-z]+)")) |>
# summarise(meanVal = mean(value),
#           .by = c(LC_Class,band, var))

anios <- 1
df2 <- tibble(year = seq(decimal_date(ymd("2000-01-01")), decimal_date(ymd("2026-01-01")), by = 0.2)
)
df2 <- df2 #* 2 * pi

rm(df)

# pred <- mods$meanVal[4] + mods$meanVal[1]*cos(df2) +  mods$meanVal[2]*cos(df2) + mods$meanVal[3]*cos(df2) + mods$meanVal[5]*sin(df2) +  mods$meanVal[6]*sin(df2) + mods$meanVal[7]*sin(df2)

# Coefficients order
# intp, slop, cos1, sin1, cos2, sin2, cos3, sin3

predictor <- function(x, i){
  tibble(id = mods[i, ]$id, 
         fittedNBR = case_when(mods[i, ]$tStart_0 <= x & x <= mods[i, ]$tEnd_0 ~ mods[i, ]$NBR_coefs_0_0 +
                                 x  * mods[i, ]$NBR_coefs_0_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_0_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_0_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_0_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_0_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_0_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_0_7,
                               mods[i, ]$tStart_1 <= x & x <= mods[i, ]$tEnd_1 ~ mods[i, ]$NBR_coefs_1_0 +
                                 x  * mods[i, ]$NBR_coefs_1_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_1_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_1_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_1_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_1_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_1_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_1_7,
                               mods[i, ]$tStart_2 <= x & x <= mods[i, ]$tEnd_2 ~ mods[i, ]$NBR_coefs_2_0 +
                                 x  * mods[i, ]$NBR_coefs_2_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_2_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_2_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_2_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_2_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_2_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_2_7,
                               mods[i, ]$tStart_3 <= x & x <= mods[i, ]$tEnd_3 ~ mods[i, ]$NBR_coefs_3_0 +
                                 x  * mods[i, ]$NBR_coefs_3_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_3_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_3_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_3_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_3_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_3_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_3_7,
                               mods[i, ]$tStart_4 <= x & x <= mods[i, ]$tEnd_4 ~ mods[i, ]$NBR_coefs_4_0 +
                                 x  * mods[i, ]$NBR_coefs_4_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_4_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_4_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_4_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_4_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_4_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_4_7,
                               mods[i, ]$tStart_5 <= x & x <= mods[i, ]$tEnd_5 ~ mods[i, ]$NBR_coefs_5_0 +
                                 x  * mods[i, ]$NBR_coefs_5_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_5_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_5_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_5_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_5_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_5_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_5_7,
                               mods[i, ]$tStart_6 <= x & x <= mods[i, ]$tEnd_6 ~ mods[i, ]$NBR_coefs_6_0 +
                                 x  * mods[i, ]$NBR_coefs_6_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_6_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_6_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_6_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_6_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_6_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_6_7,
                               mods[i, ]$tStart_7 <= x & x <= mods[i, ]$tEnd_7 ~ mods[i, ]$NBR_coefs_7_0 +
                                 x  * mods[i, ]$NBR_coefs_7_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_7_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_7_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_7_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_7_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_7_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_7_7),
         segment = case_when(mods[i, ]$tStart_0 <= x & x <= mods[i, ]$tEnd_0 ~ 0,
                             mods[i, ]$tStart_1 <= x & x <= mods[i, ]$tEnd_1 ~ 1,
                             mods[i, ]$tStart_2 <= x & x <= mods[i, ]$tEnd_2 ~ 2,
                             mods[i, ]$tStart_3 <= x & x <= mods[i, ]$tEnd_3 ~ 3,
                             mods[i, ]$tStart_4 <= x & x <= mods[i, ]$tEnd_4 ~ 4,
                             mods[i, ]$tStart_5 <= x & x <= mods[i, ]$tEnd_5 ~ 5,
                             mods[i, ]$tStart_6 <= x & x <= mods[i, ]$tEnd_6 ~ 6,
                             mods[i, ]$tStart_7 <= x & x <= mods[i, ]$tEnd_7 ~ 7),
         year = x )
}

# SWIR 1
fittedinfrared <- map(df2$year, function(year){
  map(1:nrow(mods), ~predictor(year, .x))
})

# Fitted curve per segment, colored by segment ----
segment_plotter <- bind_rows(fittedinfrared) |>
  filter(!is.na(segment)) |>
  mutate(fittedNBR = fittedNBR) |>
  mutate(across(segment, ~ .x+1))

obs_plotter <- dfobs |>
  mutate(year = decimal_date(system.time_start)) |>
  rename(fittedNBR = NBR)

seg_ids <- sort(unique(segment_plotter$segment))
seg_labels <- paste("Segment", seg_ids)
seg_colors <- RColorBrewer::brewer.pal(max(3, length(seg_ids)), "Set2")[seq_along(seg_ids)]
names(seg_colors) <- seg_labels

p1 <- segment_plotter |>
  mutate(seg_label = paste("Segment", segment)) |>
  ggplot(aes(x = year,
             y = fittedNBR,
             col = seg_label,
             group = segment)) +
  geom_vline(aes(xintercept = ccdc_date, col = "Detected disturbance"),
             linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_point(data = obs_plotter,
             aes(x = year, y = fittedNBR, col = "Observed NBR"),
             inherit.aes = FALSE,
             alpha = 0.6,
             size = 0.7) +
  scale_color_manual(
    name = NULL,
    breaks = c("Observed NBR", seg_labels, "Detected disturbance"),
    values = c(seg_colors, "Observed NBR" = "gray30", "Detected disturbance" = "black")
  ) +
  guides(col = guide_legend(override.aes = list(
    linetype = c("blank", rep("solid", length(seg_labels)), "dashed"),
    shape = c(16, rep(NA, length(seg_labels)), NA)
  ), nrow = 4, byrow = TRUE)) +
  # facet_wrap(~id,
  #            labeller = label_both,
  #            scales = "free_y",
  #            nrow = 3) +
  scale_x_continuous(breaks = seq(2000, 2025, 5),
                     minor_breaks = seq(2000,2025,1)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-0.25,0.75)) +
  labs(title = "CCDC",
  x = "Year",
       y = "NBR") +
  cowplot::theme_cowplot() +
  theme(panel.grid.minor.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        strip.background = element_rect(fill = "#EDEDED"),
        legend.position = "bottom")

p1

ggsave(paste0("Plots/CCDC_name",idpt,"_verifpts.jpg"),
       width = 29,
       height = 12,
       units = "cm")

# id 68----
# Files read
idpt <- 68
df <- read.csv(paste0("Data/fitted_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  select(-.geo)
dfobs <- read.csv(paste0("Data/obs_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  mutate(across(system.time_start, ~mdy(.x)))

mods <- df |>
  # select(matches("LC_Class|COS|COS[0-9]+|SIN|SIN[0-9]+|INTP|SLP")) |>
  mutate(id = row_number())

ccdc_date <- get_nearest_tbreak(mods[1, ], get_detected_date(idpt))
# summarise(across(matches("COS|COS[0-9]+|SIN|SIN[0-9]+|INTP|SLP"),
#                  mean),
#           .by = c(LC_Class))
# # pivot_longer(cols = -LC_Class,
#              names_to = c("band", "var"),
#              names_pattern = c("([A-z]+)_([A-z]+[0-9]|[A-z]+)")) |>
# summarise(meanVal = mean(value),
#           .by = c(LC_Class,band, var))

anios <- 1
df2 <- tibble(year = seq(decimal_date(ymd("2000-01-01")), decimal_date(ymd("2026-01-01")), by = 0.2)
)
df2 <- df2 #* 2 * pi

rm(df)

# pred <- mods$meanVal[4] + mods$meanVal[1]*cos(df2) +  mods$meanVal[2]*cos(df2) + mods$meanVal[3]*cos(df2) + mods$meanVal[5]*sin(df2) +  mods$meanVal[6]*sin(df2) + mods$meanVal[7]*sin(df2)

# Coefficients order
# intp, slop, cos1, sin1, cos2, sin2, cos3, sin3

predictor <- function(x, i){
  tibble(id = mods[i, ]$id, 
         fittedNBR = case_when(mods[i, ]$tStart_0 <= x & x <= mods[i, ]$tEnd_0 ~ mods[i, ]$NBR_coefs_0_0 +
                                 x  * mods[i, ]$NBR_coefs_0_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_0_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_0_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_0_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_0_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_0_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_0_7,
                               mods[i, ]$tStart_1 <= x & x <= mods[i, ]$tEnd_1 ~ mods[i, ]$NBR_coefs_1_0 +
                                 x  * mods[i, ]$NBR_coefs_1_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_1_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_1_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_1_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_1_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_1_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_1_7,
                               mods[i, ]$tStart_2 <= x & x <= mods[i, ]$tEnd_2 ~ mods[i, ]$NBR_coefs_2_0 +
                                 x  * mods[i, ]$NBR_coefs_2_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_2_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_2_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_2_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_2_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_2_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_2_7,
                               mods[i, ]$tStart_3 <= x & x <= mods[i, ]$tEnd_3 ~ mods[i, ]$NBR_coefs_3_0 +
                                 x  * mods[i, ]$NBR_coefs_3_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_3_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_3_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_3_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_3_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_3_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_3_7,
                               mods[i, ]$tStart_4 <= x & x <= mods[i, ]$tEnd_4 ~ mods[i, ]$NBR_coefs_4_0 +
                                 x  * mods[i, ]$NBR_coefs_4_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_4_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_4_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_4_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_4_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_4_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_4_7,
                               mods[i, ]$tStart_5 <= x & x <= mods[i, ]$tEnd_5 ~ mods[i, ]$NBR_coefs_5_0 +
                                 x  * mods[i, ]$NBR_coefs_5_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_5_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_5_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_5_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_5_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_5_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_5_7,
                               mods[i, ]$tStart_6 <= x & x <= mods[i, ]$tEnd_6 ~ mods[i, ]$NBR_coefs_6_0 +
                                 x  * mods[i, ]$NBR_coefs_6_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_6_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_6_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_6_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_6_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_6_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_6_7,
                               mods[i, ]$tStart_7 <= x & x <= mods[i, ]$tEnd_7 ~ mods[i, ]$NBR_coefs_7_0 +
                                 x  * mods[i, ]$NBR_coefs_7_1 +
                                 cos((x * 2 * pi)) * mods[i, ]$NBR_coefs_7_2 + 
                                 cos((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_7_3 + 
                                 cos((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_7_4 + 
                                 sin((x * 2 * pi)) * mods[i, ]$NBR_coefs_7_5 + 
                                 sin((x * 2 * pi * 2)) * mods[i, ]$NBR_coefs_7_6 + 
                                 sin((x * 2 * pi * 3)) * mods[i, ]$NBR_coefs_7_7),
         segment = case_when(mods[i, ]$tStart_0 <= x & x <= mods[i, ]$tEnd_0 ~ 0,
                             mods[i, ]$tStart_1 <= x & x <= mods[i, ]$tEnd_1 ~ 1,
                             mods[i, ]$tStart_2 <= x & x <= mods[i, ]$tEnd_2 ~ 2,
                             mods[i, ]$tStart_3 <= x & x <= mods[i, ]$tEnd_3 ~ 3,
                             mods[i, ]$tStart_4 <= x & x <= mods[i, ]$tEnd_4 ~ 4,
                             mods[i, ]$tStart_5 <= x & x <= mods[i, ]$tEnd_5 ~ 5,
                             mods[i, ]$tStart_6 <= x & x <= mods[i, ]$tEnd_6 ~ 6,
                             mods[i, ]$tStart_7 <= x & x <= mods[i, ]$tEnd_7 ~ 7),
         year = x )
}

# SWIR 1
fittedinfrared <- map(df2$year, function(year){
  map(1:nrow(mods), ~predictor(year, .x))
})

# Fitted curve per segment, colored by segment ----
segment_plotter <- bind_rows(fittedinfrared) |>
  filter(!is.na(segment)) |>
  mutate(fittedNBR = fittedNBR) |>
  mutate(across(segment, ~ .x+1))

obs_plotter <- dfobs |>
  mutate(year = decimal_date(system.time_start)) |>
  rename(fittedNBR = NBR)

seg_ids <- sort(unique(segment_plotter$segment))
seg_labels <- paste("Segment", seg_ids)
seg_colors <- RColorBrewer::brewer.pal(max(3, length(seg_ids)), "Set2")[seq_along(seg_ids)]
names(seg_colors) <- seg_labels

p2 <- segment_plotter |>
  mutate(seg_label = paste("Segment", segment)) |>
  ggplot(aes(x = year,
             y = fittedNBR,
             col = seg_label,
             group = segment)) +
  geom_vline(aes(xintercept = ccdc_date, col = "Detected disturbance"),
             linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_point(data = obs_plotter,
             aes(x = year, y = fittedNBR, col = "Observed NBR"),
             inherit.aes = FALSE,
             alpha = 0.6,
             size = 0.7) +
  scale_color_manual(
    name = NULL,
    breaks = c("Observed NBR", seg_labels, "Detected disturbance"),
    values = c(seg_colors, "Observed NBR" = "gray30", "Detected disturbance" = "black")
  ) +
  guides(col = guide_legend(override.aes = list(
    linetype = c("blank", rep("solid", length(seg_labels)), "dashed"),
    shape = c(16, rep(NA, length(seg_labels)), NA)
  ), nrow = 4, byrow = TRUE)) +
  # facet_wrap(~id,
  #            labeller = label_both,
  #            scales = "free_y",
  #            nrow = 3) +
  scale_x_continuous(breaks = seq(2000, 2025, 5),
                     minor_breaks = seq(2000,2025,1)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-0.25,0.75)) +
  labs(title = "CCDC",
       x = "Year",
       y = "NBR") +
  cowplot::theme_cowplot() +
  theme(panel.grid.minor.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        strip.background = element_rect(fill = "#EDEDED"),
        legend.position = "bottom")

p2

ggsave(paste0("Plots/CCDC_name",idpt,"_verifpts.jpg"),
       width = 29,
       height = 12,
       units = "cm")