library(tidyverse)
library(lubridate)

# Detected disturbance date for each example point. `name` is duplicated
# across strata in Results/FINAL_verifpts_landt_vs_ccdc_2000-2025_all.gpkg
# (see compare_disturbance_dates.R), so a lookup by name there is not
# reliable for these two illustrative points - set directly instead.
detected_dates <- tibble(idpt = c(1, 68), date = c(2021.75, 2015.75))

get_detected_date <- function(id) {
  detected_dates |>
    filter(idpt == id) |>
    pull(date)
}

# Read data----
# id 1
# Possible FIX: put years at the center of the year instead of start. for example, all years .5
idpt <- 1
landt_date <- get_detected_date(idpt)
verifpts <- read_csv(paste0("Data/ltPoint_id",idpt,"_export.csv")) |>
  pivot_longer(cols = c(starts_with("fitted"), 
                        starts_with("raw"),
                        starts_with("vertex"),
                        starts_with("year")),
               names_sep = "_",
               names_to = c("var", "index")) |> 
  pivot_wider(names_from = "var") |>
  rename("Year" = "year",
         "Fitted" = "fitted",
         "Original" = "raw") |>
  mutate(across(Year, ~ as.integer(str_remove_all(.x, ",")))) |>
  # Need to sum 1 year to indicate the year where the loss is detected instead of the year of start
  # of the segment that ends in loss; also add 0.75 to make it close to the observed values
  mutate(across(Year, ~ .x + 0.75)) |>
  mutate(across(Year, ~ ifelse(.x == 1999.75, 2000.25, .x))) |>
  mutate(Breaks = ifelse(vertex == 1, Fitted, NA)) |>
  mutate(across(c(Original, Fitted, Breaks), ~ .x/1000 *-1)) |>
  # `index` was pivoted from column suffixes as a character (0,1,10,11,...),
  # so rows are NOT in chronological order yet - sort by Year before anything
  # downstream relies on ordering (vertex_years must be sorted for findInterval).
  arrange(Year)

# Duplicate interior vertices so every segment keeps both of its endpoints -
# otherwise a segment spanning a single year (e.g. 2016-2017) has only one
# point assigned to it and geom_line has nothing to connect it to.
vertex_years <- verifpts |> 
  filter(vertex == 1) |>
  pull(Year)
interior_vertices <- vertex_years[-c(1, length(vertex_years))]

verifpts <- verifpts|>
  # Segment number (starting at 0) for the period between each pair of vertices
  mutate(segment = pmin(findInterval(Year, vertex_years), length(vertex_years) - 1) - 1)|>
  mutate(across(segment, ~ .x+1))

verifpts_seg <- bind_rows(
  verifpts,
  verifpts |> 
    filter(Year %in% interior_vertices) |> 
    mutate(segment = segment - 1)
) |>
  arrange(segment, Year)

dfobs <- read.csv(paste0("Data/obs_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  mutate(across(system.time_start, ~mdy(.x))) |>
  mutate(year = decimal_date(system.time_start)) |>
  rename(fittedNBR = NBR)

# Plot----
seg_ids <- sort(unique(verifpts_seg$segment))
seg_labels <- paste("Segment", seg_ids)
seg_colors <- RColorBrewer::brewer.pal(max(3, length(seg_ids)), "Set2")[seq_along(seg_ids)]
names(seg_colors) <- seg_labels

p3 <- ggplot(verifpts, aes(x = Year)) +
  geom_vline(aes(xintercept = landt_date, color = "Detected disturbance"),
             linetype = "dashed", linewidth = 1) +
  geom_point(data = dfobs,
             aes(x = year, y = fittedNBR, color = "Observed NBR"),
             inherit.aes = FALSE,
             alpha = 0.6,
             size = 0.7) +
  geom_line(data = verifpts_seg,
            aes(x = Year, y = Fitted, color = paste("Segment", segment), group = segment),
            inherit.aes = FALSE,
            linewidth = 1, alpha = 0.6) +
  geom_point(aes(y = Breaks, color = "Vertex"), size = 3.5) +
  geom_point(aes(y = Original, color = "Annual median"), size  = 1.5) +
  labs(
    title = "LandTrendr",
    x = "Year",
    y = "NBR",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(2000,2025,5),
                     minor_breaks = seq(2000,2025,1)) +
  scale_y_continuous(breaks = seq(-0.25, 0.75, 0.25)) +
  scale_color_manual(
    breaks = c("Observed NBR", "Annual median", seg_labels, "Vertex", "Detected disturbance"),
    values = c("Observed NBR" = "gray30",
               "Annual median" = "royalblue",
               seg_colors,
               "Vertex" = "firebrick",
               "Detected disturbance" = "black")
  ) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, 16, rep(NA, length(seg_labels)), 16, NA),
    linetype = c("blank", "blank", rep("solid", length(seg_labels)), "blank", "dashed"),
    size = c(1.2, 3, rep(NA, length(seg_labels)), 5, NA),
    linewidth = c(NA, NA, rep(1, length(seg_labels)), NA, 1)
  ), nrow = 4, byrow = TRUE)) +
  cowplot::theme_cowplot() +
  theme(panel.grid.minor.x = element_line(colour = "gray80",
                                        linewidth = 0.5,
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "gray80",
                                        linewidth = 0.5,
                                        linetype = 2),
        strip.background = element_rect(fill = "#EDEDED"),
        legend.position = "bottom")

p3

ggsave(paste0("Plots/LandTrendr_name",idpt,"_verifpts.jpeg"), 
              p3, width = 8, height = 5)


# Read id 68----

# Possible FIX: put years at the center of the year instead of start. for example, all years .5
idpt <- 68
landt_date <- get_detected_date(idpt)
verifpts <- read_csv(paste0("Data/ltPoint_id",idpt,"_export.csv")) |>
  pivot_longer(cols = c(starts_with("fitted"), 
                        starts_with("raw"),
                        starts_with("vertex"),
                        starts_with("year")),
               names_sep = "_",
               names_to = c("var", "index")) |> 
  pivot_wider(names_from = "var") |>
  rename("Year" = "year",
         "Fitted" = "fitted",
         "Original" = "raw") |>
  mutate(across(Year, ~ as.integer(str_remove_all(.x, ",")))) |>
  # Need to sum 1 year to indicate the year where the loss is detected instead of the year of start
  # of the segment that ends in loss; also add 0.75 to make it close to the observed values
  mutate(across(Year, ~ .x + 0.75)) |>
  mutate(across(Year, ~ ifelse(.x == 1999.75, 2000.25, .x))) |>
  mutate(Breaks = ifelse(vertex == 1, Fitted, NA)) |>
  mutate(across(c(Original, Fitted, Breaks), ~ .x/1000 *-1)) |>
  # `index` was pivoted from column suffixes as a character (0,1,10,11,...),
  # so rows are NOT in chronological order yet - sort by Year before anything
  # downstream relies on ordering (vertex_years must be sorted for findInterval).
  arrange(Year)

# Duplicate interior vertices so every segment keeps both of its endpoints -
# otherwise a segment spanning a single year (e.g. 2016-2017) has only one
# point assigned to it and geom_line has nothing to connect it to.
vertex_years <- verifpts |> 
  filter(vertex == 1) |>
  pull(Year)
interior_vertices <- vertex_years[-c(1, length(vertex_years))]

verifpts <- verifpts|>
  # Segment number (starting at 0) for the period between each pair of vertices
  mutate(segment = pmin(findInterval(Year, vertex_years), length(vertex_years) - 1) - 1)|>
  mutate(across(segment, ~ .x+1))

verifpts_seg <- bind_rows(
  verifpts,
  verifpts |> 
    filter(Year %in% interior_vertices) |> 
    mutate(segment = segment - 1)
) |>
  arrange(segment, Year)

dfobs <- read.csv(paste0("Data/obs_id",idpt,"_NBR_2000_2025_CCDCmetrics.csv")) |>
  mutate(across(system.time_start, ~mdy(.x))) |>
  mutate(year = decimal_date(system.time_start)) |>
  rename(fittedNBR = NBR)

# Plot----
seg_ids <- sort(unique(verifpts_seg$segment))
seg_labels <- paste("Segment", seg_ids)
seg_colors <- RColorBrewer::brewer.pal(max(3, length(seg_ids)), "Set2")[seq_along(seg_ids)]
names(seg_colors) <- seg_labels

p4 <- ggplot(verifpts, aes(x = Year)) +
  geom_vline(aes(xintercept = landt_date, color = "Detected disturbance"),
             linetype = "dashed", linewidth = 1) +
  geom_point(data = dfobs,
             aes(x = year, y = fittedNBR, color = "Observed NBR"),
             inherit.aes = FALSE,
             alpha = 0.6,
             size = 0.7) +
  geom_line(data = verifpts_seg,
            aes(x = Year, y = Fitted, color = paste("Segment", segment), group = segment),
            inherit.aes = FALSE,
            linewidth = 1, alpha = 0.6) +
  geom_point(aes(y = Breaks, color = "Vertex"), size = 3.5) +
  geom_point(aes(y = Original, color = "Annual median"), size  = 1.5) +
  labs(
    title = "LandTrendr",
    x = "Year",
    y = "NBR",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(2000,2025,5),
                     minor_breaks = seq(2000,2025,1)) +
  scale_y_continuous(breaks = seq(-0.25, 0.75, 0.25)) +
  scale_color_manual(
    breaks = c("Observed NBR", "Annual median", seg_labels, "Vertex", "Detected disturbance"),
    values = c("Observed NBR" = "gray30",
               "Annual median" = "royalblue",
               seg_colors,
               "Vertex" = "firebrick",
               "Detected disturbance" = "black")
  ) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, 16, rep(NA, length(seg_labels)), 16, NA),
    linetype = c("blank", "blank", rep("solid", length(seg_labels)), "blank", "dashed"),
    size = c(1.2, 3, rep(NA, length(seg_labels)), 5, NA),
    linewidth = c(NA, NA, rep(1, length(seg_labels)), NA, 1)
  ), nrow = 4, byrow = TRUE)) +
  cowplot::theme_cowplot() +
  theme(panel.grid.minor.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_line(colour = "gray80",
                                          linewidth = 0.5,
                                          linetype = 2),
        strip.background = element_rect(fill = "#EDEDED"),
        legend.position = "bottom")

p4

ggsave(paste0("Plots/LandTrendr_name",idpt,"_verifpts.jpeg"),
       p4, width = 8, height = 5)