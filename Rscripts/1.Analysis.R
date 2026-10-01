library(terra)
library(tidyverse)
library(sf)
library(yardstick)
library(mapaccuracy)

# Preprocessing------
landt_months <- "6_12"
# A partir de esto, thresholds:
# 6-12
# Landtrendr: 350
# CCDC: 350

# 1-12
# Landtrendr: 380
# CCDC: 380
thresh <- ifelse(landt_months == "6_12", 350, 380)

landt <- terra::rast(
  paste0("D:/GeoInfo/CCDCvsLandtrendr/LandtrendrGreatChange_Avocado00-26_nocloudFilter_ForestAvocadoMask_", landt_months, ".tif"
))
ccdc <- rast("D:/GeoInfo/CCDCvsLandtrendr/CCDCChanges_00_26_forestAvocadoMask.tif")
forest <- rast("D:/GeoInfo/CCDCvsLandtrendr/BosqueMask_geom3.tif")
forest[forest == 0] <- NA

plot(forest)

# Read final validation points
verifpts <- st_read(
  "gpkg/FINAL_verifpts_landt_vs_ccdc_2000-2025_all_driver.gpkg"
)

bounder <- c(
  max(as.vector(ext(landt))[1], as.vector(ext(ccdc))[1]),
  min(as.vector(ext(landt))[2], as.vector(ext(ccdc))[2]),
  max(as.vector(ext(landt))[3], as.vector(ext(ccdc))[3]),
  min(as.vector(ext(landt))[4], as.vector(ext(ccdc))[4])
)

landt <- crop(landt, ext(bounder))
ccdc <- crop(ccdc, ext(bounder))
forest <- crop(forest, ext(bounder))

freq(forest) |>
  mutate(areaha = count * res(forest)[1] * res(forest)[2] / 10000)

# Get magnitude and match images
landt_year <- landt["yod"]
# Add one year to match the year where the loss was first observed
landt_year <- landt_year +1 
landt_rate <- landt["rate"]
landt_dur <- landt["dur"]
landt <- landt["mag"]

# Invert NBR so it matches landt
ccdc_year <- ccdc["tBreak"]
ccdc_tbreak <- ccdc["numTbreak"]
ccdc <- ccdc["MAG"] * -1
ccdc[ccdc <= 100] <- 0

# Retain only forest pixels
landt[is.na(landt)] <- 0
landt <- mask(landt, forest)

# ccdc[ccdc == 0] <- NA
ccdc <- mask(ccdc, forest)

# Check
plot(landt)
plot(ccdc)

# Define threshold for false and true change detection----
# View if some points intersect with rasters
landt_ex <- terra::extract(landt, vect(verifpts))
ccdc_ex <- terra::extract(ccdc, vect(verifpts))

# Check validation points to get a good threshold for magnitude
set.seed(5)

# For final validation
mag_df <- verifpts |>
  mutate(landt_mag = landt_ex$mag, ccdc_mag = ccdc_ex$MAG) |>
  filter(
    !is.na(landt_mag) | !is.na(ccdc_mag) | !is.na(landt_mag) & !is.na(ccdc_mag)
  )  |>
  mutate(class = paste(Proceso2, Model2, Type2, sep = "_"))
  # mutate(sumVerif = ifelse(sumverif1 %in% c(41, 43, 51, 53, 47, 57), 1, 0)) |>
  # group_by(sumVerif) |>
  # slice_sample(n = 200) |>
  # ungroup()

# For finding threshold
# mag_df <- verifpts |>
#   mutate(landt_mag = landt_ex$mag, ccdc_mag = ccdc_ex$MAG) |>
#   filter(
#     !is.na(landt_mag) | !is.na(ccdc_mag) | !is.na(landt_mag) & !is.na(ccdc_mag)
#   ) |>
#   mutate(sumVerif = ifelse(sumverif1 %in% c(41, 43, 51, 53, 47, 57), 1, 0)) |>
#   group_by(sumVerif) |>
#   slice_sample(n = 200) |>
#   ungroup()

# # Find the magnitude threshold to discriminate between true changes and false----
# # resul_mag <- mag_df |>
# #   group_by(sumverif1) |>
# #   summarise(landt_mean = mean(landt_mag, na.rm = TRUE),
# #             landt_n = n(),
# #             landt_sd = sd(landt_mag, na.rm = TRUE)/landt_n,
# #             ccdc_mean = mean(ccdc_mag, na.rm = TRUE),
# #             ccdc_n = n(),
# #             ccdc_sd = sd(ccdc_mag, na.rm = TRUE)/ccdc_n)
# #
# # # Based on these points, a magnitude value around 230-240 seems a good choice
# # resul_mag2 <- mag_df |>
# #   group_by(sumVerif) |>
# #   summarise(landt_mean = mean(landt_mag, na.rm = TRUE),
# #             landt_n = n(),
# #             landt_sd = sd(landt_mag, na.rm = TRUE)/landt_n,
# #             ccdc_mean = mean(ccdc_mag, na.rm = TRUE),
# #             ccdc_n = n(),
# #             ccdc_sd = sd(ccdc_mag, na.rm = TRUE)/ccdc_n)
# 
# # Test different thresholds
# explore_df <- map(seq(200,500,10), function(thresh){
#   sub_landt <- landt
#   sub_ccdc <- ccdc
# 
#   sub_landt[sub_landt <= thresh] <- 0
#   sub_ccdc[sub_ccdc <= thresh] <- 0
#   sub_landt[sub_landt > thresh] <- 1
#   sub_ccdc[sub_ccdc > thresh] <- 1
# 
#   landt_ex <- terra::extract(sub_landt, vect(mag_df))
#   ccdc_ex <- terra::extract(sub_ccdc, vect(mag_df))
#   
#   # landt_ex$mag <- ifelse(!is.na(landt_ex$mag), 1, 0)
#   # ccdc_ex$MAG <- ifelse(!is.na(ccdc_ex$MAG), 1, 0)
# 
#   resul_landt <- mag_df |>
#     st_drop_geometry() |>
#     select(sumVerif) |>
#     mutate(pred = landt_ex$mag) |>
#     filter(!is.na(pred)) |>
#     mutate(across(everything(), ~factor(.x, levels = c("0","1"))))
#   resul_ccdc <- mag_df |>
#     st_drop_geometry() |>
#     select(sumVerif) |>
#     mutate(pred = ccdc_ex$MAG) |>
#     filter(!is.na(pred)) |>
#     mutate(across(everything(), ~factor(.x, levels = c("0","1"))))
# 
#   return(
#     tibble(value = thresh,
#            landt_OA = accuracy(resul_landt,
#                                truth = sumVerif,
#                                estimate = pred) |>
#              pull(.estimate),
#            ccdc_OA = accuracy(resul_ccdc,
#                               truth = sumVerif,
#                               estimate = pred)|>
#       pull(.estimate))
#   )
# 
# }) |>
#   bind_rows()
# 
# maxOA <- explore_df |>
#   pivot_longer(cols = -value,
#                names_to = "model",
#                values_to = "OA") |>
#   group_by(model) |>
#   summarise(max(OA))
# 
# explore_df |>
#   filter(landt_OA == maxOA |>
#            select(`max(OA)`) |>
#            slice(2) |>
#            pull(`max(OA)`))
# 
# explore_df |>
#   filter(ccdc_OA == maxOA |>
#            select(`max(OA)`) |>
#            slice(1) |>
#            pull(`max(OA)`))

# Without masking non-forest areas
# A partir de esto, thresholds:
# Landtrendr: 230
# CCDC: 210

# Masking forest areas

# Lantrendr 1-12 only forest
# # A tibble: 8 × 3
# value landt_OA ccdc_OA
# <dbl>    <dbl>   <dbl>
#   1   350    0.749   0.738
# 2   380    0.756   0.738
# 3   390    0.756   0.738
# 4   400    0.756   0.738
# 5   410    0.749   0.738
# 6   420    0.749   0.738
# 7   430    0.746   0.738
# 8   480    0.735   0.738

# Landtrendr 6-12 only forest
# # A tibble: 8 × 3
# value landt_OA ccdc_OA
# <dbl>    <dbl>   <dbl>
#   1   350    0.774   0.738
# 2   380    0.753   0.738
# 3   390    0.753   0.738
# 4   400    0.753   0.738
# 5   410    0.756   0.738
# 6   420    0.753   0.738
# 7   430    0.753   0.738
# 8   480    0.746   0.738

# Threshold classification and olofsson calculations----
landt[landt <= thresh] <- 0
ccdc[ccdc <= thresh] <- 0

# Get OA
landt_ex <- terra::extract(landt, vect(mag_df))
ccdc_ex <- terra::extract(ccdc, vect(mag_df))

landt_ex$mag <- ifelse(landt_ex$mag != 0, 1, 0)
ccdc_ex$MAG <- ifelse(ccdc_ex$MAG != 0, 1, 0)

resul_landt <- mag_df |>
  st_drop_geometry() |>
  mutate(pred = landt_ex$mag) |>
  # filter((Model2 == "LandTrendr" & Type2 == "Exclusive") | Type2 == "Common") |>
  filter(!is.na(pred)) |>
  select(sumVerif, pred) |>
  mutate(across(c(pred, sumVerif), ~ factor(.x, levels = c("0", "1"))))
resul_ccdc <- mag_df |>
  st_drop_geometry() |>
  mutate(pred = ccdc_ex$MAG) |>
  # filter((Model2 == "CCDC" & Type2 == "Exclusive") | Type2 == "Common") |>
  filter(!is.na(pred)) |>
  select(sumVerif, pred) |>
  mutate(across(c(pred, sumVerif), ~ factor(.x, levels = c("0", "1"))))

tibble(
  landt_OA = accuracy(resul_landt, truth = sumVerif, estimate = pred) |>
    pull(.estimate),
  ccdc_OA = accuracy(resul_ccdc, truth = sumVerif, estimate = pred) |>
    pull(.estimate)
)

# Olofsson
areaConteo <- landt
areaConteo[areaConteo!= 0] <- 1
areaConteo[areaConteo== 0] <- 0
areas <- freq(areaConteo) |>
  as_tibble() |>
  mutate(areaha = count * 30 * 30 / 10000) |>
  pull(areaha)

names(areas) <- c(0, 1)
sum(areas)

resul_landt_olof <- olofsson(resul_landt$sumVerif, resul_landt$pred, areas)


resul_landt_olof$OA
resul_landt_olof$SEoa
resul_landt_olof$PA
resul_landt_olof$SEpa
resul_landt_olof$UA
resul_landt_olof$SEua
df_exp_landt <- tibble(
  sumVerif = names(resul_landt_olof$area),
  area = resul_landt_olof$area * sum(areas),
  se = resul_landt_olof$SEa * sum(areas)
) |>
  mutate(
    IC95 = se * qnorm(0.975),
    lower = area - qnorm(0.975) * se,
    upper = area + qnorm(0.975) * se
  ) |>
  arrange(sumVerif)

df_exp_landt

areaConteo <- ccdc
areaConteo[areaConteo!= 0] <- 1
areaConteo[areaConteo== 0] <- 0
areas <- freq(areaConteo) |>
  as_tibble() |>
  mutate(areaha = count * 30 * 30 / 10000) |>
  pull(areaha)

names(areas) <- c(0, 1)
resul_ccdc_olof <- olofsson(resul_ccdc$sumVerif, resul_ccdc$pred, areas)

resul_ccdc_olof$OA
resul_ccdc_olof$SEoa
resul_ccdc_olof$PA
resul_ccdc_olof$SEpa
resul_ccdc_olof$UA
resul_ccdc_olof$SEua
df_exp_ccdc <- tibble(
  sumVerif = names(resul_ccdc_olof$area),
  area = resul_ccdc_olof$area * sum(areas),
  se = resul_ccdc_olof$SEa * sum(areas)
) |>
  mutate(
    IC95 = se * qnorm(0.975),
    lower = area - qnorm(0.975) * se,
    upper = area + qnorm(0.975) * se
  ) |>
  arrange(sumVerif)

df_exp_ccdc

writeRaster(
  round(landt),
  paste0("Resul/Landtrendr_customThresh_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

writeRaster(
  round(ccdc),
  "Resul/CCDC_customThresh_nocloudfilt.tif",
  datatype = "INT2S",
  overwrite = TRUE
)

plot(landt)
plot(ccdc)

# Check differences in common/exclusive----

# Get OA
landt_ex <- terra::extract(landt, vect(mag_df))
ccdc_ex <- terra::extract(ccdc, vect(mag_df))

landt_ex$mag <- ifelse(landt_ex$mag != 0, 1, 0)
ccdc_ex$MAG <- ifelse(ccdc_ex$MAG != 0, 1, 0)

# One row per validation point with the reference (sumVerif) and BOTH methods'
# predictions. landt_ex / ccdc_ex come back in the same order as vect(mag_df),
# so they align positionally with mag_df - no join needed.
df_mc <- mag_df |>
  st_drop_geometry() |>
  mutate(
    sumVerif   = as.numeric(sumVerif),
    landt_pred = landt_ex$mag,
    ccdc_pred  = ccdc_ex$MAG
  ) |>
  filter(!is.na(landt_pred), !is.na(ccdc_pred)) |>
  mutate(
    landt_hit = as.integer(sumVerif == landt_pred),   # hit = sumVerif == pred
    ccdc_hit  = as.integer(sumVerif == ccdc_pred),
    agreement = factor(
      case_when(
        landt_hit == 1 & ccdc_hit == 1 ~ "both_correct",     # both methods agree & hit
        landt_hit == 1 & ccdc_hit == 0 ~ "only_LandTrendr",  # only LandTrendr correct
        landt_hit == 0 & ccdc_hit == 1 ~ "only_CCDC",        # only CCDC correct
        landt_hit == 0 & ccdc_hit == 0 ~ "both_wrong"        # both methods miss
      ),
      levels = c("both_correct", "only_LandTrendr", "only_CCDC", "both_wrong")
    )
  )

# ---- Overall agreement in correctness between the two methods 
agree_overall <- df_mc |>
  summarise(
    n               = n(),
    both_correct    = sum(agreement == "both_correct"),     # hits both methods agree on
    both_wrong      = sum(agreement == "both_wrong"),       # misses both methods make
    only_LandTrendr = sum(agreement == "only_LandTrendr"),
    only_CCDC       = sum(agreement == "only_CCDC"),
    # one_correct     = only_LandTrendr + only_CCDC           # exactly one method correct
  )
agree_overall

mcnemar.test(matrix(c(agree_overall$both_correct, 
                      agree_overall$only_CCDC, 
                      agree_overall$only_LandTrendr, 
                      agree_overall$both_wrong), nrow = 2))

# ---- Same, restricted to reference disturbances (sumVerif == 1) 
# How many real disturbances are detected (pred == 1) by both / one / neither method
agree_disturbance <- df_mc |>
  filter(sumVerif == 1) |>
  summarise(
    n_disturbance            = n(),
    detected_by_both         = sum(landt_pred == 1 & ccdc_pred == 1),
    detected_only_LandTrendr = sum(landt_pred == 1 & ccdc_pred == 0),
    detected_only_CCDC       = sum(landt_pred == 0 & ccdc_pred == 1),
    missed_by_both           = sum(landt_pred == 0 & ccdc_pred == 0)
  )
agree_disturbance

# ---- Broken down by common / exclusive stratum 
agree_by_type <- df_mc |>
  count(Type2, agreement, .drop = FALSE) |>
  pivot_wider(names_from = agreement, values_from = n, values_fill = 0)
agree_by_type

agree_by_stratum <- df_mc |>
  count(Model2, Type2, agreement, .drop = FALSE) |>
  pivot_wider(names_from = agreement, values_from = n, values_fill = 0)
agree_by_stratum

# ---- McNemar's test on paired correctness 
#                    CCDC correct   CCDC wrong
# LandTrendr correct     n11            n12
# LandTrendr wrong       n21            n22
n11 <- agree_overall$both_correct
n12 <- agree_overall$only_LandTrendr
n21 <- agree_overall$only_CCDC
n22 <- agree_overall$both_wrong

mcnemar_tab <- matrix(
  c(n11, n21, n12, n22), nrow = 2,
  dimnames = list(LandTrendr = c("correct", "wrong"),
                  CCDC       = c("correct", "wrong"))
)
mcnemar_tab
mcnemar.test(mcnemar_tab)

# ---- Exclusivo / common 
df_mc |>
  count(Model2, Type2, Proceso2, agreement, .drop = FALSE) |>
  pivot_wider(names_from = agreement, values_from = n, values_fill = 0) |>
  filter(Proceso2 == "Cambio bosque")

# # Check histograms----
reclass <- function(im, model) {
  rcl <- if (model == "landt") {
    matrix(
      c(0,thresh, NA, 
        thresh, thresh+100, 1, 
        thresh+100, thresh+200, 2, 
        thresh+200, thresh+300, 3, 
        thresh+300, thresh+400, 4, 
        thresh+400, 5000, 5),
      ncol = 3,
      byrow = TRUE
    )
  } else {
    matrix(
      c(0,thresh, NA, 
        thresh, thresh+100, 1, 
        thresh+100, thresh+200, 2, 
        thresh+200, thresh+300, 3, 
        thresh+300, thresh+400, 4, 
        thresh+400, 5000, 5),
      ncol = 3,
      byrow = TRUE
    )
  }
  return(classify(im, rcl, include.lowest = FALSE))
}
landt_rec <- reclass(landt, "landt")
ccdc_rec <- reclass(ccdc, "ccdc")

list(
  freq(landt_rec) |>
    as_tibble() |>
    mutate(
      areaha = count * 30 * 30 / 10000,
      perc = areaha / sum(areaha),
      model = "LandTrendr",
      sumVerif = case_when(
        value == 1 ~ paste(thresh, thresh+100, sep = "-"),
        value == 2 ~ paste(thresh+100, thresh+200, sep = "-"),
        value == 3 ~ paste(thresh+200, thresh+300, sep = "-"),
        value == 4 ~ paste(thresh+300, thresh+400, sep = "-"),
        value == 5 ~ paste(thresh+400, 5000, sep = "-"),
      )
    ),
  freq(ccdc_rec) |>
    as_tibble() |>
    mutate(
      areaha = count * 30 * 30 / 10000,
      perc = areaha / sum(areaha),
      model = "CCDC",
      sumVerif = case_when(
        value == 1 ~ paste(thresh, thresh+100, sep = "-"),
        value == 2 ~ paste(thresh+100, thresh+200, sep = "-"),
        value == 3 ~ paste(thresh+200, thresh+300, sep = "-"),
        value == 4 ~ paste(thresh+300, thresh+400, sep = "-"),
        value == 5 ~ paste(thresh+400, 5000, sep = "-"),
      )
    )
) |>
  bind_rows() |>
  filter(!is.na(sumVerif)) |>
  ggplot(aes(x = sumVerif, y = areaha)) +
  geom_col() +
  facet_wrap(~model) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,22000)) +
  labs(x = "Change Magnitude Class", y = "Area (ha)") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

if (!dir.exists("Plots")) {
  dir.create("Plots")
}

ggsave(
  paste0("Plots/threshold_changes_nocloudfilt_",landt_months,".jpeg"),
  width = 20,
  height = 16,
  units = "cm",
  dpi = 300
)

#Correlation----
# Low pearson correlation 0.18
layerCor(
  c(landt, ccdc),
  fun = "cor",
  asSampl = TRUE,
  use = "pairwise.complete.obs"
)

# IoU ----
landt_iou <- landt
ccdc_iou <- ccdc

landt_iou[landt_iou != 0] <- 1
ccdc_iou[ccdc_iou != 0] <- 1
landt_iou[landt_iou == 0] <- NA
ccdc_iou[ccdc_iou == 0] <- NA

# Intersection
inter <- mask(landt_iou, ccdc_iou)
plot(inter)

writeRaster(
  inter,
  paste0("Resul/intersection_Landtrendr_CCDC_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

writeRaster(
  mask(round(landt), inter, inverse = TRUE),
  paste0("Resul/Landtrendr_notinCCDC_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

writeRaster(
  mask(round(ccdc), inter, inverse = TRUE),
  paste0("Resul/CCDC_notinLandtrendr_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

# Create raster for map----
inter
temp_land <- mask(round(landt), inter, inverse = TRUE)
temp_land[temp_land >= 1] <- 1 
temp_ccdc <- mask(round(ccdc), inter, inverse = TRUE)
temp_ccdc[temp_ccdc >= 1] <- 1 

plot(temp_land)
plot(temp_ccdc)

writeRaster(
  temp_land,
  paste0("Resul/Landtrendr_notinCCDC_nocloudfilt_",landt_months,"_binary.tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

writeRaster(
  temp_ccdc,
  paste0("Resul/CCDC_notinLandtrendr_nocloudfilt_",landt_months,"_binary.tif"),
  datatype = "INT2S",
  overwrite = TRUE
)
# --- Summary raster: common vs exclusive disturbance / no-disturbance ---
# Within forest, classify every pixel by (LandTrendr change?, CCDC change?):
#   1 = Common disturbance             both methods detect change
#   2 = LandTrendr-exclusive disturb.  LT detects, CCDC does not
#                                      (= CCDC-exclusive no-disturbance)
#   3 = CCDC-exclusive disturbance     CCDC detects, LT does not
#                                      (= LandTrendr-exclusive no-disturbance)
#   4 = Common no-disturbance          neither method detects change
# non-forest pixels stay NA
landt_bin <- ifel(landt != 0, 1L, 0L)
ccdc_bin  <- ifel(ccdc  != 0, 1L, 0L)

map_class <- ifel(landt_bin == 1 & ccdc_bin == 1, 1L,
             ifel(landt_bin == 1 & ccdc_bin == 0, 2L,
             ifel(landt_bin == 0 & ccdc_bin == 1, 3L, 4L)))
names(map_class) <- "class"
levels(map_class) <- data.frame(
  value = 1:4,
  class = c("Common disturbance",
            "LandTrendr-exclusive disturbance",
            "CCDC-exclusive disturbance",
            "Common no-disturbance")
)
plot(map_class)

# Area per class
freq(map_class) |>
  as_tibble() |>
  mutate(areaha = count * 30 * 30 / 10000,
         perc   = areaha / sum(areaha) * 100)

writeRaster(
  map_class,
  paste0("Resul/DisturbanceSummary_commonExclusive_nocloudfilt_", landt_months, ".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

# Union-----
landt_u <- landt
ccdc_u <- ccdc
landt_u[landt_u != 0] <- 1
ccdc_u[ccdc_u != 0] <- 1
landt_u[landt_u == 0] <- 0
ccdc_u[ccdc_u == 0] <- 0
uni <- app(c(landt_u, ccdc_u), fun = "max")
# uni[uni == 0] <- NA

plot(uni)
plot(inter)

inter_df <- freq(inter) |>
  as_tibble()
uni_df <- freq(uni) |>
  as_tibble() |>
  filter(value != 0)

inter_df$count / uni_df$count

# Check date of detection -----
# disturbances detected in both methods
landt_timeDetection <- mask(landt_year, inter)
ccdc_timeDetection <- mask(ccdc_year, inter)

plot(landt_timeDetection)
plot(ccdc_timeDetection)

delta_time <- app(c(landt_timeDetection, (ccdc_timeDetection * -1)), fun = "sum")
plot(delta_time)

writeRaster(
  delta_time,
  paste0("Resul/deltaTime_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

# Landtrendr - ccdc
checker <- values(landt_timeDetection, na.rm = TRUE) |>
  as_tibble() |>
  bind_cols(values(ccdc_timeDetection, na.rm = TRUE) |>
              as_tibble()) |>
  bind_cols(values(delta_time, na.rm = TRUE) |>
              as_tibble()) |>
  rename("Landtrendr" = "yod",
         "CCDC" = "tBreak",
         "deltaTime" = "sum")

plotter <- values(delta_time, na.rm = TRUE) |>
  as_tibble()

plotter |>
  as_tibble() |>
  summarise(mean = mean(sum), sd = sd(sum))

range(plotter)

temp <- plotter |>
  group_by(sum) |>
  count() |>
  ungroup() |>
  mutate(perc = n/sum(n)) |>
  ungroup()

temp |>
  arrange(desc(perc))

common_plot <- temp |>
  ggplot(aes(x = sum, 
             y = perc)) +
  geom_col()+
  labs(
    x = "Difference change detection LandTrendr - CCDC (years)",
    y = " Pixel percentage (%)"
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  cowplot::theme_cowplot() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,0.8,0.2),
                     limits = c(0,0.8)) 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(
  paste0("Plots/deltaChangeDetectionLandsatminCCDC_nocloudfilt_",landt_months,".jpeg"),
  width = 16,
  height = 12,
  units = "cm",
  dpi = 300
)

writeRaster(
  round(delta_time),
  paste0("Resul/Landtrendr_CCDC_delta_timeDetection_nocloudfilt_",landt_months,".tif"),
  datatype = "INT2S",
  overwrite = TRUE
)

# Analyze most common errors by class -----

# Changes first
resul_driver <- verifpts |>
  st_drop_geometry() |>
  filter(sumVerif == 1) |> 
  filter(Proceso2 == "Cambio bosque") |> 
  mutate(driver2 = str_to_title(str_extract(Driver, "Degradación|degradacion|CUS|aguacate|Aguacate|degradación|Agricultura|agricultura"))) |>
  mutate(across(driver2, ~ifelse(is.na(.x), "Other", .x))) |>
  mutate(across(driver2, ~case_when(.x == "Degradación" ~ "Degradation",
                                    .x == "Cus" ~ "LULC",
                                    .x == "Agricultura" ~ "LULC",
                                    .x == "Aguacate" ~ "Avocado",
                                    TRUE ~ "Other"))) |>
  mutate(across(Model2, ~ ifelse(Type2 == "Common", "CCDC/LandTrendr", .x))) |>
  group_by(Proceso2, Model2, Type2, driver2) |>
  count() |>
  group_by(Proceso2, Model2, Type2) |>
  mutate(perc = n/sum(n) * 100) |>
  ungroup()

resul_driver |>
  ggplot(aes(x = driver2, y = perc)) +
  geom_col() +
  facet_wrap(~Model2) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,100)) +
  labs(x = "Disturbance driver", y = "Observations percentage (%)") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(
  paste0("Plots/driversValidation_",landt_months,".jpeg"),
  width = 16,
  height = 12,
  units = "cm",
  dpi = 300
)
