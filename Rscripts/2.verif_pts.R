library(terra)
library(tidyverse)
library(sf)
library(yardstick)
library(mapaccuracy)

# Preprocessing------
landt_months <- "6_12"

# Desde acatama
## Overall expected error = 0.0100
## Todas min pts = 50
## 500 Ui a priori = 0.95; 132 pts
## 600 Ui a priori = 0.95; 371 pts
n_verif_perm <- 371
n_verif_chg <- 50

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

verifpts <- st_read(
  "VerifPts/verifpts8_fulltimeAll.gpkg"
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

# Get magnitude and match images
landt_year <- landt["yod"]
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

# # Check histograms----
reclassifier <- function(im, model) {
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
landt_rec <- reclassifier(landt, "landt")
ccdc_rec <- reclassifier(ccdc, "ccdc")

# Reclasificados

# Intersection
landt_iou <- landt_rec
ccdc_iou <- ccdc_rec

landt_iou[landt_iou>= 1] <- 1
landt_iou[landt_iou== 0] <- NA
ccdc_iou[ccdc_iou>= 1] <- 1
ccdc_iou[ccdc_iou == 0] <- NA

inter <- mask(landt_iou, ccdc_iou)

inter[inter == 1] <- 1
inter[inter == 0] <- NA
plot(inter)

length(values(inter, na.rm = TRUE))

plot(forest)

# Check extent of forest and ccdc_rec match
# No son iguales el número de pixeles de bosque y no cambio de lantrendr o ccdc.
length(values(forest, na.rm = TRUE))
length(values(ccdc_rec, na.rm = TRUE))

# Generar rasters de muestreo----
## Cambios
## cambios exclusivos landt
landt_ch_excl <- mask(landt_rec, inter, inverse = TRUE)
landt_ch_excl[landt_ch_excl == 0] <- NA 
plot(landt_ch_excl)

## cambios exlusivos ccdc
ccdc_ch_excl <- mask(ccdc_rec, inter, inverse = TRUE)
ccdc_ch_excl[ccdc_ch_excl == 0] <- NA
plot(ccdc_ch_excl)

## cambios común landt
landt_ch_comm <- mask(landt_rec, inter, inverse = FALSE)
landt_ch_comm[landt_ch_comm == 0] <- NA 
plot(landt_ch_comm)

## cambios común ccdc
ccdc_ch_comm <- mask(ccdc_rec, inter, inverse = FALSE)
ccdc_ch_comm[ccdc_ch_comm == 0] <- NA
landt_ccdc_ch_comm <- c(landt_ch_comm, ccdc_ch_comm)
plot(landt_ccdc_ch_comm)

length(values(ccdc_ch_comm, na.rm = TRUE))
length(values(landt_ch_comm, na.rm = TRUE))

## no cambios
## Inter común forest
landt_temp <- forest
landt_temp[landt_temp == 1] <- 0
landt_temp <- landt_temp + landt_rec

plot(landt_temp)

ccdc_temp <- forest
ccdc_temp[ccdc_temp == 1] <- 0
ccdc_temp <- ccdc_temp + ccdc_rec

plot(ccdc_temp)

inter2 <- landt_temp + ccdc_temp
inter2[inter2 != 0] <- NA
plot(inter2, col = "darkgreen")

## no cambios exclusivos landt
landt_nch_excl <- mask(landt_temp, inter2, inverse = TRUE)
landt_nch_excl[landt_nch_excl != 0] <- NA 
plot(landt_nch_excl, col = "darkgreen")

## no cambios exlusivos ccdc
ccdc_nch_excl <- mask(ccdc_temp, inter2, inverse = TRUE)
ccdc_nch_excl[ccdc_nch_excl != 0] <- NA 
plot(ccdc_nch_excl, col = "darkgreen")

## no cambios común landt y ccdc (como no hay magnitud es lo mismo)
landt_temp <- forest
landt_temp[landt_temp == 1] <- 0
landt_temp <- landt_temp + landt_rec
landt_nch_comm <- mask(landt_temp, inter2, inverse = FALSE)
landt_nch_comm[landt_nch_comm != 0] <- NA 
plot(landt_nch_comm, col = "darkgreen")

# Raster único
plot(landt_ch_excl)
plot(ccdc_ch_excl)
plot(landt_ccdc_ch_comm)

plot(landt_nch_excl)
plot(ccdc_nch_excl)
plot(landt_nch_comm)

rasters_l <- list(landt_ch_excl, ccdc_ch_excl, landt_ccdc_ch_comm, 
             landt_nch_excl, ccdc_nch_excl, landt_nch_comm)

rasters_l |>
  map(~length(values(.x, na.rm = TRUE)))

# Raster for Atacama
exp_r <- c(landt_ch_excl,
       ccdc_ch_excl+100,
       landt_ccdc_ch_comm[[1]]+200,
       # Comentado para evitar que se sumen las coincidencias
       # landt_ccdc_ch_comm[[2]]+300,
       landt_nch_excl + 400,
       ccdc_nch_excl + 500,
       landt_nch_comm + 600)

exp_r <- app(exp_r, function(x) sum(x, na.rm = TRUE))
exp_r[exp_r == 0]<-NA
plot(exp_r)

freq(exp_r)
# No debería haber valores de 501-510
# Checador
temp <- mask(ccdc_ch_excl, landt_nch_excl)
plot(temp)


writeRaster(exp_r,
            "Raster/rast4verif_nocloudFilter.tif",
            datatype = "INT2S",
            overwrite = TRUE)

# Revisar si datos de validación anteriores cambiarían----

# Comparar raster anterior con actual
exp_r_1 <- rast("Raster/rast4verif.tif")
exp_r_2 <- exp_r

# exp_r_2[exp_r_2 == exp_r_1] <- NA
# length(values(exp_r_2, na.rm = TRUE))
plot(exp_r_2)
plot(exp_r_1)
# Interpr
# 0-10: landt_ch_excl,
# 100: ccdc_ch_excl+100,
# 200: landt_ccdc_ch_comm[[1]]+200,
# 400: landt_nch_excl + 400,
# 500: ccdc_nch_excl + 500,
# 600: landt_nch_comm + 600

# Primeros puntos de verificación (con cloud filter)
verifpts_pre <- st_read("Results/verifpts_landt_vs_ccdc_2000-2025.gpkg")

landt_ch_excl[landt_ch_excl>=1] <- 1
ccdc_ch_excl[ccdc_ch_excl>=1] <- 1
landt_ccdc_ch_comm[[1]][landt_ccdc_ch_comm[[1]]>=1] <- 1 
landt_ccdc_ch_comm[[2]][landt_ccdc_ch_comm[[2]]>=1] <- 1
landt_nch_excl
ccdc_nch_excl
landt_nch_comm

temp_l_ch_ex <- terra::extract(landt_ch_excl, vect(verifpts_pre))
temp_c_ch_ex <- terra::extract(ccdc_ch_excl, vect(verifpts_pre))
temp_l_ch_co <- terra::extract(landt_ccdc_ch_comm[[1]], vect(verifpts_pre))
temp_c_ch_co <- terra::extract(landt_ccdc_ch_comm[[2]], vect(verifpts_pre))
temp_l_nch_ex <- terra::extract(landt_nch_excl, vect(verifpts_pre))
temp_c_nch_ex <- terra::extract(ccdc_nch_excl, vect(verifpts_pre))
temp_c_nch_co <- terra::extract(landt_nch_comm, vect(verifpts_pre))

temp_df <- list(temp_l_ch_ex, 
     temp_c_ch_ex,
     temp_l_ch_co,
     temp_c_ch_co,
     temp_l_nch_ex,
     temp_c_nch_ex,
     temp_c_nch_co) |>
  bind_cols() 

temp_df <- temp_df |>
  mutate(Proceso2 = case_when(mag...2 >= 1 ~ "Cambio bosque", 
                             MAG...4 >= 1 ~ "Cambio bosque", 
                             mag...6 >= 1 ~ "Cambio bosque", 
                             MAG...8 >= 1 ~ "Cambio bosque", 
                             constant...10 == 0 ~ "No cambio bosque", 
                             constant...12 == 0 ~ "No cambio bosque", 
                             constant...14 == 0 ~ "No cambio bosque", 
                             ),
         Model2 = case_when(mag...2 >= 1 ~ "LandTrendr", 
                             MAG...4 >= 1 ~ "CCDC", 
                             mag...6 >= 1 ~ "LandTrendr", 
                             MAG...8 >= 1 ~ "CCDC", 
                             constant...10 == 0 ~ "LandTrendr", 
                             constant...12 == 0 ~ "CCDC", 
                             constant...14 == 0 ~ "CCDC", 
         ),
         Type2 = case_when(mag...2 >= 1 ~ "Exclusive", 
                             MAG...4 >= 1 ~ "Exclusive", 
                             mag...6 >= 1 ~ "Common", 
                             MAG...8 >= 1 ~ "Common", 
                             constant...10 == 0 ~ "Exclusive", 
                             constant...12 == 0 ~ "Exclusive", 
                             constant...14 == 0 ~ "Common", 
         )) |>
  select(id, Proceso2, Model2, Type2)

temp_checker <- verifpts_pre |>
  bind_cols(temp_df)

temp_checker |>
  filter(Proceso != Proceso2 | Model != Model2 | Type != Type2)

temp_checker |>
  group_by(Proceso, Model, Type) |>
  count()

temp_checker |>
  group_by(Proceso2, Model2, Type2) |>
  count()


# Option 2 weird shit happening----
temp2 <- extract(exp_r_2, verifpts_pre)
temp <- extract(exp_r_1, verifpts_pre)
temp2 <- temp2 |>
  as_tibble() |>
  mutate(Model2 = case_when(
    lyr.1 <= 10 ~ "Landtrendr",
    lyr.1 >= 10 & lyr.1 <= 110~ "CCDC",
    lyr.1 >= 110 & lyr.1 <= 210~ "Landtrendr",
    lyr.1 >= 210 & lyr.1 <= 310~ "CCDC",
    lyr.1 >= 310 & lyr.1 <= 410~ "Landtrendr",
    lyr.1 >= 410 & lyr.1 <= 510~ "CCDC",
    lyr.1 >= 510 & lyr.1 <= 610~ "Landtrendr",
  ),
  Type2 = case_when(
    lyr.1 <= 10 ~ "Exclusive",
    lyr.1 >= 10 & lyr.1 <= 110~ "Exclusive",
    lyr.1 >= 110 & lyr.1 <= 210~ "Common",
    lyr.1 >= 210 & lyr.1 <= 310~ "Common",
    lyr.1 >= 310 & lyr.1 <= 410~ "Exclusive",
    lyr.1 >= 410 & lyr.1 <= 510~ "Exclusive",
    lyr.1 >= 510 & lyr.1 <= 610~ "Common",
  ))

temp <- temp |>
  as_tibble() |>
  mutate(Model1 = case_when(
    lyr.1 <= 10 ~ "Landtrendr",
    lyr.1 >= 10 & lyr.1 <= 110~ "CCDC",
    lyr.1 >= 110 & lyr.1 <= 210~ "Landtrendr",
    lyr.1 >= 210 & lyr.1 <= 310~ "CCDC",
    lyr.1 >= 310 & lyr.1 <= 410~ "Landtrendr",
    lyr.1 >= 410 & lyr.1 <= 510~ "CCDC",
    lyr.1 >= 510 & lyr.1 <= 610~ "Landtrendr",
  ),
  Type1 = case_when(
    lyr.1 <= 10 ~ "Exclusive",
    lyr.1 >= 10 & lyr.1 <= 110~ "Exclusive",
    lyr.1 >= 110 & lyr.1 <= 210~ "Common",
    lyr.1 >= 210 & lyr.1 <= 310~ "Common",
    lyr.1 >= 310 & lyr.1 <= 410~ "Exclusive",
    lyr.1 >= 410 & lyr.1 <= 510~ "Exclusive",
    lyr.1 >= 510 & lyr.1 <= 610~ "Common",
  ))

t_df <- verifpts_pre |>
  bind_cols(temp) |>
  bind_cols(temp2) 

# 82 diferentes
t_df |> 
  filter(Model1 != Model2 | Type1 != Type2 | (Model1 != Model2 & Type1 != Type2))

t_df |>
  group_by(Model1, Type1) |>
  count()

t_df |>
  group_by(Model2, Type2) |>
  count()

# Generar puntos de muestreo-----
# Desde acatama
## Overall expected error = 0.0100
## Todas min pts = 50
## 500 Ui a priori = 0.95; 132 pts
## 600 Ui a priori = 0.95; 371 pts

# Validación solo cambios/no cambios sin magnitud
# Cambios
set.seed(3)
landt_ch_excl[landt_ch_excl>=1] <- 1
landt_ch_excl_pts <- spatSample(landt_ch_excl,
                  size = n_verif_chg,
                  method = "stratified",
                  # replace = TRUE,
                  na.rm = TRUE,
                  as.points = TRUE,
                  exhaustive = TRUE,
                  exp = 10,
                  warn = FALSE)
year_chg_excl <- terra::extract(landt_year, landt_ch_excl_pts)
landt_ch_excl_pts <- landt_ch_excl_pts |>
  st_as_sf() |>
  mutate(fechaCambio = year_chg_excl$yod)

set.seed(8)
ccdc_ch_excl[ccdc_ch_excl>=1] <- 1
ccdc_ch_excl_pts <- spatSample(ccdc_ch_excl,
                  size = n_verif_chg,
                  method = "stratified",
                  # replace = TRUE,
                  na.rm = TRUE,
                  as.points = TRUE,
                  exhaustive = TRUE,
                  exp = 10,
                  warn = FALSE)

year_chg_excl <- terra::extract(ccdc_year, ccdc_ch_excl_pts)
ccdc_ch_excl_pts <- ccdc_ch_excl_pts |>
  st_as_sf() |>
  mutate(fechaCambio = year_chg_excl$tBreak)

set.seed(18)
landt_ccdc_ch_comm[[1]][landt_ccdc_ch_comm[[1]]>=1] <- 1 
landt_ch_comm_pts <- spatSample(landt_ccdc_ch_comm[[1]],
                               size = n_verif_chg,
                               method = "stratified",
                               # replace = TRUE,
                               na.rm = TRUE,
                               as.points = TRUE,
                               exhaustive = TRUE,
                               exp = 10,
                               warn = FALSE)
year_chg_excl <- terra::extract(landt_year, landt_ch_comm_pts)
landt_ch_comm_pts <- landt_ch_comm_pts |>
  st_as_sf() |>
  mutate(fechaCambio = year_chg_excl$yod)

set.seed(6)
landt_ccdc_ch_comm[[2]][landt_ccdc_ch_comm[[2]]>=1] <- 1
ccdc_ch_comm_pts <- spatSample(landt_ccdc_ch_comm[[2]],
                            size = n_verif_chg,
                            method = "stratified",
                            # replace = TRUE,
                            na.rm = TRUE,
                            as.points = TRUE,
                            exhaustive = TRUE,
                            exp = 10,
                            warn = FALSE)
year_chg_excl <- terra::extract(ccdc_year, ccdc_ch_comm_pts)
ccdc_ch_comm_pts <- ccdc_ch_comm_pts |>
  st_as_sf() |>
  mutate(fechaCambio = year_chg_excl$tBreak)

# No cambios
set.seed(2)
landt_nch_excl_pts <- spatSample(landt_nch_excl,
                           size = n_verif_chg,
                           method = "stratified",
                           # replace = TRUE,
                           na.rm = TRUE,
                           as.points = TRUE,
                           exhaustive = TRUE,
                           exp = 10,
                           warn = FALSE)
set.seed(5)
ccdc_nch_excl_pts <- spatSample(ccdc_nch_excl,
                                 size = n_verif_perm,
                                 method = "stratified",
                                 # replace = TRUE,
                                 na.rm = TRUE,
                                 as.points = TRUE,
                                 exhaustive = TRUE,
                                 exp = 10,
                                 warn = FALSE)
set.seed(1)
landt_nch_comm_pts <- spatSample(landt_nch_comm,
                                 size = n_verif_perm,
                                 method = "stratified",
                                 # replace = TRUE,
                                 na.rm = TRUE,
                                 as.points = TRUE,
                                 exhaustive = TRUE,
                                 exp = 10,
                                 warn = FALSE)

resul <- list(landt_ch_excl_pts, 
     ccdc_ch_excl_pts,
     landt_ch_comm_pts,
     ccdc_ch_comm_pts,
     landt_nch_excl_pts,
     ccdc_nch_excl_pts,
     landt_nch_comm_pts) |>
  map(~st_as_sf(.x))

# Agregar campo de capa
resul[[1]] <- resul[[1]] |>
  mutate(Model = "Landtrendr",
         Type = "Exclusive")
resul[[2]] <- resul[[2]] |>
  mutate(Model = "CCDC",
         Type = "Exclusive")
resul[[3]] <- resul[[3]] |>
  mutate(Model = "Landtrendr",
         Type = "Common")
resul[[4]] <- resul[[4]] |>
  mutate(Model = "CCDC",
         Type = "Common")
resul[[5]] <- resul[[5]] |>
  mutate(Model = "Lantrendr",
         Type = "Exclusive")
set.seed(10)
resul[[6]] <- resul[[6]] |>
  sample_n(size = 132) |>
  mutate(Model = "CCDC",
         Type = "Exclusive")
resul[[7]] <- resul[[7]] |>
  mutate(Model = "CCDC",
         Type = "Common")

resul <- resul |>
  bind_rows()

# Estandarizar todo
resul <- resul |>
  mutate(
    magnitude = dplyr::coalesce(mag, MAG),
    id = dplyr::row_number()
  ) |>
  mutate(Proceso = ifelse(magnitude == 1, "Cambio bosque", "No cambio bosque"))|>
  mutate(across(Proceso, ~ifelse(is.na(.x), "No cambio bosque", .x))) |>
  mutate(sum = ifelse(Proceso == "Cambio bosque", 1, 0)) |>
  mutate(name = id) |>
  mutate(fechaCambioVerif = as.numeric(""),
         sumVerif = as.numeric(""),
         ProcesoVerif  = as.character(""),
         Obs  = as.character("")) |>
  select(name, sum, sumVerif, Proceso, ProcesoVerif, fechaCambio, fechaCambioVerif, Obs, Model, Type, geometry)

# Escribir puntos----
set.seed(5)
daniel <- resul |>
  group_by(Proceso, sum) |>
  slice_sample(prop = 0.5) |>
  mutate(responsable = "Daniel")|>
  arrange(name) |>
  ungroup()  |>
  select(name, responsable, everything())

sugey <- resul |>
  anti_join(daniel |>
              st_drop_geometry(),
            by = "name") |>
  group_by(Proceso, sum) |>
  mutate(responsable = "Sugey")|>
  arrange(name)|>
  ungroup()  |>
  select(name, responsable, everything())

verifpts <- list(daniel, sugey) |>
  bind_rows() |>
  select(name, responsable, everything()) |>
  arrange(name) 

# Segundos puntos de verificación
st_write(verifpts,
         "Results/verifpts_landt_vs_ccdc_2000-2025_extra.gpkg",
         append = FALSE)

st_write(verifpts,
         layer = "veripts_chgnchg_",
         "Results/verifpts_landt_vs_ccdc_2000-2025_extra.kml",
         append = FALSE)

walk2(list(daniel, sugey), 
      list("Daniel", "Sugey"),
      function(x, y){
        st_write(x,
                 paste0("Results/verifpts_landt_vs_ccdc_2000-2025_",y,"_extra.gpkg"),
                 append = FALSE)
        st_write(x,
                 layer = paste0("veripts_chgnchg_", y),
                 paste0("Results/verifpts_landt_vs_ccdc_2000-2025_",y,"_extra.kml"),
                 append = FALSE)
      })

# Buffer
walk2(list(daniel, sugey), 
      list("Daniel", "Sugey"),
      function(x, y){
        st_write(x|>
                   st_buffer(dist = 15,
                             endCapStyle = "SQUARE"),
                 paste0("Results/verifpts_landt_vs_ccdc_2000-2025_",y,"_buff_extra.gpkg"),
                 append = FALSE)
        st_write(x|>
                   st_buffer(dist = 15,
                             endCapStyle = "SQUARE"),
                 layer = paste0("veripts_chgnchg_", y),
                 paste0("Results/verifpts_landt_vs_ccdc_2000-2025_",y,"_buff_extra.kml"),
                 append = FALSE)
      })
