#Skript Svalbard

# Packages laden

library("dplyr")
library("lme4")
library("sf")
library("terra")
library("lubridate")
library("tidyverse")
library("luna")
library("geodata")
library("viridis")
library("stringr")
library("ggplot2")

# 1. Datenaquirierung


studyarea <- gadm("SJM", level = 0, path = getwd())
studyarea <- studyarea[studyarea$NAME_1=="Svalbard",]


files = list.files("MOD10A1/",
                   pattern = "\\.tif$",
                   full.names = TRUE)
#length(files)

## Datum extrahieren
library(stringr)

fname <- basename(files)

dates <- rep(NA, length(files))


# Format 1: YYYYMMDD
idx1 <- str_detect(fname, "_[0-9]{8}T")

dates[idx1] <- str_extract(
  fname[idx1],
  "(?<=_)[0-9]{8}(?=T)"
)


# Format 2: doyYYYYDDD
idx2 <- str_detect(fname, "doy")

doy_string <- str_extract(
  fname[idx2],
  "doy[0-9]{7}"
)

year <- as.numeric(substr(doy_string, 4, 7))
doy  <- as.numeric(substr(doy_string, 8, 10))


dates[idx2] <- format(
  as.Date(paste0(year, "-01-01")) + doy - 1,
  "%Y%m%d"
)


dates_real <- as.Date(dates, "%Y%m%d")
sum(is.na(dates_real))

length(dates_real)
length(unique(dates_real))


head(dates_real)
tail(dates_real)

## Rasterstack
dat =  rast(files)
time(dat) <- dates_real

head(time(dat))
tail(time(dat))

is.unsorted(time(dat))
sum(duplicated(time(dat)))

### Fehlende Tage

dates_all = seq(min(dates_real),
                max(dates_real),
                by = "day")

missing_dates_filtered <- missing_dates[
  !(as.numeric(format(missing_dates, "%m")) %in% c(11,12,1,2))
]

missing_dates_filtered


## MODIS Qualitätswerte entfernen
#NDSI snow cover values and data flags values, stored as 8-bit unsigned integers:
# 0–100: NDSI snow cover; 200: missing data; 201: no decision; 211: night
#237: inland water; 239: ocean; 250: cloud; 254: detector saturated; 255: fill

# MODIS Qualitätswerte entfernen
dat_clean = ifel(dat <= 100, dat, NA)

time(dat_clean) <- dates_real # Zeit nochmal setzen

## Testweise Plotten
plot(dat[[201]])
plot(dat_clean[[7]])

# Mittelwert/summe Schneebedeckung, %verfügbare Pixel
values(dat_clean [[7]])
summary(dat_clean[[7]])
mean_snow <- global(dat_clean, "mean", na.rm=TRUE)


writeRaster(dat,"Modis_svalbard.tif",overwrite=T,datatype="INT4S")
writeRaster(dat_clean,"Modis_svalbard_clean.tif",overwrite=T,datatype="INT4S")

# 2. Daten einladen

setwd("C:/data")
dat <- rast("Modis_svalbard.tif")
dat_clean <- rast("Modis_svalbard_cleanfull.tif")

studyarea <- st_read("svalbard.gpkg")

studyarea <- st_transform(
  studyarea,
  crs(dat_clean)
)


crs(dat_clean)
st_crs(studyarea)

# Daten extrahieren

metrics <- data.frame(date = time(dat_clean),
                      mean_snow = NA,
                      snow_pixels = NA,
                      available_pixels = NA,
                      total_pixels = ncell(dat_clean),
                      available_percent = NA,
                      snow_cover_percent = NA)

for(i in 1:nlyr(dat_clean)){
  vals <- values(dat_clean[[i]])
  
  # verfügbare Pixel
  available <- sum(!is.na(vals))
  
  # Pixel mit >=50% Schnee
  snow <- sum(vals >= 50, na.rm=TRUE)
  
  metrics$mean_snow[i] <- mean(
    vals,
    na.rm=TRUE)
  
  metrics$snow_pixels[i] <- snow
  
  metrics$available_pixels[i] <- available
  
  metrics$available_percent[i] <- 
    available / metrics$total_pixels[i] * 100
  
  metrics$snow_cover_percent[i] <-
    snow / available * 100}

#Erste CSV herausschreiben

write.csv(metrics, "metrics_new.csv")

head(metrics)
summary(metrics)

plot(metrics$date,
     metrics$snow_cover_percent,
     type="l")

vals <- values(dat_clean)
vals <- values(dat_year)

#Schneebedeckung ermitteln

mean_snow <- colMeans(vals, na.rm = TRUE)
available_pixels <- colSums(!is.na(vals))
snow_pixels <- colSums(vals >= 50, na.rm = TRUE)

metrics <- data.frame(
  date = time(dat_year),
  mean_snow = mean_snow,
  snow_pixels = snow_pixels,
  available_pixels = available_pixels,
  total_pixels = ncell(dat_year),
  available_percent = available_pixels / ncell(dat_year) * 100,
  snow_cover_percent = snow_pixels / available_pixels * 100
)^
  
  ####
  
  mean_snow <- global(dat_clean_mask, "mean", na.rm=TRUE)[,1]

snow_pixels <- global(
  dat_clean_mask >= 50,
  "sum",
  na.rm=TRUE
)[,1]


available_pixels <- global(
  !is.na(dat_clean_mask),
  "sum"
)[,1]

metrics <- data.frame(
  date = time(dat_clean_mask),
  mean_snow = mean_snow,
  snow_pixels = snow_pixels,
  available_pixels = available_pixels,
  total_pixels = ncell(dat_clean_mask)
)


metrics$available_percent <-
  metrics$available_pixels / metrics$total_pixels * 100

metrics$snow_cover_percent <-
  metrics$snow_pixels / metrics$available_pixels * 100

write.csv(metrics, "metrics_masked.csv")
##
####

mean_snow <- global(dat_clean_crop, "mean", na.rm=TRUE)[,1]

snow_pixels <- global(
  dat_clean_crop >= 50,
  "sum",
  na.rm=TRUE
)[,1]

available_pixels <- global(
  !is.na(dat_clean_crop),
  "sum"
)[,1]

metrics <- data.frame(
  date = time(dat_clean_crop),
  mean_snow = mean_snow,
  snow_pixels = snow_pixels,
  available_pixels = available_pixels,
  total_pixels = ncell(dat_clean_crop)
)

metrics$available_percent <-
  metrics$available_pixels / metrics$total_pixels * 100

metrics$snow_cover_percent <-
  metrics$snow_pixels / metrics$available_pixels * 100

write.csv(metrics, "metrics_crop.csv")
####
#dat_clean_mask = mask(dat_clean,studyarea)
plot(dat_clean_mask)
dat_clean_crop = crop(dat_clean, studyarea)
plot(dat_clean_crop)
writeRaster(dat_clean_mask, "Modis_svalbard_cleanfull_maske.tif")


# Datensatz einlesen
metrics_v2 <- read.csv("metrics_v2", stringsAsFactors = FALSE)

# Datum umwandeln
metrics_v2$date <- as.Date(metrics_v2$date)

# Nur Juni, Juli und August auswählen
metrics_sommer <- metrics_v2 %>%
  mutate(
    Jahr = year(date),
    Monat = month(date)
  ) %>%
  filter(Monat %in% c(6, 7, 8))


# Nach Jahren gruppieren
gruppen <- metrics_sommer %>%
  group_by(Jahr)

# Überblick
gruppen

#Statistik
statistik <- metrics_sommer %>%
  group_by(Jahr) %>%
  summarise(
    Median = median(snow_cover_percent, na.rm = TRUE),
    Standardabweichung = sd(snow_cover_percent, na.rm = TRUE),
    n = n()
  )
print(statistik)
summary(statistik)


#Metrics v3 mit Begrenzung auf 2001 bis ENde 2025 erstellen
metrics_v3 = metrics_v2[c(164:7865),]
metrics_v3
write.csv(metrics_v3, "metrics_v3.csv")

# 2.MITTLERE SCHNEEBEDECKUNG alle 5 jahre:
ggplot(metrics_sub,
       aes(month,
           snow_cover_percent,
           group = year,
           color = year)) +
  
  geom_line(linewidth = 1.5) +
  
  scale_color_manual(
    values = c(
      "2000" = "#2166AC",
      "2005" = "#67A9CF",
      "2010" = "#D1E5F0",
      "2015" = "#FDDBC7",
      "2020" = "#EF8A62",
      "2025" = "#B2182B"
    )
  ) +
  
  theme_bw() +
  
  labs(
    x = "Monat",
    y = "Schneebedeckung (%)",
    color = "Jahr"
  )

#SCHNEEBEDECKUNG alle 5 Jahre
#langjähriges Monatsmittel = schwarze Strichlierung:
monthly_mean <- metrics_v2 %>%
  group_by(year, month) %>%
  summarise(
    snow_mean = mean(snow_cover_percent, na.rm = TRUE),
    .groups = "drop"
  )

monthly_mean_5 <- monthly_mean %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020, 2025))


longterm_mean <- monthly_mean %>%
  group_by(month) %>%
  summarise(
    snow_mean = mean(snow_mean, na.rm = TRUE)
  )
summary(monthly_mean_5)

## Spalte fürs Jahr hinzufügen und Halbjahre rausfiltern
metrics_v2$date = as.Date(metrics_v2$date)
metrics_v2$year = as.integer(format(metrics_v2$date, "%Y"))
metrics_v2 = metrics_v2 %>%
  filter(year != 2000, year != 2026)

## Jahresaggregate bilden
metrics_V2 = metrics_v2 %>%
  mutate(
    period5 = case_when(
      year <= 2005 ~ "2001–2005",
      year <= 2010 ~ "2006–2010",
      year <= 2015 ~ "2011–2015",
      year <= 2020 ~ "2016–2020",
      TRUE         ~ "2021–2025"))

summary(subset(metrics_V2, period5 == "2001–2005"))

sd(
  subset(metrics_V2, period5 == "2001–2005")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_V2, period5 == "2006–2010"))

sd(
  subset(metrics_V2, period5 == "2006–2010")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_V2, period5 == "2011–2015"))

sd(
  subset(metrics_V2, period5 == "2011–2015")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_V2, period5 == "2016–2020"))

sd(
  subset(metrics_V2, period5 == "2016–2020")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_V2, period5 == "2021–2025"))

sd(
  subset(metrics_V2, period5 == "2021–2025")$snow_cover_percent,
  na.rm = TRUE
)

# 2. Long term snow
# Monatliche Mittelwerte (Juni–August) über alle Jahre

metrics_sommerV2 <- metrics_v2 %>%
  mutate(
    Jahr = year(date),
    Monat = month(date)
  ) %>%
  filter(Monat %in% c(6, 7, 8)) %>%
  mutate(
    period5 = case_when(
      Jahr >= 2001 & Jahr <= 2005 ~ "2001–2005",
      Jahr >= 2006 & Jahr <= 2010 ~ "2006–2010",
      Jahr >= 2011 & Jahr <= 2015 ~ "2011–2015",
      Jahr >= 2016 & Jahr <= 2020 ~ "2016–2020",
      Jahr >= 2021 & Jahr <= 2025 ~ "2021–2025",
      TRUE ~ NA_character_
    ),
    period5 = factor(
      period5,
      levels = c("2001–2005", "2006–2010", "2011–2015",
                 "2016–2020", "2021–2025")
    )
  )

monthly_snow <- metrics_sommerv2 %>%
  mutate(month = lubridate::month(date)) %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(period5, month) %>%
  summarise(
    snow_cover_mean = mean(snow_cover_percent, na.rm = TRUE),
    .groups = "drop"
  )

longterm_snow <- metrics_sommerV2 %>%
  mutate(month = lubridate::month(date)) %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(month) %>%
  summarise(
    snow_cover_mean = mean(snow_cover_percent, na.rm = TRUE),
    .groups = "drop"
  )


a1 <- ggplot(
  monthly_snow,
  aes(
    x = factor(month,
               levels = c(6, 7, 8),
               labels = c("Jun", "Jul", "Aug")),
    y = snow_cover_mean,
    group = 1
  )
)

summary(subset(metrics_sommerV2, period5 == "2001–2005"))

sd(
  subset(metrics_sommerV2, period5 == "2001–2005")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_sommerV2, period5 == "2006–2010"))

sd(
  subset(metrics_sommerV2, period5 == "2006–2010")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_sommerV2, period5 == "2011–2015"))

sd(
  subset(metrics_sommerV2, period5 == "2011–2015")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_sommerV2, period5 == "2016–2020"))

sd(
  subset(metrics_sommerV2, period5 == "2016–2020")$snow_cover_percent,
  na.rm = TRUE
)

summary(subset(metrics_sommerV2, period5 == "2021–2025"))

sd(
  subset(metrics_sommerV2, period5 == "2021–2025")$snow_cover_percent,
  na.rm = TRUE
)

#2.

metrics_v3 = read.csv("metrics_v3.csv")

summary(metrics_v3)

clim_dat=read.csv("svalbard_wetter_monatsmittel_2000-01_bis_2026-06.csv")

TS_daily=read.csv("svalbard_merra2_oberflaechentemperatur_taeglich_2001-2025.csv")

metrics_v3$date=as.character(metrics_v3$date)

TS_daily_date_transform = metrics_v3%>%dplyr::left_join(TS_daily,by = "date")

TS_ddt=TS_daily_date_transform

TS_ddt = TS_ddt[complete.cases(TS_ddt),]

write.csv(TS_ddt, "metrics_v4.csv")

metrics_v4 = read.csv("metrics_v4.csv")

GLMM=lmer(TS_ddt$mean_snow~TS_ddt$oberflaechentemperatur_ts_mittel_c+(1|TS_ddt$year))
summary(GLMM)

GLM_Jahr=glm(TS_ddt$mean_snow~TS_ddt$oberflaechentemperatur_ts_mittel_c+TS_ddt$year)
summary(GLM_Jahr)

LM_Jahr=lm(TS_ddt$mean_snow~TS_ddt$oberflaechentemperatur_ts_mittel_c+TS_ddt$year)
summary(LM_Jahr)

lm0= glm(TS_ddt$mean_snow~1)

ANV_GLM_lm0 = anova(GLM_Jahr,lm0)

summary(metrics_v4)

sd(metrics_v4$oberflaechentemperatur_ts_mittel_c)

sd(TS_ddt$mean_snow)
