library(shiny)
library(RPostgres)
library(DBI)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(glue)
library(leaflet)
library(RColorBrewer)
library(celestial)
library(DataExplorer)
library(tmaptools)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# # Fuer den Dashboard-Bau wird mit einem lokalen DataFrame gearbeitet
# testdata <- load("data/poland_shiny.RData")
# testdata <- load("poland_shiny.RData")
# testdata <- poland_shiny

# ------------ NICHT LOESCHEN ! -------------------
db <- "schneefall_database"
db_host <- "v000727.edu.ds.fhnw.ch"
db_port <- "50011"
db_user <- 'schneefall'
db_pass <- "Klima2023"

conn <- dbConnect(
  Postgres(),
  dbname = db,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)

# ------- Spaltennamen der Tabellen ------------
# columns <- sort(dbListFields(conn, "cdk"))
# columns <- sort(columns)
#country <- unique(closest_stations()$country)
#ch_shiny <- dbGetQuery(conn, glue("SELECT * FROM {country} where month in (9,10,11,12,1,2,3) and staid in ({closest_stations()$staid})"))
# save(ch_shiny, file = "ch_shiny.RData")
# load("ch_shiny.RData")
# --------------- interne Listen -------------------

# # ------- Laender ------------
# # Liste Tabellen der Laender aus der DB
# country_db <- dbListTables(conn)
# patterns <- c("snow", "cdk")
# country_db <- country_db[!grepl(paste(patterns, collapse = "|"), country_db)]

# ------- Koordinaten ------------
# # erstellen einer zusaetzlichen Spalte mit dem Durchschnitt der Koordinaten pro Land
# coord_country <- dbGetQuery(conn, glue("SELECT DISTINCT (country), (lon), (lat) FROM cdk ORDER BY (country)"))
# coord_country <- coord_country %>% group_by(country) %>% summarise(lon = mean(lon), lat = mean(lat)) %>% distinct() %>% ungroup()

# Koordinaten von Europa (fuer den check ob ein Ort oder Land innerhalb Europa liegt)
# bbox_europe <- geocode_OSM("Europa")$bbox
bbox_europe <- bb("Europa")
# world <- ne_countries(scale = "medium", returnclass = "sf")

# Startwert fuer textInput country
val_loc <- "Brugg"

# ---------------- Stationen in eine interne Tabelle laden ------------
# stations <- dbGetQuery(conn, "select distinct(staid),staname, lat, lon, country from cdk")
# save(stations, file = "stations.RData")
# load("stations.RData")
load("data/stations.RData")

# distatance <- stations %>% 
#   rowwise() %>%
#   mutate(distanz = approx_distances(bs, matrix(c(lon, lat), nrow = 1, byrow = TRUE))) %>%
#   ungroup() %>% 
#   slice_min(distanz)


# %>%
#   mutate(distance = approx_distances(c(bs), vektor))



# # ----------- Jahres Eckpunkte -------------
# rng_y <- 30
# max_y <- max(dbGetQuery(conn, glue("SELECT MAX (year) FROM cdk")))
# min_y <- min(dbGetQuery(conn, glue("SELECT MIN (year) FROM cdk")))
# val_min_y_old <- 1950
# val_max_y_old <- 1980
# val_min_y <- max_y - rng_y
# val_max_y <- max_y

# # ------------ Stations-Informationen --------------
# min_sta <- dbGetQuery(conn, glue("SELECT MIN (stahght) FROM cdk"))
# max_sta <- dbGetQuery(conn, glue("SELECT MAX (stahght) FROM cdk"))
# 
# 
# 
# 
# 
# 
# # ------------------- Plots erstellen -------------------
# 
# # --------------- Plots von Ramin -------------------
# 
# # ---------------  Line Plot: Snow Depth over the years with dashed line for reference value -------------------
# # Calculate the baseline of the average snow depth for years between 1920 and 1950 and winter months November until April
# 
# reference <- poland %>%
#   filter(stahght == "Dukla, 427m") %>%
#   filter(year(date) >= 1950 & year(date) <= 1980 & month(date) %in% c(11, 12, 1, 2, 3, 4)) %>%
#   summarise(mean_sd = mean(sd)) %>%
#   pull(mean_sd)
# 
# 
# # Show a line plot
# 
# poland %>%
#   filter(stahght == "Dukla, 427m") %>%
#   filter(year(date) >= 1920 & month(date) %in% c(11, 12, 1, 2, 3, 4)) %>%
#   group_by(year = (year(date))) %>%
#   summarise(mean_sd = mean(sd)) %>%
#   ggplot(aes(x = year, y = mean_sd)) +
#   geom_smooth(se = FALSE, color = "#2671B5", linetype = "solid", size = 1.2) +
#   geom_hline(yintercept = reference, color = "red", linetype = "dashed", size = 1.2) +
#   labs(y = "Mean Snow Depth in cm", x = "") +
#   ggtitle("Mean Daily Snow Depth in Dukla, Poland") +
#   labs(subtitle = "Dashed Line: Mean Snow Depth between 1950 and 1980") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),
#     axis.text = element_text(size = 12),
#     axis.title.y = element_text(size = 14),
#     legend.position = "none",
#     panel.grid.major = element_line(color = "lightgray"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     plot.margin = margin(10, 20, 20, 10),
#     plot.subtitle = element_text(color = "red")
#   )
# 
# # --------------------- 3 Lines for comparisons between different stations ---------------------
# # Compare 3 different stations of various height ranges
# poland %>%
#   filter(stahght %in% c("Plock, 87m", "Zabkowice, 319m" , "Sloszow, 636m" )) %>%
#   ggplot(aes(x = year(date), y = sd, color = stahght)) +
#   geom_smooth(se = FALSE, size = 1.2) +
#   labs(
#     title = "Snow Depth of Selected Stations in Poland",
#     x = "",  # Remove the "Year" label
#     y = "Mean Snow Depth in cm",
#     color = "Station Name"
#   ) +
#   scale_color_manual(values = c("Plock, 87m" = "#E41A1C", "Zabkowice, 319m"  = "#377EB8", "Sloszow, 636m"  = "#66C2A5")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12),
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 11),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "lightgray"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()
#   )
# 
# # Histogram for monthly comparison of two periods in in Sniezka, Poland
# monthly_avg_combined <- rbind(
#   poland %>%
#     filter(stahght == "Sniezka, 958m") %>%
#     filter(year(date) >= 1950 & year(date) <= 1980) %>%
#     group_by(month = factor(format(date, "%m"), levels = sprintf("%02d", 1:12), labels = month.abb)) %>%
#     summarise(sd = mean(sd)) %>%
#     mutate(dataset = "Monthly Average 1"),
# 
#   poland %>%
#     filter(stahght == "Sniezka, 958m") %>%
#     filter(year(date) >= 2018 & year(date) <= 2023) %>%
#     group_by(month = factor(format(date, "%m"), levels = sprintf("%02d", 1:12), labels = month.abb)) %>%
#     summarise(sd = mean(sd)) %>%
#     mutate(dataset = "Monthly Average 2")
# )
# 
# 
# 
# ggplot(monthly_avg_combined, aes(x = month, y = sd, fill = dataset)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.5) +
#   labs(title = "Comparison of Monthly Snow Depth of 2 Periods in Poland",
#        subtitle = "Location: Sniezka, 958m",  # Add the subtitle
#        x = "",  # Remove the "Month" label
#        y = "Snow Depth in cm") +
#   scale_fill_manual(values = c("Monthly Average 1" = "#2671B5", "Monthly Average 2" = "#E9333F"),
#                     labels = c("1950 - 1980", "2018 - 2023")) +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 16, face = "bold"),
#         plot.subtitle = element_text(size = 14, face = "italic", margin = margin(b = 10)),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12),
#         legend.position = "top",
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank())
# 
# 
# # Plot snow depth by height range (this is of limited usability as there are more stations in later years, especially at higher altitudes, which skew the results)
# poland %>%
#   filter(year(date) >= 1920) %>%
#   group_by(year(date)) %>%
#   mutate(height_range = case_when(
#     hght < 450 ~ "Below 450m",
#     hght >= 451 & hght <= 600 ~ "451m - 600m",
#     hght > 600 ~ "Above 600m"
#   )) %>%
#   ggplot(aes(x = year(date), y = sd, color = height_range)) +
#   geom_smooth(se = FALSE, size = 1.2) +
#   labs(
#     title = "Snow Depth of Various Height Ranges in Poland",
#     x = "",  # Remove the "Year" label
#     y = "Mean Snow Depth in cm",
#     color = "Height Range"
#   ) +
#   scale_color_manual(values = c("Below 450m" = "#E41A1C", "451m - 600m" = "#377EB8", "Above 600m" = "#66C2A5")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12),
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 11),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "lightgray"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()
#   )
# 
# # ---------------------- Plots von Antonia ----------------------
# # --------------------- fig_1 ---------------------
# 
# length(levels(p2_clean$staid))
# length(levels(p1_clean$staid))
# 
# fig_1 <- p1_clean %>%
#   select(-c(lat, lon, staname, cn)) %>%
#   filter(sd > 0, month %in% c(9,10,11,12,1,2) )%>%
#   group_by(staid, winter_season) %>%
#   arrange(staid, date) %>%
#   distinct() %>%
#   mutate(day = day(date), yearmon = paste(winter_season, month)) %>%
#   slice_min(date) %>%
#   group_by(staid, month) %>%
#   count() %>%
#   ggplot(aes(staid, n, fill = month))+
#   geom_col(position= "dodge")+
#   scale_fill_brewer(palette="Blues")
# 
# # --------------------- fig_2 ---------------------
# 
# fig_2 <- p2_clean %>%
#   select(-c(lat, lon, staname, cn)) %>%
#   filter(sd > 0, month %in% c(9,10,11,12,1,2) )%>%
#   group_by(staid, winter_season) %>%
#   arrange(staid, date) %>%
#   distinct() %>%
#   mutate(day = day(date), yearmon = paste(winter_season, month)) %>%
#   slice_min(date) %>%
#   group_by(staid, month) %>%
#   count() %>%
#   ggplot(aes(staid, n, fill = month))+
#   geom_col(position= "dodge")+
#   scale_fill_brewer(palette="Blues")
# 
# # --------------------- p1 ---------------------
# p1 %>%
#   group_by(staid, winter_season, month) %>%
#   mutate(month = factor(month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12)),
#          staid = as.factor(staid)) %>%
#   arrange(year, month) %>%
#   summarize(snow = mean(sd)) %>%
#   ungroup() %>%
#   ggplot(aes(winter_season, snow, color = staid)) +
#   geom_line()
# # ------------------ fs_p1 ------------------
# # Create a vector of all dates between September and April
# dates <- seq(as.Date("2021-09-01"), as.Date("2022-04-30"), by = "day")
# md <- format(dates, "%m-%d")
# df_days <- data.frame(md, day_nr = 1:length(md))
# 
# first_snowday <- function(x){
#   df <- x %>%
#     filter(winter_season > min(year)) %>%
#     mutate(md = format(date,"%m-%d" )) %>%
#     inner_join(df_days, by = "md") %>%
#     filter(sd > 0) %>%
#     group_by(staid, winter_season) %>%
#     slice_min(day_nr)
#   df["period"] <- paste(min(df$year), max(df$year), sep = " - ")
#   return(df)
#   }
# 
# fs_p1<- first_snowday(p1_clean)
# fs_p2<- first_snowday(p2_clean)
# 
# 
# fs_p1 %>% filter(staid == 58) %>%
#   ggplot(aes(year, day_nr))+
#   geom_point()
# 
# 
# combined <- rbind(fs_p1, fs_p2)
# combined$period <- as.factor(combined$period)
# 
# summary(combined)
# avg_firstday_p1 <- mean(fs_p1$day_nr)
# avg_firstday_p2<-  mean(fs_p2$day_nr)
# 
# #densityplot -> Kurve der ersten Schneetage
# ggplot(combined, aes(x = day_nr, color = period))+
#   geom_density()+
#   geom_vline(xintercept = avg_firstday_p1, colour = "red", alpha = 0.5) +
#   geom_vline(xintercept = avg_firstday_p2, colour = "blue", alpha = 0.5) +
#   geom_vline(xintercept =  123, colour = "black", alpha = 0.5)
# 
# # ----------------- p2_new -----------------
# p2_new <- p2_clean %>%
#   filter(season == "winter") %>%
#   mutate(md = format(date,"%m-%d" )) %>%
#   inner_join(df_days, by = "md") %>%
#   filter(sd > 0) %>%
#   distinct() %>%
#   group_by(staid, year) %>%
#   slice_min(day_nr)
# 
# p2_new["period"] <- "1990-2020"
# 
# combined <- rbind(fs_p1, fs_p2)
# combined$period <- as.factor(combined$period)
# 
# ggplot(combined, aes(month, day_nr, color = staid))+
#   geom_boxplot() +
#   facet_grid(~period)
# 
# 
# 
# ggplot(combined, aes(day_nr, staid, fill = staid))+
#    geom_boxplot()+
#   facet_wrap(~period)
# 
# avg_firstday_p1 <- mean(p1_new$day_nr)
# avg_firstday_p2 <- mean(p2_new$day_nr)
# 
# 
# ggplot(combined, aes(day_nr, staid, fill = period))+
#    geom_boxplot() +
#   geom_vline(xintercept = avg_firstday_p1, colour = "red", alpha = 0.5) +
#     geom_vline(xintercept = avg_firstday_p2, colour = "blue", alpha = 0.5)
# 
# 
# #boxplots per station
# combined %>%
#   group_by(staid, period) %>%
#   summarize(avg_first_day = round(mean(day_nr),0)) %>%
#   ggplot(aes(avg_first_day, color = period))+
#   geom_density()
# 
# 
# 
# ggplot(combined, aes(x = year, y = day_nr, color = period))+
#   geom_density()
# 
# # ----------------- p1_new -----------------
# ggplot(p1_new, aes(x = winter_season, y = day_nr))+
#   geom_density()
# 
# # --------------- df_extreme ----------------
# df <-p1_clean %>%
#     filter(winter_season > min(year)) %>%
#     mutate(md = format(date,"%m-%d" )) %>%
#     inner_join(df_days, by = "md")
# 
# df_extreme <-p2_clean %>%
#     filter(winter_season > min(year)) %>%
#     mutate(md = format(date,"%m-%d" )) %>%
#     inner_join(df_days, by = "md") %>%
#     arrange(desc(winter_season)) %>%
#   slice(5)
# 
# ggplot(df, aes(day_nr, sd, color = ifelse(winter_season > 1949, "red", "blue")))+
#   geom_point( alpha = 0.1)
# 
# # ------------------ p1_clean ------------------
# p1_clean %>%
#   filter(season == "winter") %>%
#   mutate(md = format(date,"%m-%d" )) %>%
#   left_join(df_days, by = "md") %>%
#   mutate(snow = ifelse(sd > 0, 1, 0)) %>%
#   group_by(staid, day_nr) %>%
# 
#   summarize(p = round(mean(snow, na.rm = TRUE), 2)) %>%
#   arrange(desc(p))
# 
# # ------------------- plotly ------------------
# winters <- subset(p1_clean, season == "winter")
# p1_mean_snow <- mean(subset(p1_clean, season == "winter")$sd, na.rm = TRUE)
# 
# p1_clean %>%
#   filter(season == "winter") %>%
#   group_by(winter_season) %>%
#   summarize(avg_snow = mean(sd), difference = avg_snow - p1_mean_snow) %>%
#   ggplot(aes(winter_season, difference))+
#   geom_col()
# 
# 
# p2_mean_snow <- mean(subset(p2_clean, season == "winter")$sd, na.rm = TRUE)
# 
# p2_clean  %>%
#   filter(season == "winter") %>%
#   group_by(winter_season) %>%
#   summarize(avg_snow = mean(sd), difference = avg_snow - p1_mean_snow) %>%
#   arrange(desc(difference)) %>%
#   ggplot(aes(winter_season, difference, fill = ifelse(difference > 0, "blue", "red")))+
#   geom_hline(yintercept = p2_mean_snow -all_years_average,  color = "red")+
#   geom_col()
# 
# 
# p2_clean  %>%
#   filter(season == "winter") %>%
#   group_by(winter_season) %>%
#   summarize(avg_snow = mean(sd), difference = avg_snow - p2_mean_snow) %>%
#   arrange(desc(difference)) %>%
#   ggplot(aes(winter_season, difference, fill = ifelse(difference > 0, "blue", "red")))+
#   geom_col()
# 
# 
# plotly(p2_clean  %>%
#   filter(season == "winter") %>%
#   group_by(winter_season) %>%
#   summarize(avg_snow = mean(sd)) %>%
#   ggplot(aes(winter_season, avg_snow, fill = ifelse(avg_snow - p2_mean_snow < 0, "blue", "red")))+
#    geom_hline(yintercept = p2_mean_snow,  color = "red")+
#   geom_col())
# 
# # ------------------- all years ------------------
# p1_clean["period"] <- paste(min(p1_clean$year), max(p1_clean$year), sep = " - ")
# p2_clean["period"] <- paste(min(p2_clean$year), max(p2_clean$year), sep = " - ")
# 
# all_years <- rbind(p1_clean, p2_clean)
# 
# all_years_average <- mean(subset(all_years, season == "winter")$sd, na.rm = TRUE)
# 
# 
# all_years  %>%
#   filter(season == "winter") %>%
#   group_by(winter_season) %>%
#   summarize(avg_snow = mean(sd), difference = avg_snow - all_years_average) %>%
#   arrange(desc(difference)) %>%
#   ggplot(aes(winter_season, difference, fill = ifelse(difference > 0, "blue", "red")))+
#   geom_hline(yintercept = p2_mean_snow -all_years_average,  color = "red")+
#   geom_hline(yintercept = p1_mean_snow -all_years_average,  color = "Blue")+
#   geom_col()
# 
# 
# all_years %>%
#   filter(season == "winter") %>%
#   mutate(height = case_when(hght < 500 ~ "<500",
#                             hght < 1000 ~ "<1000",
#                             hght < 1500 ~ "<1500",
#                             hght < 2000 ~"<2000",
#                             hght >= 2000 ~">2000")) %>%
#   group_by(period, winter_season, height) %>%
#   summarize(snow = mean(sd)) %>%
#   ggplot(aes(winter_season, snow, color = height))+
#   geom_line()+
#   geom_smooth()+
#   facet_wrap(~period)
# 
# 
# all_years %>%
#   filter(season == "winter") %>%
#   mutate(height = case_when(hght < 500 ~ "<500",
#                             hght < 1000 ~ "<1000",
#                             hght < 1500 ~ "<1500",
#                             hght < 2000 ~"<2000",
#                             hght >= 2000 ~">2000")) %>%
#   group_by(period, winter_season, height) %>%
#   summarize(avg_snow = mean(sd)) %>%
#   group_by(height, period) %>%
#   slice_max(avg_snow)%>%
#   arrange(period)
# 
# 
# all_years %>%
#   filter(season == "winter") %>%
#   mutate(height = case_when(hght < 500 ~ "<500",
#                             hght < 1000 ~ "<1000",
#                             hght < 1500 ~ "<1500",
#                             hght < 2000 ~"<2000",
#                             hght >= 2000 ~">2000")) %>%
#   group_by(period, winter_season, height) %>%
#   summarize(avg_snow = mean(sd)) %>%
#   group_by(height, period) %>%
#   slice_min(avg_snow) %>%
#   arrange(period)