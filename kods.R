install.packages("installr")
library(installr)
updateR()

library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)
library(rstudioapi)
library(ggplot2)
library(tidyr)
library(zoo)
library(janitor)

current_path = rstudioapi::getActiveDocumentContext()$path

setwd(dirname(current_path ))


data <- read_excel("dati.xlsx")

#data <-clean_names(data)

data$year <- year(ymd_hms(data$DateTime))
data$month <- lubridate::month(ymd_hms(data$DateTime))

data_group <- data %>%
  dplyr::filter(Group == "Cladocera") %>%
  filter(month > 5 & month < 10) %>%
  filter(!is.na(`wet_weight-WW`)) #%>%
  #filter(Station == "119")


data_group2 <- data_group %>%
  count(Waterbody, WaterType, Station, year, month, DateTime) %>%
  count(Waterbody, WaterType, Station, year, month) %>%
  count(Station, year) %>%
  mutate(year = as.numeric(as.character(year))) %>%   #konverteju
  arrange(year) %>% #gadi sakartoti secigi
  tidyr::pivot_wider(names_from = "Station", values_from = "n", values_fill = 0) %>% 
  arrange(year)

print(colnames(data_group2))

write.xlsx(data_group2, "stacijas.xlsx")


#heatmap izveide

data2 <- read_excel("stacijas.xlsx")

data_df <- as.data.frame(data2)

colnames(data_df)[1] <- "Year"
data_df <- data_df %>%
  mutate(across(everything(), as.numeric))


filtered_stations <- data_df %>%
  pivot_longer(cols = -Year, names_to = "Station", values_to = "Count") %>%
  group_by(Station) %>%
  filter(sum(!is.na(Count) & Count > 0) >= 20) %>% #optimals variants 20
  ungroup() %>%
  pivot_wider(names_from = Station, values_from = Count, values_fill = list(Count = 0))


data_long <- filtered_stations %>%
  pivot_longer(cols = -Year, names_to = "Station", values_to = "Count") %>%
  mutate(Year = as.factor(Year))


ggplot(data_long, aes(x = Station, y = Year, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +  
  labs(x = "Stacija", y = "Gads") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1.5)) 


#stacija


data3 <- read_excel("dati.xlsx")

data3 <- data3 %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%S"))

data3 <- data3 %>%
  mutate(Year = year(DateTime),
         Month = month(DateTime))

data3_filtered <- data3 %>%
  dplyr::filter(Taxa == "Bosmina coregoni") %>%
  dplyr::filter(Station %in% c("101A","121", "121A", "142","162", "163", "165", "167", "170","119"))%>%
  dplyr::filter(Month > 5 & Month < 10) %>%
  dplyr::filter(!is.na(SizeClass))

print(head(data3_filtered))
print(summary(data3_filtered$Year))


### 1.4. Layers/whole column ####
### Sampling from layers (not the whole column)
## identify information on the sampled layer and compare to depth considered for the station (see 'MSTS_StatnRef.txt')
b <-data3_filtered %>%
  select(DateTime,Year,Month,Station,StationVisitId, `Station Visit`, Layer) %>%
  tidyr::separate(col = "Layer", into = c("MNDEP", "MXDEP"), sep = "-", remove = TRUE) %>%
  # return all sampled layers for every unique sampling event# return all sampled layers for every unique sampling event# return all sampled layers for every unique sampling event
  # create groups for splitting dataset into every unique station visit (including all layers sampled)
  group_by(DateTime,Year,Month,Station,StationVisitId, `Station Visit`) %>%
  reframe(layer_from = MNDEP, layer_to = MXDEP) %>%
  group_by(DateTime,Year,Month,Station,StationVisitId, `Station Visit`) %>%
  mutate(`Depth_minLimit[m]` = 18) %>%
  # create variable that will hold the information whether the layer should be included or not
  # value is set to 99 (to be different from the resulting values [i.e. 1 or] and to set variable to numeric) 
  unique() %>% mutate(layer_use = 99) %>%
  # split df into list by groups
  group_split()

## Loop looks for samples that do not represent the considered water column (see 'MSTS_StatnRef.txt'), e.g. are deeper
### Variation of sampled depth vs defined `BotDepth_considered[m]` depth has been considered (-30% and +20% variation)
for(n in seq_along(b)){
  
  for(L in 1:nrow(b[[n]])){
    if(b[[n]]$layer_to[L] %in% b[[n]]$layer_from){
      b[[n]]$layer_use[L] = 1
    }else if(as.numeric(b[[n]]$layer_to[L]) >= b[[n]]$`Depth_minLimit[m]`[L])
    {b[[n]]$layer_use[L] = 1
    }else{b[[n]]$layer_use[L] = 0}
  }
  
  ##
  #### look for cases when the maximal depth was smaller then defined `Depth_minLimit[m]`
  #### in order to be able to remve all sampled layers for the STNNO from further calculations for these cases  
  # if(max(as.numeric(b[[n]]$layer_to)) < b[[n]]$`Depth_minLimit[m]`[L]){
  #   b[[n]]$layer_use = 0
  # }
  # if(!is.na(b[[n]]$`Depth_maxLimit[m]`[L])){
  #   if(as.numeric(b[[n]]$layer_to[L]) > b[[n]]$`Depth_maxLimit[m]`[L]){
  #     b[[n]]$layer_use[L] = 0
  #   }}
} 
## .$layer_use == 1 if the sampling event covers all of the considered water column and 
### have information for all successive layers from top to bottom

## .$layer_use == 0 if the sample is collected deeper (relevant for Landsort Deep where top 30 meters are considered)
### samples representing 30-60 meters are marked with 0
### also samples that do not have the next successive layer sampled 
### (e.g. samples 0-10, 10-41, 10-60, 60-200: sample from layer 10-41 is excluded .$out == 0)

######################################################################################.
## make df from the list!

b <- data.frame(Reduce(rbind, b)) %>%
  select(-layer_from, -layer_to, -`Depth_minLimit.m.`)

#izmērs

size <- c("<0.3" = 0.25, "0.3-0.4" = 0.35, "0.4-0.5" = 0.45, "0.5-0.6" = 0.55,
          "0.6-0.8" = 0.7, "0.8-1.0" = 0.9, ">1.0" = 1)

data3_filtered <- data3_filtered %>%
  mutate(size_class_numeric = size[as.character(SizeClass)])

data3_counts <- data3_filtered %>%
  group_by(Year, SizeClass) %>%
  summarise(Count = n(), size_class_numeric = size[as.character(SizeClass)], .groups = 'drop')

data3_weighted_avg_size <- data3_counts %>%
  group_by(Year) %>%
  summarise(total_weighted_size = sum(size_class_numeric * Count, na.rm = TRUE),
  total_count = sum(Count, na.rm = TRUE), 
  weighted_avg_size = total_weighted_size / total_count, .groups = 'drop')

print(data3_weighted_avg_size)



ggplot(data3_weighted_avg_size, aes(x = Year, y = weighted_avg_size)) +
  geom_point() +                
  geom_line() +  
  geom_smooth(method = "lm", se = TRUE) + 
  labs( x = "Year", y = "Size") +
  theme_minimal()           


#zooplankton mean size (MS; µg wet mass/ind.)  mg/m3//ind/m3

print(data3_filtered)

data_filtered <- data3_filtered %>%
  mutate(
    `wet_weight-WW` = as.numeric(`wet_weight-WW`),
    `abundance-AB` = as.numeric(`abundance-AB`)
  ) %>%
  filter(!is.na(`wet_weight-WW`), !is.na(`abundance-AB`))

mean_size<- data_filtered%>%
  group_by(Year) %>%
  summarise(
    total_biomass = sum(`wet_weight-WW`, na.rm = TRUE),   
    total_abundance = sum(`abundance-AB`, na.rm = TRUE),  
    mean_size = total_biomass / total_abundance,          
    .groups = 'drop'
  )
  
print(mean_size)

ggplot(mean_size, aes(x = Year, y = mean_size)) +
  geom_point() + geom_line() +                     
  geom_smooth(method = "lm", se = TRUE) +  
  labs( x = "Year", y = "Mean size") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )





#skabeklis

skabeklis <- read_excel("skabeklis.xlsx")
print(skabeklis)

skabeklis$year <- year(ymd_hms(skabeklis$date_time))
skabeklis$month <- lubridate::month(ymd_hms(skabeklis$date_time))

filtered_data$value2 <- as.numeric(filtered_data$value2)

Vid_skab <- filtered_data %>%
  group_by(year) %>%
  summarise(average_value = mean(value2, na.rm = TRUE), .groups = 'drop')

print(Vid_skab)


merged_data <- left_join(data3_weighted_avg_size, Vid_skab, by = c("Year" = "year"))
print(merged_data)


model <- lm(weighted_avg_size ~ average_value, data = merged_data)
summary(model)













# data3_avg_size <- data3_filtered %>%
#   mutate(size_class_numeric = size[as.character(SizeClass)]) %>%
#   group_by(Year) %>%
#   summarise(avg_size_class = mean(size_class_numeric, na.rm = TRUE), .groups = 'drop')
#
# print(data3_avg_size)

#interp

 # data3_avg_size <- data3_avg_size %>%
 #  complete(Year = seq(min(Year), max(Year), by = 1)) %>%
 #  mutate(avg_size_class = na.approx(avg_size_class, na.rm = FALSE))

#modelis
# model <- lm(weighted_avg_size ~ Year, data = data3_weighted_avg_size)
# summary(model)
#
# slope <- coef(model)["Year"]
# intercept <- coef(model)["(Intercept)"]
#
# ggplot(data3_weighted_avg_size, aes(x = Year, y = weighted_avg_size)) +
#   geom_line() +geom_point() +
#   geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "dashed") +
#   labs(title = "Bosmina coregoni", x = "Gads", y = "Vidējais izmērs") +theme_minimal()
#
# print(summary(model))

# ggplot(data3_avg_size, aes(x = Year, y = avg_size_class)) +
#   geom_line() +geom_point() +
#   geom_smooth(method = "loess", se = FALSE)+
#   scale_x_continuous(
#   breaks = seq(min(data3_avg_size$Year, na.rm = TRUE), max(data3_avg_size$Year, na.rm = TRUE), by = 1),
#   limits = c(min(data3_avg_size$Year, na.rm = TRUE), max(data3_avg_size$Year, na.rm = TRUE))) +
#   labs(title = "Vidējais izmērs",x = "Gads",y = "Izmērs") +
#   theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# #modelis
# 
# model <- lm(avg_size_class ~ Year, data = data3_avg_size)
# summary(model)
# 
# slope <- coef(model)["Year"]
# intercept <- coef(model)["(Intercept)"]
# 
# ggplot(data3_avg_size, aes(x = Year, y = avg_size_class)) +
#   geom_line() + geom_point() +
#   geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "dashed") +
#   labs(title = "Bosmina izmērs", x = "Gads", y = "Izmērs") + theme_minimal()
# 
# print(summary(model))




