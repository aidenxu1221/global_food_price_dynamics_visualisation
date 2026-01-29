library(dplyr)
library(readxl)
library(ggplot2)
library(countrycode)
library(tidyverse)
library(maps)
library(leaflet)
library(sf)
library(rnaturalearth)
library(ggExtra)
library(RColorBrewer)
library(scales)
library(rnaturalearthdata)
library(units)
library(ggrepel)
library(cluster)
library(ggalt)
library(viridis)
library(gganimate)
library(readr)


## PPI wrangling
producer_price <- read.csv("Prices_E_All_Data_NOFLAG.csv", fileEncoding = "ISO-8859-1")


producer_price <- producer_price %>% 
  mutate(Area = gsub("Belgium-Luxembourg", "Luxembourg", Area))


producer_price$Country <- countrycode(producer_price$Area, origin = "country.name", destination = "country.name")
unique(producer_price[is.na(producer_price$Country), "Area"])

ppi_cols_remove <- c("Area.Code", "Area.Code..M49.", "Area", "Item.Code", "Item.Code..CPC.",
                     "Element.Code", "Element", "Months.Code", "Months", "Unit")
producer_price <- producer_price %>% 
  filter(Element == "Producer Price Index (2014-2016 = 100)",
         Item %in% c("Cereals, primary", "Meat, Total", "Vegetables Primary", "Fruit Primary"),
         Months == "Annual value") %>% 
  select(-all_of(ppi_cols_remove)) %>% 
  mutate(Item = gsub("Cereals, primary", "Cereal", Item),
         Item = gsub("Meat, Total", "Meat", Item),
         Item = gsub("Vegetables Primary", "Vegetables", Item),
         Item = gsub("Fruit Primary", "Fruit", Item))


# convert long format and remove data before 2000
ppi <- producer_price %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "value"
  ) %>%
  mutate(Year = str_remove(Year, "^Y")) %>% 
  filter(grepl("^(20[0-2][0-9])$", Year),
         Year != "2024") %>% 
  mutate(Item = ifelse(Item %in% c("Fruit", "Vegetables"), "Vegetables and Fruit", Item)) %>% 
  group_by(Country, Item, Year) %>%
  summarise(PPI = mean(value, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Year = as.integer(Year))

summary(ppi)



### CPI wrangling
consumer_price <- read.csv("ConsumerPriceIndices_E_All_Data_NOFLAG.csv", fileEncoding = "ISO-8859-1")

consumer_price$Country <- countrycode(consumer_price$Area, origin = "country.name", destination = "country.name")


## cpi_country
consumer_price <- consumer_price %>% 
  filter(!is.na(Country),
         Item %in% c("Food price inflation")) %>% 
  select(-all_of(c("Area.Code", "Area.Code..M49.", "Item.Code", "Element.Code",
                   "Element", "Months.Code", "Unit", "Area")))

# convert long format and remove data after 2023
cpi <- consumer_price %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "value"
  ) %>%
  mutate(Year = str_remove(Year, "^Y")) %>% 
  filter(Year != "2024",
         Year != "2000") %>%
  mutate(
    Food_Price_Inflation = case_when(
      Item == "Food price inflation" ~ value,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(-Item, -value) %>% 
  group_by(Country, Year, Months) %>% 
  summarise(Food_Price_Inflation = mean(Food_Price_Inflation, na.rm = TRUE),
            .groups = 'drop') %>% 
  mutate(Year = as.integer(Year)) %>% 
  group_by(Country, Year) %>% 
  summarise(Food_Price_Inflation = mean(Food_Price_Inflation, na.rm = TRUE),
            .groups = 'drop')

summary(cpi)

### GPI
## wrangling
production_indice <- read.csv("Production_Indices_E_All_Data_NOFLAG.csv", fileEncoding = "ISO-8859-1")

production_indice <- production_indice %>% 
  filter(Area != 'Australia and New Zealand')
  

production_indice$Country <- countrycode(production_indice$Area, origin = "country.name", destination = "country.name")


production_indice <- production_indice %>% 
  filter(!is.na(Country),
         Item %in% c("Cereals, primary", "Meat indigenous, total", "Vegetables and Fruit Primary")) %>% 
  select(-all_of(c("Area.Code", "Area.Code..M49.", "Area", "Item.Code", "Item.Code..CPC.", "Element.Code", "Unit"))) %>% 
  mutate(Item = gsub("Cereals, primary", "Cereal", Item),
         Item = gsub("Meat indigenous, total", "Meat", Item),
         Item = gsub("Vegetables and Fruit Primary", "Vegetables and Fruit", Item))


gpi <- production_indice %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "value"
  ) %>%
  mutate(Year = str_remove(Year, "^Y")) %>% 
  filter(grepl("^(20[0-2][0-9])$", Year),
         Year != "2024") %>%
  mutate(
    GPI = case_when(
      Element == "Gross Production Index Number (2014-2016 = 100)" ~ value,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(-Element, -value) %>% 
  group_by(Country, Item, Year) %>%
  summarise(GPI = mean(GPI, na.rm = TRUE),
            .groups = 'drop') %>% 
  mutate(Year = as.integer(Year))


summary(gpi)


### aggregate cpi, gpi, ppi

QA <- ppi %>%
  full_join(gpi, by = c("Country", "Item", "Year")) %>%
  full_join(cpi, by = c("Country", "Year"))

QA <- QA[(!is.na(QA$Item)), ]
summary(QA)


# 第一步：计算每个Item和Year组合的ppi均值
group_ppi <- QA %>%
  group_by(Item, Year) %>%
  summarise(median_ppi = median(PPI, na.rm = TRUE),
            .groups = 'drop') %>%
  ungroup()

QA <- QA %>%
  left_join(group_ppi, by = c("Item", "Year")) %>%
  mutate(PPI = ifelse(is.na(PPI), median_ppi, PPI)) %>% 
  select(-median_ppi)

group_gpi <- QA %>%
  group_by(Country, Item) %>%
  summarise(median_gpi = median(GPI, na.rm = TRUE),
            .groups = 'drop') %>%
  ungroup()

QA <- QA %>%
  left_join(group_gpi, by = c("Country", "Item")) %>%
  mutate(GPI = ifelse(is.na(GPI), median_gpi, GPI)) %>% 
  select(-median_gpi)



### MACRO ECONOMIC
## wrangling
macro_indicator <- read.csv("Macro-Statistics_Key_Indicators_E_All_Data_NOFLAG.csv", fileEncoding = "ISO-8859-1")

macro_indicator <- macro_indicator %>% 
  filter(Area != 'Australia and New Zealand')

macro_indicator$Country <- countrycode(macro_indicator$Area, origin = "country.name", destination = "country.name")


macro_indicator <- macro_indicator %>% 
  filter(!is.na(Country),
         (Item == "Gross Domestic Product" & Element == "Value US$, 2015 prices")) %>% 
  select(-all_of(c("Area.Code", "Area.Code..M49.", "Area", "Item.Code", "Element.Code", "Unit")))


gdp <- macro_indicator %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "value"
  ) %>%
  mutate(Year = str_remove(Year, "^Y")) %>% 
  filter(grepl("^(20[0-2][0-9])$", Year),
         Year != "2024") %>%
  mutate(
    GDP = case_when(
      Item == "Gross Domestic Product" & Element == "Value US$, 2015 prices" ~ value,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(-Item, -Element, -value) %>% 
  group_by(Country, Year) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE),
            .groups = 'drop') %>% 
  mutate(Year = as.integer(Year))


QA <- QA %>% 
  left_join(gdp, by = c("Country", "Year"))

summary(QA)

QA <- QA %>% 
  group_by(Country, Item) %>% 
  arrange(Country, Item, Year) %>% 
  mutate(
    ppi_rate = (PPI / lag(PPI) - 1) * 100,
    gpi_rate = (GPI / lag(GPI) - 1) * 100,
    gdp_rate = (GDP / lag(GDP) - 1) * 100
  ) %>% 
  ungroup()


QA <- QA %>%
  mutate(inflation_group = case_when(
    is.na(Food_Price_Inflation) ~ "No data",
    Food_Price_Inflation < -100 ~ "Below -100%",
    Food_Price_Inflation >= -100 & Food_Price_Inflation < -75 ~ "-100% to -75%",
    Food_Price_Inflation >= -75 & Food_Price_Inflation < -50 ~ "-75% to -50%",
    Food_Price_Inflation >= -50 & Food_Price_Inflation < -25 ~ "-50% to -25%",
    Food_Price_Inflation >= -25 & Food_Price_Inflation < -10 ~ "-25% to -10%",
    Food_Price_Inflation >= -10 & Food_Price_Inflation < 0 ~ "-10% to 0%",
    Food_Price_Inflation >= 0 & Food_Price_Inflation < 10 ~ "0% to 10%",
    Food_Price_Inflation >= 10 & Food_Price_Inflation < 25 ~ "10% to 25%",
    Food_Price_Inflation >= 25 & Food_Price_Inflation < 50 ~ "25% to 50%",
    Food_Price_Inflation >= 50 & Food_Price_Inflation < 75 ~ "50% to 75%",
    Food_Price_Inflation >= 75 & Food_Price_Inflation < 100 ~ "75% to 100%",
    Food_Price_Inflation >= 100 ~ "Above 100%"
  ))


QA <- QA[(QA$Year != 2000), ]


### happiness
world_happiness <- read_excel("DataForTable2.1.xls", sheet = "Sheet1")


world_happiness$Country <- countrycode(world_happiness$`Country name`, origin = "country.name", destination = "country.name")


world_happiness <- world_happiness %>% 
  select(Country, year, `Life Ladder`) %>% 
  rename(Year = year,
         Life_Ladder = `Life Ladder`)


fill_missing_years <- function(df) {
  # 创建完整的年份序列(2005-2023)
  all_years <- 2005:2023
  
  # 获取所有唯一的国家
  countries <- unique(df$Country)
  
  # 创建完整的Country-Year组合
  complete_grid <- expand.grid(
    Country = countries,
    Year = all_years) %>% 
    arrange(Country, Year)
  
  # 将原始数据与完整网格合并
  filled_df <- complete_grid %>%
    left_join(df, by = c("Country", "Year"))
  
  # 按国家计算Life_ladder的平均值
  country_means <- filled_df %>%
    group_by(Country) %>%
    summarise(mean_life_ladder = mean(Life_Ladder, na.rm = TRUE))
  
  # 用国家平均值填充缺失值
  final_df <- filled_df %>%
    left_join(country_means, by = "Country") %>%
    mutate(Life_Ladder = ifelse(is.na(Life_Ladder), mean_life_ladder, Life_Ladder)) %>%
    select(-mean_life_ladder)  # 删除均值列
  
  return(final_df)
}

world_happiness <- fill_missing_years(world_happiness)


QA <- QA %>% 
  left_join(world_happiness, by = c("Country", "Year"))



QA <- QA %>%
  mutate(across(c(Food_Price_Inflation, ppi_rate, gpi_rate), 
                ~pmin(., 200)))


### health stat
world_health <- read_excel("web_download.xlsx", sheet = "data")

world_health$Country <- countrycode(world_health$DIM_GEO_NAME, origin = "country.name", destination = "country.name")


## wrangling
world_health <- world_health %>% 
  filter(!is.na(Country),
         IND_NAME %in% c("Age-standardized prevalence of obesity among adults (18+ years) (%)",
                         "Prevalence of obesity among children and adolescents (5–19 years) (%)",
                         "Prevalence of overweight in children under 5 (%)",
                         "Prevalence of stunting in children under 5 (%)")) %>%
  select(IND_NAME, Country, DIM_TIME_YEAR, VALUE_NUMERIC) %>% 
  rename(Health_Indicator = IND_NAME,
         Year = DIM_TIME_YEAR,
         Rate = VALUE_NUMERIC) %>% 
  mutate(Health_Indicator = gsub("Age-standardized prevalence of obesity among adults (18+ years) (%)", 
                                 "adult_obesity", 
                                 Health_Indicator,
                                 fixed = TRUE),
         Health_Indicator = gsub("Prevalence of obesity among children and adolescents (5–19 years) (%)", 
                                 "teen_obesity", 
                                 Health_Indicator,
                                 fixed = TRUE),
         Health_Indicator = gsub("Prevalence of overweight in children under 5 (%)", 
                                 "child_obesity", 
                                 Health_Indicator,
                                 fixed = TRUE),
         Health_Indicator = gsub("Prevalence of stunting in children under 5 (%)", 
                                 "child_stunting", 
                                 Health_Indicator,
                                 fixed = TRUE)) %>% 
  pivot_wider(
    id_cols = c(Country, Year),
    names_from = Health_Indicator,
    values_from = Rate
  )

world_health <- world_health %>%
  mutate(
    child_obesity = replace_na(child_obesity, median(child_obesity, na.rm = TRUE)),
    child_stunting = replace_na(child_stunting, median(child_stunting, na.rm = TRUE))
  )

world_health <- world_health %>% 
  left_join(
    QA[,c("Country", "Year", "Food_Price_Inflation")],
    by = c("Country", "Year")
  )

summary(world_health)

world_health <- world_health[!is.na(world_health$Food_Price_Inflation),]

sf::sf_use_s2(FALSE)
Sys.setenv(SHAPE_RESTORE_SHX="YES")
world_spatial <- st_read("ne_110m_admin_0_countries.shp")

shapefile_countries <- unique(world_spatial$ADMIN)


# 创建手动映射字典
country_mapping <- c(
  "Bahamas" = "The Bahamas",
  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "Congo - Brazzaville" = "Republic of the Congo",
  "Congo - Kinshasa" = "Democratic Republic of the Congo",
  "Côte d’Ivoire" = "Ivory Coast",
  "Eswatini" = "eSwatini",
  "Myanmar (Burma)" = "Myanmar",
  "Palestinian Territories" = "Palestine",
  "Serbia" = "Republic of Serbia",
  "Tanzania" = "United Republic of Tanzania",
  "Timor-Leste" = "East Timor",
  "Trinidad & Tobago" = "Trinidad and Tobago",
  "United States" = "United States of America"
)


QA <- QA %>%
  mutate(
    Country = case_when(
      Country %in% names(country_mapping) ~ country_mapping[Country],
      Country %in% shapefile_countries ~ Country,
      TRUE ~ Country  # 保持原样如果没有匹配
    )
  )


world_health <- world_health %>%
  mutate(
    Country = case_when(
      Country %in% names(country_mapping) ~ country_mapping[Country],
      Country %in% shapefile_countries ~ Country,
      TRUE ~ Country  # 保持原样如果没有匹配
    )
  )

world_health <- world_health[!duplicated(world_health),]

world_median_data <- QA %>%
  group_by(Year, Item) %>%
  summarise(
    Food_Price_Inflation = median(Food_Price_Inflation, na.rm = TRUE),
    ppi_rate = median(ppi_rate, na.rm = TRUE),
    gpi_rate = median(gpi_rate, na.rm = TRUE),
    gdp_rate = median(gdp_rate, na.rm = TRUE),
    Life_Ladder = median(Life_Ladder, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Country = "World Median")

QA <- bind_rows(QA, world_median_data)

health_median_data <- world_health %>%
  group_by(Year) %>%
  summarise(
    adult_obesity = median(adult_obesity, na.rm = TRUE),
    teen_obesity = median(teen_obesity, na.rm = TRUE),
    child_obesity = median(child_obesity, na.rm = TRUE),
    child_stunting = median(child_stunting, na.rm = TRUE),
    Food_Price_Inflation = median(Food_Price_Inflation, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Country = "World Median")

world_health <- bind_rows(world_health, health_median_data)


write_csv(QA, "output/FIT5147DVP_food_price.csv")
write_csv(world_health, "output/FIT5147DVP_price_health.csv")

