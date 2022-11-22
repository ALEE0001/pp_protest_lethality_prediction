library("jsonlite")

json_file <- 'https://datahub.io/core/corruption-perceptions-index/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}

cpi_0 <- 
  data %>% 
  pivot_longer(cols = X1998:X2015) %>% 
  mutate(Year = sub("X", "", name) %>% as.numeric()) %>% 
  mutate(value = as.numeric(value)) %>%
  select(Year, Country = Jurisdiction, cpi_score = value) %>%
  filter(Year <= 2009) %>%
  distinct()

cpi_1 <- 
  read_csv("content/portfolio/data/Mass Mobilization Protests/corruption_perception_index.csv") %>%
  select(Year, Country, cpi_score = "CPI Score")

cpi_2 <- 
  read_csv("content/portfolio/data/Mass Mobilization Protests/cpi_data.csv") %>%
  pivot_longer(cols = `CPI score 2021`:`CPI score 2020`) %>%
  mutate(Year = sub("CPI score ", "", name) %>% as.numeric()) %>%
  select(Year, Country = `Country / Territory`, cpi_score = value)

iso3 <- 
  read_csv("content/portfolio/data/Mass Mobilization Protests/cpi_data.csv") %>%
  select(Country = `Country / Territory`, iso_3 = ISO3) %>% distinct()

df_cpi <- 
  rbind(cpi_0, cpi_1, cpi_2) %>% 
  left_join(iso3, by = "Country") %>% 
  mutate(iso_3 = case_when(Country == "Belize" ~ "BLZ",
                          Country == "Bosnia & Herzegovina" ~ "BIH",
                          Country == "Bosnia and Herzgegovina" ~ "BIH",
                          Country == "Brunei" ~ "BRN",
                          Country == "CÃ´te-d'Ivoire" ~ "CIV",
                          Country == "CÃ´te dÂ´Ivoire" ~ "CIV",
                          Country == "Côte d'Ivoire" ~ "CIV",
                          Country == "Cote dÂ´Ivoire" ~ "CIV",
                          Country == "Cape Verde" ~ "CPV",
                          Country == "Congo-Brazzaville" ~ "COG",
                          Country == "Congo Brazzaville" ~ "COG",
                          Country == "Congo Democratic Republic" ~ "COG",
                          Country == "Congo. Democratic Republic" ~ "COG",
                          Country == "Democratic Republic of Congo" ~ "COG",
                          Country == "Congo  Republic" ~ "COD",
                          Country == "Congo Republic" ~ "COD",
                          Country == "Congo Republic of" ~ "COD",
                          Country == "Congo Republic of the" ~ "COD",
                          Country == "Congo. Republic" ~ "COD",
                          Country == "Congo. Republic of" ~ "COD",
                          Country == "Czech Republic" ~ "CZE",
                          Country == "Czech Republik" ~ "CZE",
                          Country == "Kuweit" ~ "KWT",
                          Country == "Serbia and Montenegro" ~ "SRB",
                          Country == "Serbia & Montenegro" ~ "SRB",
                          Country == "Taijikistan" ~ "TJK",
                          Country == "Yugoslavia" ~ "YUG",
                          TRUE ~ iso_3
                  )) %>%
  mutate(iso_3 = case_when(is.na(iso_3) ~ countrycode(Country, "country.name", "iso3c"),
                           TRUE ~ iso_3)) %>%
  select(-Country) %>%
  as.data.frame()


write.csv(df_cpi, file = "content/portfolio/data/Mass Mobilization Protests/df_cpi.csv", row.names = FALSE)



















