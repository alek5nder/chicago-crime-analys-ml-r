dane <- read.csv("2016-2020-chicago-crime.csv", sep=",")

dane %>% 
  summarize(sum(duplicated(Case.Number)))#mamy 160 duplikatów


dane <- dane %>% distinct(Case.Number, .keep_all = TRUE)

dane %>% group_by(Primary.Type)%>%summarize(count=n())%>%arrange(desc(count))

data_clean <- data %>%
  mutate(
    Date = mdy_hms(Date),
    Year = year(Date),
    Month = month(Date),
    District = as.factor(District)
  ) %>%
  filter(!is.na(District)) 

data <- data %>% select( -c("X", "ID", "Case.Number", "Location", "Updated.On")) 
data <- data %>% rename("Type" = "Primary.Type")


