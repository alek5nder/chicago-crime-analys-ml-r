data <- data %>%
  mutate(Type = case_when(
    grepl("NON\\s?-\\s?CRIMINAL", Type) ~ "NON-CRIMINAL",
    grepl("NARCOTIC", Type) ~ "NARCOTICS",
    .default = Type
  ))

data <- data %>%
  mutate(Location_Category = case_when(
    # Lokalizacje mieszkalne
    grepl("RESIDENCE|APARTMENT|CHA APARTMENT|HOUSE|NURSING HOME|RETIREMENT HOME|ROOMING HOUSE|DRIVEWAY - RESIDENTIAL|RESIDENCE-GARAGE|RESIDENCE - GARAGE|RESIDENCE PORCH|RESIDENCE - PORCH|RESIDENTIAL YARD|RESIDENCE - YARD", Location.Description, ignore.case = TRUE) ~ "Residential",
    
    # Sklepy, biura, komercja
    grepl("COMMERCIAL|BUSINESS OFFICE|RETAIL|STORE|DEPARTMENT STORE|SMALL RETAIL|GROCERY|CONVENIENCE STORE|DRUG STORE|PAWN SHOP|TAVERN/LIQUOR|LIQUOR STORE|NEWSSTAND|APPLIANCE STORE|CLEANING STORE|AUTO / BOAT / RV DEALERSHIP", Location.Description, ignore.case = TRUE) ~ "Commercial",
    
    # Instytucje finansowe
    grepl("BANK|CURRENCY EXCHANGE|ATM|SAVINGS AND LOAN|CREDIT UNION", Location.Description, ignore.case = TRUE) ~ "Financial",
    
    # Usługi
    grepl("OFFICE|MEDICAL/DENTAL|MEDICAL / DENTAL|ANIMAL HOSPITAL|BARBERSHOP|BARBER SHOP/BEAUTY SALON", Location.Description, ignore.case = TRUE) ~ "Office/Service",
    
    # Związane z taksówkami, samochodami
    grepl("VEHICLE|CAR WASH|AUTO|TAXICAB|RIDE SHARE|UBER|LYFT|DELIVERY TRUCK|GARAGE/AUTO REPAIR", Location.Description, ignore.case = TRUE) ~ "Vehicle",
    
    # Parkingi
    grepl("PARKING LOT|GARAGE \\(NON|DRIVEWAY", Location.Description, ignore.case = TRUE) ~ "Parking",
    
    # Drogi i chodniki publiczne
    grepl("STREET|HIGHWAY|EXPRESSWAY|SIDEWALK|ALLEY|BRIDGE", Location.Description, ignore.case = TRUE) ~ "Public Roads",
    
    # Stacja benzynowa
    grepl("GAS STATION", Location.Description, ignore.case = TRUE) ~ "Gas Station",
    
    # Parki i tereny zielone
    grepl("PARK PROPERTY|FOREST PRESERVE|WOODED AREA|LAKEFRONT|WATERFRONT|RIVERBANK|RIVER BANK|LAKE|LAGOON", Location.Description, ignore.case = TRUE) ~ "Outdoor Green Areas",
    
    # Komunikacja miejska
    grepl("CTA|TRAIN|BUS|SUBWAY|PLATFORM|OTHER RAILROAD", Location.Description, ignore.case = TRUE) ~ "Public Transit",
    
    # Lotnisko
    grepl("AIRPORT|AIRCRAFT", Location.Description, ignore.case = TRUE) ~ "Airport",
    
    # Statki
    grepl("BOAT|WATERCRAFT", Location.Description, ignore.case = TRUE) ~ "Water/Maritime",
    
    # Placówki oświatowe
    grepl("SCHOOL|COLLEGE|UNIVERSITY", Location.Description, ignore.case = TRUE) ~ "Educational Institutions",
    
    # Instytucje federalne
    grepl("GOVERNMENT|FEDERAL|POLICE|JAIL|LOCK-UP|FIRE STATION", Location.Description, ignore.case = TRUE) ~ "Government/Security",
    
    # Służba zdrowia
    grepl("HOSPITAL|NURSING|RETIREMENT HOME", Location.Description, ignore.case = TRUE) ~ "Healthcare",
    
    # Miejsca kultu
    grepl("CHURCH|SYNAGOGUE|PLACE OF WORSHIP|CEMETARY", Location.Description, ignore.case = TRUE) ~ "Religious",
    
    # Rozrywka i rekreacja
    grepl("RESTAURANT|BAR|TAVERN|CLUB|POOL ROOM|ATHLETIC|SPORTS|STADIUM|MOVIE|THEATER|BOWLING", Location.Description, ignore.case = TRUE) ~ "Indoor Entertainment/Sports",
    
    # Hotele
    grepl("HOTEL|MOTEL", Location.Description, ignore.case = TRUE) ~ "Accommodation",
    
    # Tereny przemysłowe / plac budowy
    grepl("WAREHOUSE|FACTORY|MANUFACTURING|CONSTRUCTION", Location.Description, ignore.case = TRUE) ~ "Industrial",
    
    # Wnętrza budynków
    grepl("HALLWAY|STAIRWELL|VESTIBULE|ELEVATOR|PORCH|BASEMENT|YARD", Location.Description, ignore.case = TRUE) ~ "Building Areas",
    
    # Inne
    grepl("OTHER|SPECIFY", Location.Description, ignore.case = TRUE) ~ "Other",
    
    # Nieznane
    Location.Description == "" ~ "Unknown",
    
    # Wszystko inne
    TRUE ~ "Miscellaneous"
  ))




data %>% 
  group_by(Location_Category) %>%
  count() %>% 
  ggplot(aes(y=reorder(Location_Category, n), x=n)) + 
  geom_col(fill="lightblue", color="darkblue") + 
  theme_bw() + 
  scale_x_continuous(breaks= seq(5e4, 3e5, 5e4), labels = function(x) {x/1000}) +
  labs(title="Liczba przestępstw według ich lokalizacji",
       subtitle="Lata 2016-2020",
       y = "Lokalizacja przestępstwa",
       x = "Liczba przestępstw (tys.)")


data %>%
  group_by(Location_Category, Type) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(Location_Category) %>%
  slice_max(order_by = count, n = 3)


num_crimes <- data %>% group_by(Type) %>% count() %>% arrange(desc(n))

num_crimes %>% 
  head(10) %>%
  ggplot(aes(y=reorder(Type, n), x=n)) + 
  geom_col(fill="lightblue", color="darkblue") + 
  theme_bw() + 
  scale_x_continuous(breaks= seq(5e4, 3e5, 5e4), labels = function(x) {x/1000}) +
  labs(title="Liczba przestępstw według ich rodzaju",
       subtitle="10 najczęściej popełnianych przestępstw",
       y = "Rodzaj przestępstwa",
       x = "Liczba przestępstw (tys.)")

num_crimes %>% 
  tail(20) %>%
  ggplot(aes(y=reorder(Type, n), x=n)) + 
  geom_col(fill="lightblue", color="darkblue") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 12000, 3000)) +
  labs(title="Liczba przestępstw według ich rodzaju",
       subtitle="20 najrzadziej popełnianych przestępstw",
       y = "Rodzaj przestępstwa",
       x = "Liczba przestępstw")

theft_types <- data %>%
  filter(Type == "THEFT") %>%
  group_by(Description) %>%
  count() %>%
  arrange(desc(n))
theft_types

sapply(names(data), function(col) {sum(is.na(data$col))})

data <- data %>% mutate(Date = as_datetime(Date, format = "%m/%d/%Y %I:%M:%S %p"))

crimes_by_hour <- data %>%
  mutate(hour = hour(Date)) %>%
  group_by(hour) %>%
  summarize(num_crimes = n())



ggplot(crimes_by_hour, aes(x=hour, y=num_crimes)) + 
  geom_smooth(method="loess", formula= y~x, alpha=0.25, color="darkgray") + 
  geom_point(color = "blue") + 
  scale_y_continuous(breaks = seq(0, 8e4, 2e4), labels = function(x) x/1000) + 
  expand_limits(y=0) + 
  scale_x_continuous(breaks = seq(0, 23, 2), labels = function(x) paste(x, "00", sep=":")) + 
  labs(title = "Liczba przestępstw w ciągu dnia",
       subtitle = "Lata 2016-2020",
       x = "Godzina",
       y = "Liczba przestępstw (tys.)") + 
  theme_bw()


crime_types_by_hour %>%
  filter(Type %in% c("PROSTITUTION", 
                     "ARSON", "PUBLIC PEACE VIOLATION", "INTERFERENCE WITH PUBLIC OFFICER", "SEX OFFENSE")) %>%
  ggplot(aes(x=hour, y=n, color= Type)) + 
  geom_point() + 
  geom_line(linewidth=1.1, alpha=0.7) + 
  scale_y_continuous(limits = c(0,1000)) + 
  expand_limits(y=0) + 
  scale_x_continuous(breaks = seq(0, 23, 2), labels = function(x) paste(x, "00", sep=":")) + 
  labs(title = "Liczba przestępstw w ciągu dnia",
       subtitle = "Lata 2016-2020",
       x = "Godzina",
       y = "Liczba przestępstw",
       color = "Rodzaj przestępstwa") + 
  theme_bw() + 
  theme(legend.position = c(0.6, 0.75),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))  

crime_locations_by_hour <- data %>%
  mutate(hour = hour(Date)) %>%
  group_by(hour, Location_Category) %>%
  count()

crime_locations_by_hour %>%
  filter(Location_Category%in% c("Residential", "Commercial", "Public Roads")) %>%
  ggplot(aes(x=hour, y=n, color= Location_Category)) + 
  geom_smooth(method="loess", formula= y~x, alpha=0.25, se=F) + 
  geom_point() + 
  scale_y_continuous(breaks = seq(0, 2e4, 5e3), labels = function(x) x/1000) + 
  expand_limits(y=0) + 
  scale_x_continuous(breaks = seq(0, 23, 2), labels = function(x) paste(x, "00", sep=":")) + 
  labs(title = "Liczba przestępstw w ciągu dnia",
       subtitle = "Lata 2016-2020",
       x = "Godzina",
       y = "Liczba przestępstw (tys.)",
       color = "Lokalizacja przestępstwa") + 
  theme_bw()

crime_locations_by_hour %>%
  filter(Location_Category%in% c("Educational Institutions", "Public Transit", "Indoor Entertainment/Sports")) %>%
  ggplot(aes(x=hour, y=n, color= Location_Category)) + 
  geom_line(linewidth=1.1, alpha=0.5) + 
  geom_point() + 
  expand_limits(y=0) + 
  scale_x_continuous(breaks = seq(0, 23, 2), labels = function(x) paste(x, "00", sep=":")) + 
  labs(title = "Liczba przestępstw w ciągu dnia",
       subtitle = "Lata 2016-2020",
       x = "Godzina",
       y = "Liczba przestępstw",
       color = "Lokalizacja przestępstwa") + 
  theme_bw()

# liczba przestepstw w zaleznosci od dnia tygodnia

data$Weekday <- factor(weekdays(data$Date), 
                       levels= c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))

crimes_by_weekday <- data %>%
  group_by(Weekday, Type) %>%
  summarize(num_crimes = n(), .groups="drop")

crimes_by_weekday %>%
  filter(Type %in% c("THEFT", "BATTERY", "CRIMINAL DAMAGE", "DECEPTIVE PRACTICE")) %>%
  ggplot(aes(x=Weekday, y=num_crimes, color=Type, group=Type)) + 
  geom_point() + 
  geom_line(size=1.1, alpha=0.5) + 
  scale_y_continuous(breaks = seq(0, 4e4, 1e4), labels = function(x) x/1000) + 
  expand_limits(y=0) + 
  labs(title = "Liczba przestępstw w zależności od dnia tygodnia",
       subtitle = "Lata 2016-2020",
       x = "Dzień tygodnia",
       y = "Liczba przestępstw (tys.)",
       color = "Rodzaj przestępstwa") + 
  theme_bw()

crimes_by_weekday %>%
  filter(Type %in% c("PROSTITUTION", 
                     "ARSON", "PUBLIC PEACE VIOLATION",
                     "INTERFERENCE WITH PUBLIC OFFICER",
                     "OFFENSE INVOLVING CHILDREN")) %>%
  ggplot(aes(x=Weekday, y=num_crimes, color= Type, group=Type)) + 
  scale_x_discrete(labels = c("pon", "wt", "śr", "czw", "pt", "sob", "nd")) +
  geom_point() + 
  geom_line(size=1.1, alpha=0.7) + 
  expand_limits(y=0) + 
  labs(title = "Liczba przestępstw w zależności od dnia tygodnia",
       subtitle = "Lata 2016-2020",
       x = "Dzień tygodnia",
       y = "Liczba przestępstw",
       color = "Rodzaj przestępstwa") + 
  theme_bw()



# przestępstwa pogrupowane na miesiące kolejnych lat
crimes_by_month_year <- data %>%
  mutate(Month = my(paste(month(Date), year(Date), sep="-"))) %>%
  group_by(Month, Type) %>%
  summarize(num_crimes = n(), .groups = "drop")

# przestępstwa pogrupowane wg numeru tygodnia (bez roku)
crimes_by_week <- data %>%
  mutate(Week = week(Date)) %>%
  group_by(Week,  Type) %>%
  summarize(num_crimes = n(), .groups = "drop", )

# przestępstwa pogrupowane wg daty
crimes_by_date <- data %>%
  mutate(Date = date(Date)) %>%
  group_by(Date, Type) %>%
  summarize(num_crimes = n(), .groups = "drop") 

# przestępstwa pogrupowane wg numeru tygodnia i roku - domyślnie z datą odpowiadającą poniedziałkowi danego tyg.
crimes_by_week_year <- data %>%
  mutate(
    week = isoweek(Date),
    year = isoyear(Date)
  ) %>%
  group_by(week, year, Type) %>%
  summarize(num_crimes = n(), .groups = "drop") %>%
  mutate(week_year = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%W-%u")) %>% arrange(week_year)

# przestępstwa pogrupowane wg daty i godziny
crimes_by_date_hour <- data %>%
  mutate(date_hour = ymd_h(paste(date(Date), hour(Date), sep=", "))) %>%
  group_by(date_hour, Type) %>%
  summarize(num_crimes = n(), .groups = "drop")


crimes_by_week_year %>%  
  filter(Type %in% c("THEFT", "BATTERY", "CRIMINAL DAMAGE", "DECEPTIVE PRACTICE")) %>%
  ggplot(aes(x=week_year, y=num_crimes, color=Type)) + 
  geom_line(size=0.75, alpha=0.75) + 
  scale_x_date(breaks = as.Date("2016-01-01") %m+% months(seq(0, 60, 6)), 
               labels = function(x) format(x, "%Y-%m") ) +
  labs(title = "Tygodniowa liczba przestępstw danego typu w czasie",
       x = "Data", 
       y = "Liczba przestępstw",
       color = "Rodzaj przestępstwa") + 
  theme_bw() + 
  theme(legend.position = "bottom")

crimes_by_date %>%  
  filter(Type %in% c("ROBBERY", "MOTOR VEHICLE THEFT", 
                     "ASSAULT", "NARCOTICS", "BURGLARY", 
                     "CRIMINAL DAMAGE")) %>%
  ggplot(aes(x=Date, y=num_crimes, color=Type)) + 
  geom_line(size=0.75, alpha=0.75) + 
  scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2021-01-01"), by="12 months"),
               labels = function(x) format(x, "%Y")) +
  facet_wrap(~Type, scale="free", ncol=3) + 
  labs(title = "Dzienna liczba przestępstw danego typu w czasie",
       x = "Data", 
       y = "Liczba przestępstw",
       color = "Rodzaj przestępstwa") + 
  theme_bw() + 
  theme(legend.position = "bottom")

