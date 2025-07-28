theft_n_trespass <- dane %>% filter(Primary.Type %in% c("THEFT", "CRIMINAL TRESPASS"))%>% 
  select(-c(X,Block,Case.Number, IUCR, Description, FBI.Code, Location.Description, X.Coordinate,Y.Coordinate,Location))%>%
  mutate(Primary.Type = factor(Primary.Type),
         Arrest = factor(Arrest))

theft_n_trespass <- theft_n_trespass %>%
  mutate(
    Hour = as.integer(substr(Date, 12, 13)),
    part_of_day = case_when(
      Hour >= 0  & Hour <= 5  ~ "noc",
      Hour >= 6  & Hour <= 11 ~ "przedpoludnie",
      Hour >= 12 & Hour <= 23 ~ "popoludnie"
    ))

model_dane <- theft_n_trespass %>%
  mutate(
    arrest_bin = if_else(Arrest == "True", 1, 0),
    theft_vs_trespass = factor(Primary.Type,
                               levels = c("THEFT", "CRIMINAL TRESPASS")),
    domestic = as.factor(if_else(Domestic == "True", 1, 0)),
    district = as.factor(District),
    year = as.factor(Year),
    part_of_day = as.factor(part_of_day)
  ) %>%
  select(ID,Primary.Type,arrest_bin, theft_vs_trespass, domestic, district, year, part_of_day)


model <- glm(
  arrest_bin ~ theft_vs_trespass + domestic + district + year+part_of_day,
  data = model_dane,
  family = binomial()
)

summary(model)

exp(coef(model))


model_dane %>% filter(district==31)

model_dane <- model_dane  %>% filter(district!=31) 


dane_train <- model_dane %>% slice_sample(prop=0.7)
dane_test <- model_dane %>% anti_join(dane_train, by="ID")


set.seed(123)

dane_train <- ovun.sample(
  Primary.Type~.,
  data = dane_train,
  method = "over",
  N=100000+sum(dane_train$Primary.Type == "THEFT")
)$data
table(dane_train$Primary.Type)

model <- glm(
  arrest_bin ~ theft_vs_trespass + domestic + district + year+part_of_day,
  data = dane_train,
  family = binomial(link="logit")
)

p_test <- predict(model, dane_test, type="response")

p_val <- ifelse(p_test > 0.5, 1, 0)


mean(p_val == dane_test$arrest_bin)

ct<-table(real=dane_test$arrest_bin,pred=p_val)
ct

roc_theft_n_trespass <- roc(dane_test$arrest_bin, p_test)
plot(roc_theft_n_trespass)

auc(roc_theft_n_trespass)


#dokladnosc 
acc<- (ct[1,1]+ct[2,2])/sum(ct)
acc

#czulosc
sens <- ct[2,2]/(ct[2,1]+ct[2,2])
sens

#swoistosc

spe <- ct[1,1]/(ct[1,1]+ct[1,2])
spe
#precyzja
pre <- ct[2,2]/(ct[2,2]+ct[1,2])
pre


coords(roc_theft_n_trespass, "best", ret = "threshold")

p_val_opt <- ifelse(p_test > 0.14, 1, 0)
ct_opt <- table(real = dane_test$arrest_bin, pred = p_val_opt)
ct_opt


#dokladnosc 
acc<- (ct_opt[1,1]+ct_opt[2,2])/sum(ct_opt)
acc

#czulosc
sens <- ct_opt[2,2]/(ct_opt[2,1]+ct_opt[2,2])
sens

#swoistosc

spe <- ct_opt[1,1]/(ct_opt[1,1]+ct_opt[1,2])
spe
#precyzja
pre <- ct_opt[2,2]/(ct_opt[2,2]+ct_opt[1,2])
pre

f1 <- 2 * (pre * sens) / (pre + sens)
f1






