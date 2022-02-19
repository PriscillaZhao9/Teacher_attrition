library(here)
library(readxl)
library(tidyverse)
data_2014 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2014-15 Professional Personnel Individual Staff Report.xlsx")
data_2015 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2015-16 Professional Personnel Individual Staff Report.xlsx")
data_2016 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2016-17 Professional Personnel Individual Staff Report.xlsx")
data_2017 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2017-18 Professional Personnel Individual Staff Report.xlsx")

data_2018 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2018-19 Professional Personnel Individual Staff Report.xlsx") %>% rename(YearsInEd = YearsInED)
data_2019 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2019-20 Professional Personnel Individual Staff Report.xlsx") %>% rename(YearsInEd = YearsInED)

data_2020 <- read_excel("/Users/priscilla/Desktop/teacher_shortage/2020-21 Professional Personnel Individual Staff Report.xlsx") %>% 
  mutate(PublicID = as.numeric(PublicID)) %>% 
  rename(YearsInEd = YearsInED) %>%
  mutate(AnnualSalary = ifelse(AnnualSalary == "Salary Excluded for Fictitious", NA, AnnualSalary))

select_var <- function(data) {
  
  data <- data %>%
    dplyr::select(SY, PublicID, CategoryDescription)
  
}
data_list <- list(data_2014, data_2015, data_2016, data_2017, data_2018, data_2019, data_2020)
temp <- lapply(data_list, function(x){x <- x %>% dplyr::select(SY, PublicID, CategoryDescription); return (x)})
names(temp) <- c("data_2014_short", "data_2015_short", "data_2016_short", "data_2017_short", "data_2018_short","data_2019_short", "data_2020_short")
list2env(temp, envir = .GlobalEnv)
head(data_2014_short$CategoryDescription)


data_2014_short$num=ifelse(data_2014_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2015_short$num=ifelse(data_2015_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2016_short$num=ifelse(data_2016_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2017_short$num=ifelse(data_2017_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2018_short$num=ifelse(data_2018_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2019_short$num=ifelse(data_2019_short$CategoryDescription=="Classroom Teachers" ,1,0)
data_2020_short$num=ifelse(data_2020_short$CategoryDescription=="Classroom Teachers" ,1,0)

full_short <- data_2014_short %>%
  full_join(data_2015_short, by = "PublicID") %>%
  full_join(data_2016_short, by = "PublicID") %>%
  full_join(data_2017_short, by = "PublicID") %>%
  full_join(data_2018_short, by = "PublicID") %>%
  full_join(data_2019_short, by = "PublicID") %>%
  full_join(data_2020_short, by = "PublicID")

datal<-list(data_2014_short, data_2015_short, data_2016_short, data_2017_short, data_2018_short, data_2019_short, data_2020_short)


mean14<-data_2014_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean15<-data_2015_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean16<-data_2016_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean17<-data_2017_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean18<-data_2018_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean19<-data_2019_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))
mean20<-data_2020_short%>%
  group_by(PublicID) %>%
  summarise_at(vars(num), list(name = mean))

meantotal <-mean14 %>%
  full_join(mean15, by = "PublicID") %>%
  full_join(mean16, by = "PublicID") %>%
  full_join(mean17, by = "PublicID") %>%
  full_join(mean18, by = "PublicID") %>%
  full_join(mean19, by = "PublicID") %>%
  full_join(mean20, by = "PublicID")

meantotal <- meantotal%>%
  rename(y14 = name.x,
         y15 = name.y,
         y16 = name.x.x,
         y17 = name.y.y,
         y18 = name.x.x.x,
         y19 = name.y.y.y,
         y20 = name)
yearlist<-list(meantotal$y14,meantotal$y15, meantotal$y16, meantotal$y17, meantotal$y18, meantotal$y19, meantotal$y20)

meantotal<- meantotal%>%mutate(
  y14=case_when(y14 > 0 ~ 1,y14 == 0 ~ 0),
  y15=case_when(y15 > 0 ~ 1,y15 == 0 ~ 0),
  y16=case_when(y16 > 0 ~ 1,y16 == 0 ~ 0),
  y17=case_when(y17 > 0 ~ 1,y17 == 0 ~ 0),
  y18=case_when(y18 > 0 ~ 1,y18 == 0 ~ 0),
  y19=case_when(y19 > 0 ~ 1,y19 == 0 ~ 0),
  y20=case_when(y20 > 0 ~ 1,y20 == 0 ~ 0)
)
exit_list<-data.frame(2015, 2016, 2017, 2018, 2019, 2020)
exit_rate<- data.frame(rate15,rate16,rate17,rate18,rate19,rate20)
rate15<-sum(meantotal$y14 == 1 & meantotal$y15 == 0, na.rm = TRUE)/(sum(meantotal$y14 == 1 & meantotal$y15 == 0, na.rm = TRUE)+sum(meantotal$y14 == 1 & meantotal$y15 == 1, na.rm = TRUE))
rate16<-sum(meantotal$y15 == 1 & meantotal$y16 == 0, na.rm = TRUE)/(sum(meantotal$y15 == 1 & meantotal$y16 == 0, na.rm = TRUE)+sum(meantotal$y15 == 1 & meantotal$y16 == 1, na.rm = TRUE))
rate17<-sum(meantotal$y16 == 1 & meantotal$y17 == 0, na.rm = TRUE)/(sum(meantotal$y16 == 1 & meantotal$y17 == 0, na.rm = TRUE)+sum(meantotal$y16 == 1 & meantotal$y17 == 1, na.rm = TRUE))
rate18<-sum(meantotal$y17 == 1 & meantotal$y18 == 0, na.rm = TRUE)/(sum(meantotal$y17 == 1 & meantotal$y18 == 0, na.rm = TRUE)+sum(meantotal$y17 == 1 & meantotal$y18 == 1, na.rm = TRUE))
rate19<-sum(meantotal$y18 == 1 & meantotal$y19 == 0, na.rm = TRUE)/(sum(meantotal$y18 == 1 & meantotal$y19 == 0, na.rm = TRUE)+sum(meantotal$y18 == 1 & meantotal$y19 == 1, na.rm = TRUE))
rate20<-sum(meantotal$y19 == 1 & meantotal$y20 == 0, na.rm = TRUE)/(sum(meantotal$y19 == 1 & meantotal$y20 == 0, na.rm = TRUE)+sum(meantotal$y19 == 1 & meantotal$y20 == 1, na.rm = TRUE))
Years<-data.frame(2015,2016,2017,2018,2019,2020)

ggplot(data=exit_rate, aes(x = exit_list, y = exit_rate)) +
  geom_point()