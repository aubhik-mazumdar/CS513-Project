Olympics_summer <- read.csv("https://raw.githubusercontent.com/aubhik-mazumdar/CS513-Project/master/datasets/summer_new.csv")

View(table(Olympics_summer$Country))



Olympics_summer_country<-Olympics_summer$Country
Olympics_summer_country<-as.character(Olympics_summer_country)
Olympics_summer_country[Olympics_summer_country == "YUG"]<-"SRB"

Olympics_summer_country[Olympics_summer_country == "URS"]<-"RUS"
Olympics_summer_country[Olympics_summer_country == "TCH"]<-"CZE"
Olympics_summer_country[Olympics_summer_country == "SCG"]<-"SRB"
Olympics_summer_country[Olympics_summer_country == "IOP"]<-"SRB"
Olympics_summer_country[Olympics_summer_country == "GDR"]<-"GER"
Olympics_summer_country[Olympics_summer_country == "FRG"]<-"GER"
Olympics_summer_country[Olympics_summer_country == "EUN"]<-"RUS"
Olympics_summer_country[Olympics_summer_country == "AHO"]<-"NED"

Olympics_summer_country<-as.factor(Olympics_summer_country)
Olympics_summer_countryreplaced<-Olympics_summer
Olympics_summer_countryreplaced$Country<-Olympics_summer_country

View(table(Olympics_summer_countryreplaced$Country))






