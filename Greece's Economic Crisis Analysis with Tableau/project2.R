library(parallel)
library(data.table)
library(readxl)
library(xlsx)
library(tidyverse)
library(stringr)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.7.0_51\\jre1.8.0_211')


#suicides
suicides=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/healthcare expenses.xls", 
                     sheet = "Suicides, deaths per 100000 pop", col_names=FALSE, skip=2))

suicides=data.frame("year"=as.character(t(suicides[1,])), "suicides"=as.numeric(t(suicides %>% filter(suicides[,1]=="Greece"))))[42:58,]

#healthcare
healthcare=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/healthcare expenses.xls", 
                              sheet = "Current expenditure, % GDP", col_names=FALSE, skip=2))

healthcare=data.frame("year"=as.character(t(healthcare[1,])), "healthcare expenses"=as.numeric(t(healthcare %>% filter(healthcare[,1]=="Greece"))))[32:48,]

#pharmacy
pharmacy=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/healthcare expenses.xls", 
                             sheet = "Pharmaceutical exp, % curr exp", col_names=FALSE, skip=2))

pharmacy=data.frame("year"=as.character(t(pharmacy[1,])), "Pharmaceutical expenses"=as.numeric(t(pharmacy %>% filter(pharmacy[,1]=="Greece"))))[32:48,]

#alcohol consumption
infant=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/healthcare expenses.xls", 
                             sheet = "Infant mortality rate", col_names=FALSE, skip=2))

infant=data.frame("year"=as.character(t(infant[1,])), "infant mortality"=as.numeric(t(infant %>% filter(infant[,1]=="Greece"))))[42:58,]

#alcohol consumption
alcohol=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/healthcare expenses.xls", 
                             sheet = "Alcohol consumption", col_names=FALSE, skip=2))

alcohol=data.frame("year"=as.character(t(alcohol[1,])), "alcohol consumption"=as.numeric(t(alcohol %>% filter(alcohol[,1]=="Greece"))))[42:58,]


#gdp
gdp=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/gdp.xls", col_names=FALSE, skip=2))

gdp=data.frame("year"=as.character(t(gdp[1,])), "gdp"=as.numeric(t(gdp %>% filter(gdp[,1]=="Greece"))))[45:61,]

#unemployment
unemployment=as_tibble(fread("D:/Data Science/Data Visualization/Exercises/unemployment.csv"))

unemployment=data.frame(unemployment %>% 
                        filter(unemployment$LOCATION=="GRC", unemployment$TIME>=2000, 
                        unemployment$TIME<=2016, nchar(unemployment$TIME)<=4) %>% 
                        select(3,6,7))

unemployment=data.frame(unique(unemployment$TIME), 
                        unemployment %>% filter(unemployment$SUBJECT=="MEN") %>% select(3),
                        unemployment %>% filter(unemployment$SUBJECT=="WOMEN") %>% select(3),
                        unemployment %>% filter(unemployment$SUBJECT=="TOT") %>% select(3))

colnames(unemployment) = c("year", "unemployment men", "unemployment women", "unemployment total")

#happiness
happiness=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/happiness.xlsx",
                               sheet = "Data behind Table 2.1 WHR 2017", col_names=TRUE))

happiness=data.frame(happiness %>% filter(country=="Greece", year>=2009) %>% select(3,4,6,7,8,9,10))

#demographic
demographic=as_tibble(read_excel("D:/Data Science/Data Visualization/Exercises/demographics.xlsx", col_names=TRUE))

demographic.genderize = function(demographic, gender){
  demographic=data.frame(demographic %>% filter(GEO=="Greece", TIME<=2016, SEX==gender) %>% select(1,2,3,5))
  demographic$AGE=as.numeric(str_split_fixed(demographic$AGE, " ", 2)[,1])
  demographic$AGE[is.na(demographic$AGE)] = 0
  demographic$Value=as.numeric(demographic$Value)
  demographic$AGE=apply(demographic[,1,drop=F], 1, function(x) if(x==0) "newborn" else if(x>=1 & x<20) "1-19" else if(x>=20 & x<40) "20-39" else if(x>=40 & x<60) "40-59" else if(x>=60 & x<80) "60-79" else "80+")
  
  demographic=aggregate(as.numeric(demographic$Value), by=list("AGE"=demographic$AGE, "YEAR"=demographic$TIME), mean)
  
  demographic=data.frame(unique(demographic$YEAR), 
                         demographic %>% filter(demographic$AGE=="newborn") %>% select(3),
                         demographic %>% filter(demographic$AGE=="1-19") %>% select(3),
                         demographic %>% filter(demographic$AGE=="20-39") %>% select(3),
                         demographic %>% filter(demographic$AGE=="40-59") %>% select(3),
                         demographic %>% filter(demographic$AGE=="60-79") %>% select(3),
                         demographic %>% filter(demographic$AGE=="80+") %>% select(3))
  
  colnames(demographic) = c("year", paste("newborn", gender, " "), paste("1-19", gender, " "), paste("20-39", gender, " "), paste("40-59", gender, " "), paste("60-79", gender, " "), paste("80+", gender, " "))
  return(demographic)
}

demographicm = demographic.genderize(demographic, "Males")
demographicf = demographic.genderize(demographic, "Females")
demographict = demographic.genderize(demographic, "Total")

total=NULL
total=merge(demographicm, demographicf, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, demographict, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, gdp, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, unemployment, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, happiness, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, suicides, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, alcohol, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, healthcare, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, pharmacy, by.x="year", by.y="year", all.x=TRUE)
total=merge(total, infant, by.x="year", by.y="year", all.x=TRUE)

fwrite(total, "D:/Data Science/Data Visualization/Exercises/total_crisis.csv")

total=as_tibble(fread("D:/Data Science/Data Visualization/Exercises/total_crisis.csv"))

demo.2011=demographic %>% filter(AGE =="Less than 1 year", TIME==2011, SEX=="Total") %>% select(4,5) %>% filter(Value!=":")
demo.2011$Value=as.numeric(demo.2011$Value)
demo.2011=demo.2011[order(demo.2011$Value),c(1,2)]
demo.2011$GEO <- factor(demo.2011$GEO, levels = demo.2011$GEO)
demo.2012=demographic %>% filter(AGE =="Less than 1 year", TIME==2012, SEX=="Total") %>% select(4,5) %>% filter(Value!=":")
demo.2012$Value=as.numeric(demo.2012$Value)
demo.2012=demo.2012[order(demo.2012$Value),c(1,2)]
demo.2012$GEO <- factor(demo.2012$GEO, levels = demo.2012$GEO)

require(maps)


life.exp <- demo.2011 %>%
  rename(region = GEO, lifeExp = Value)
eu.maps <- map_data("world", region = demo.2011$GEO)
life.exp.map <- left_join(life.exp, eu.maps, by = "region")
ggplot(life.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = lifeExp ), color = "white")+
  scale_fill_viridis_c(option = "C")



ggplot(demo.2011, aes(x=GEO, y=Value, fill=factor(ifelse(GEO=="Greece","Greece","Other")))) +
  theme(legend.title = element_blank()) +
  ggtitle("European life expectancy for someone born in 2012") +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Countries") +
  ylab("Life Expectancy")



life.exp <- demo.2012 %>%
  rename(region = GEO, lifeExp = Value)
eu.maps <- map_data("world", region = demo.2012$GEO)
life.exp.map <- left_join(life.exp, eu.maps, by = "region")
ggplot(life.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = lifeExp ), color = "white")+
  scale_fill_viridis_c(option = "C")

ggplot(demo.2012, aes(x=GEO, y=Value, fill=factor(ifelse(GEO=="Greece","Greece","Other")))) +
  theme(legend.title = element_blank()) +
  ggtitle("European life expectancy for someone born in 2012") +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Countries") +
  ylab("Life Expectancy")






