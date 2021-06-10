library(ggplot2)
library(dplyr)
library(corrplot)
library(leaflet)
library(ggpubr)
library(plotly)
library(tidyr)
library(tidyverse)
library(maps)
library(mapproj)
library(stringr)

#Lettura datasets

happiness = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/HappinesWorldReport.csv")

View(happiness)

alcoholConsumption = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/alccons/HappinessAlcoholConsumption.csv")

View(alcoholConsumption)

unemployment = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/unemployment-rate.csv")

View(unemployment)

#Modifica e creazione datasets

colnames(unemployment)[4] = "UnemploymentRate"

new_unemployment = unemployment %>%
  filter(Year == "2017") %>%
  select(Entity,UnemploymentRate)

View(new_unemployment)

happinessUnemp = left_join(happiness, new_unemployment, by = c("Country.or.region" = "Entity"))

View(happinessUnemp)

new_alcohol = alcoholConsumption %>%
  select(Country,Region,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

new_dataset = inner_join(happinessUnemp,new_alcohol,by=c("Country.or.region" = "Country"))
View(new_dataset)

new_dataset = new_dataset[c(1,2,11,3,4,5,6,7,8,9,10,12,13,14)]
View(new_dataset)

new_dataset = na.omit(new_dataset)
View(new_dataset)

##### Top 10 Paesi più felici #####

top10 = head(new_dataset,10) %>%
  select(Country.or.region,Region,Score)

top10

##### Mappa Top 10 Paesi più felici #####

worldtb = map_data("world") %>%
  as_tibble()

top10States = left_join(worldtb,top10,by=c("region" = "Country.or.region"))
View(top10States)

top10States %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = top10States,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Score), color="black") +
  scale_fill_gradient2(high="yellow") +
  theme_minimal()+
  labs(
    title = "Top 10 Stati più felici", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#E6AF2E"),
    legend.position = "right"
  ) 

#######################

n_regions = new_dataset %>% 
  group_by(Region) %>%
  summarise(count = n())

View(n_regions)

regions = as.vector(n_regions$Region)
regions

media_happ = vector(mode="character", length=9)
media_gdp = vector(mode="character", length=9)
media_supp = vector(mode="character", length=9)
media_corrup = vector(mode="character", length=9)
media_unemp = vector(mode="character", length=9)
media_wine = vector(mode="character", length=9)
media_beer = vector(mode="character", length=9)
media_spirit = vector(mode="character", length=9)

for(i in 1:9){
  
  gg = new_dataset %>%
    filter(Region == regions[i])
  
  media_happ[i] = mean(gg$Score)
  
  media_gdp[i] = mean(gg$GDP.per.capita)
  
  media_supp[i] = mean(gg$Social.support)
  
  media_corrup[i] = mean(gg$Perceptions.of.corruption)
  
  media_unemp[i] = mean(gg$UnemploymentRate)
  
  media_wine[i] = mean(gg$Wine_PerCapita)
  
  media_beer[i] = mean(gg$Beer_PerCapita)
  
  media_spirit[i] = mean(gg$Spirit_PerCapita)
}

media_happ
media_gdp
media_supp
media_corrup
media_unemp
media_wine
media_beer
media_spirit

media_happ = as.double(media_happ)
media_happ

media_happ = signif(media_happ,3)
media_happ

media_gdp = as.double(media_gdp)
media_gdp

media_gdp = signif(media_gdp,3)
media_gdp

media_supp = as.double(media_supp)
media_supp

media_supp = signif(media_supp,3)
media_supp

media_corrup = as.double(media_corrup)
media_corrup

media_corrup = signif(media_corrup,3)
media_corrup

media_unemp = as.double(media_unemp)
media_unemp

media_unemp = signif(media_unemp,3)
media_unemp

media_wine = as.double(media_wine)
media_wine

media_wine = signif(media_wine,3)
media_wine

media_beer = as.double(media_beer)
media_beer

media_beer = signif(media_beer,3)
media_beer

media_spirit = as.double(media_spirit)
media_spirit

media_spirit = signif(media_spirit,3)
media_spirit

new_data = data.frame(regions,media_happ,media_gdp,media_supp,
                      media_corrup,media_unemp,media_wine,
                      media_beer,media_spirit)

View(new_data)

g1 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_happ), stat="identity", fill="#FFFB1F") +
  ggtitle("Average happiness by region") +
  xlab("Regions") +
  ylab("Average Happiness") +
  coord_flip()          

g2 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_gdp), stat="identity", fill="#4CA8BD") +
  ggtitle("Average GDP by region") +
  xlab("Regions") +
  ylab("Average GDP") +
  coord_flip() 

g3 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_supp), stat="identity", fill="#F97924") +
  ggtitle("Average Social Support by region") +
  xlab("Regions") +
  ylab("Average Social Support") +
  coord_flip() 

g4 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_corrup), stat="identity", fill="#5E277C") +
  ggtitle("Average Corruption by region") +
  xlab("Regions") +
  ylab("Average Corruption") +
  coord_flip() 

g5 = new_data %>%
   ggplot() +
   geom_bar(aes(x=regions, y=media_unemp), stat="identity", fill="dimgray") +
   ggtitle("Average Unemployment by region") +
   xlab("Regions") + 
   ylab("Average Unemployment") +
   coord_flip()

g6 = new_data %>%
   ggplot() +
   geom_bar(aes(x=regions, y=media_wine), stat="identity", fill="darkred") +
   ggtitle("Average Wine consumption by region") +
   xlab("Regions") +
   ylab("Average Wine") +
   coord_flip()

g7 = new_data %>%
   ggplot() +
   geom_bar(aes(x=regions, y=media_beer), stat="identity", fill="gold") +
   ggtitle("Average Beer consumption by region") +
   xlab("Regions") +
   ylab("Average Beer") +
   coord_flip()

g8 = new_data %>%
   ggplot() +
   geom_bar(aes(x=regions, y=media_spirit), stat="identity", fill="deepskyblue") +
   ggtitle("Average Spirit consumption by region") +
   xlab("Regions") +
   ylab("Average Spirit") +
   coord_flip()


ggarrange(g1, g2, g3, g4,g5,g6,g7,g8,
          ncol = 2, nrow = 4)

ggarrange(g1, g2, g3, g4,
          ncol = 2, nrow = 2)

########## correlazione tra dati medi di tutte le regioni #####################

x = matrix(c(new_data[,2],new_data[,3],new_data[,4],new_data[,5],new_data[,6],new_data[,7],new_data[,8],new_data[,9]),nrow = 9,ncol=8)

dim(x) = c(9,8)
colnames(x) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment","Average Beer",
                "Average Wine","Average Spirit")
x

M = cor(x)

corrplot(M, is.corr = FALSE, method = "ellipse",bg="#A5AC20")

corrplot(M, is.corr = FALSE, method = "number",bg="#A5AC20")

########## correlazione tra dati medi per singola regione #####################

#Oceania and Asia (correlazione maggiore felicità -> Average Wine Consumption)

ANZ_tab = new_dataset %>%
  filter(Region == "Australia and New Zealand" | Region == "Eastern Asia" | Region == "Southeastern Asia") %>%
  select(Score,GDP.per.capita,Social.support,Perceptions.of.corruption,UnemploymentRate,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

View(ANZ_tab)

mat = matrix(c(ANZ_tab[,1],ANZ_tab[,2],ANZ_tab[,3],ANZ_tab[,4],ANZ_tab[,5],ANZ_tab[,6],ANZ_tab[,7],ANZ_tab[,8]),nrow=11,ncol=8)

dim(mat) = c(11,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M1 = cor(mat)
pairs(mat)

corrplot(M1, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M1, is.corr = FALSE, method = "number",bg="#72c475")

#Western Europe (correlazione maggiore felicità -> Average Corruption diretta)
 
EU_tab = new_dataset %>%
  filter(Region == "Western Europe") %>%
  select(Score,GDP.per.capita,Social.support,Perceptions.of.corruption,UnemploymentRate,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

View(EU_tab)

mat = matrix(c(EU_tab[,1],EU_tab[,2],EU_tab[,3],EU_tab[,4],EU_tab[,5],EU_tab[,6],EU_tab[,7],EU_tab[,8]),nrow=20,ncol=8)

dim(mat) = c(20,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M2 = cor(mat)
pairs(mat)

corrplot(M2, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M2, is.corr = FALSE, method = "number",bg="#72c475")

#Central and Eastern Europe (correlazione maggiore felicità -> Average Support)

CEE_tab = new_dataset %>%
  filter(Region == "Central and Eastern Europe")

View(CEE_tab)

mat = matrix(c(CEE_tab[,4],CEE_tab[,5],CEE_tab[,6],CEE_tab[,10],CEE_tab[,11],CEE_tab[,12],CEE_tab[,13],CEE_tab[,14]),nrow=24,ncol=8)

dim(mat) = c(24,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M3 = cor(mat)
pairs(mat)

corrplot(M3, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M3, is.corr = FALSE, method = "number",bg="#72c475")

#Sub-Saharan Africa (correlazione maggiore felicità -> Average Corruption inversa)

SSA_tab = new_dataset %>%
  filter(Region == "Sub-Saharan Africa")%>%
  select(Score,GDP.per.capita,Social.support,Perceptions.of.corruption,UnemploymentRate,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

View(SSA_tab)

mat = matrix(c(SSA_tab[,1],SSA_tab[,2],SSA_tab[,3],SSA_tab[,4],SSA_tab[,5],SSA_tab[,6],SSA_tab[,7],SSA_tab[,8]),nrow=24,ncol=8)

dim(mat) = c(24,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M4 = cor(mat)
pairs(mat)

corrplot(M4, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M4, is.corr = FALSE, method = "number",bg="white")

#Middle East and Northern Africa (correlazione maggiore felicità -> Average Unemployment)

MAF_tab = new_dataset %>%
  filter(Region == "Middle East and Northern Africa")%>%
  select(Score,GDP.per.capita,Social.support,Perceptions.of.corruption,UnemploymentRate,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

View(MAF_tab)

matMAF = matrix(c(MAF_tab[,1],MAF_tab[,2],MAF_tab[,3],MAF_tab[,4],MAF_tab[,5],MAF_tab[,6],MAF_tab[,7],MAF_tab[,8]),nrow=10,ncol=8)

dim(matMAF) = c(10,8)
colnames(matMAF) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment","Average Beer","Average Wine","Average Spirit")
matMAF

View(matMAF)
M5 = cor(matMAF)
pairs(matMAF)

corrplot(M5, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M5, is.corr = FALSE, method = "number",bg="#72c475")

#Americas (correlazione maggiore felicità -> Average GDP)

LAM_tab = new_dataset %>%
  filter(Region == "Latin America and Caribbean" | Region == "North America")%>%
  select(Score,GDP.per.capita,Social.support,Perceptions.of.corruption,UnemploymentRate,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

View(LAM_tab)

matLAM = matrix(c(LAM_tab[,1],LAM_tab[,2],LAM_tab[,3],LAM_tab[,4],LAM_tab[,5],LAM_tab[,6],LAM_tab[,7],LAM_tab[,8]),nrow=22,ncol=8)

dim(matLAM) = c(22,8)
colnames(matLAM) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment","Average Beer","Average Wine","Average Spirit")
matLAM

M6 = cor(matLAM)
pairs(matLAM)

corrplot(M6, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M6, is.corr = FALSE, method = "number",bg="#72c475")

########################################################

##### Mappa Stati per felicità #####

worldtb = map_data("world") %>%
  as_tibble()

class(happiness$Country.or.region)

happiness$Country.or.region[happiness$Country.or.region == "United States"] <- "USA"

happiness$Country.or.region[happiness$Country.or.region == "United Kingdom"] <- "UK"

happiness$Country.or.region[happiness$Country.or.region == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"

happiness$Country.or.region[happiness$Country.or.region == "Congo (Brazzaville)"] <- "Republic of Congo"

happiness$Country.or.region[happiness$Country.or.region == "North Macedonia"] <- "Macedonia"

View(happiness)

happinessMap = left_join(worldtb,happiness,by=c("region" = "Country.or.region"))
View(happinessMap)

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Score), color="black") +
  scale_fill_gradient2(low="black",high="#FFFB1F",midpoint = 5.4) +
  theme_minimal()+
  labs(
    title = "Top 10 Stati più felici", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#E6AF2E"),
    legend.position = "right"
  ) 

##### Mappa Stati per GDP pro Capite #####

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=GDP.per.capita), color="black") +
  scale_fill_gradient2(low="white",mid="#3F74CA",high="black",midpoint = 0.82) +
  theme_minimal()+
  labs(
    title = "Stati per GDP Pro Capita", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#3F74CA"),
    legend.position = "right"
  ) 

##### Mappa Stati per Social Support #####

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Social.support), color="black") +
  scale_fill_gradient2(low="black",mid="#F96D10",high="khaki1",midpoint = 0.8) +
  theme_minimal()+
  labs(
    title = "Stati per Supporto Sociale", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#F96D10"),
    legend.position = "right"
  ) 

##### Mappa Stati per Average Corruptipon #####

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Perceptions.of.corruption), color="black") +
  scale_fill_gradient2(low="black",mid="#F96D10",high="chartreuse",midpoint = 0.23) +
  theme_minimal()+
  labs(
    title = "Stati per Percezionde della Corruzione", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#F96D10"),
    legend.position = "right"
  ) 

