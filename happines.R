library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")


constructorResults = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/constructorResults.csv")

View(constructorResults)

circuits = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/circuits.csv")

View(circuits)

races = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/races.csv")

View(races)

results = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/results.csv")

View(results)

drivers = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/drivers.csv")

View(drivers)

constructors = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/constructors.csv")

View(constructors)

status = read.csv("C:/Users/giaco/Desktop/progetto_esame/formula1/status.csv")

View(status)

######################################################

races[7] <- NULL
races[8] <- NULL
 
circuits[8] <- NULL
circuits[9] <- NULL

constructors[5] <- NULL
constructors[6] <- NULL

View(constructors)

races_results = inner_join(races,results,by=c("raceId" = "raceId"))
View(races_results)

racRes_status = inner_join(races_results,status,by=c("statusId" = "statusId"))
View(racRes_status)

stat_const = inner_join(racRes_status,constructors,by=c("constructorId"="constructorId"))
View(stat_const)

new_Stat = stat_const %>% 
  filter(!str_detect(status, "Lap")) %>%
  filter(!str_detect(status, "Finished"))

View(new_Stat)

scuderia_inc =  new_Stat %>%
  count(name.y)

View(scuderia_inc)

############################################################
numScudIncidenti =  new_Stat %>%
  count(name.y)

View(numScudIncidenti)

numScudCorse =  stat_const  %>%
  count(name.y) 

View(numScudCorse)

numScudCorse = numScudCorse %>%
  filter(n > 2)

numCorseIncidenti = inner_join(numScudCorse,numScudIncidenti,by=c("name.y" = "name.y"))

View(numCorseIncidenti)

numCorseIncidenti = numCorseIncidenti %>% 
  rename(numCorse = n.x,numIncidenti = n.y)

View(numCorseIncidenti)

constructor = as.vector(numCorseIncidenti$name.y)

nRaces = as.vector(numCorseIncidenti$numCorse)

nAccidents = as.vector(numCorseIncidenti$numIncidenti)
nAccidents

percentuale <- rep(NA, length(nAccidents))

for(j in 1:length(nAccidents)){
  
  percentuale[j] = (nAccidents[j] / nRaces[j]) * 100
  
}

percentuale

compl_dataf = data.frame(constructor,nRaces,nAccidents,percentuale)
View(compl_dataf)

ggplot(data = compl_dataf) +
  geom_bar(mapping = aes(x = nRaces, fill = percentuale), position = "dodge")

