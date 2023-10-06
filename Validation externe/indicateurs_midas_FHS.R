# Indicateurs MIDAS #

######## PARTIE 0 : Préparation des données  ########

#### Librairie ####
librairie = c("knitr","xlsx","lubridate","png","jpeg","tinytex","haven","xlsx",
              "here","readxl","openxlsx","dplyr","tidyr","tidyverse","ggplot2","writexl","arrow")

#Installation des librairies manquantes
installed_librairie = librairie %in% rownames(installed.packages())
if(any(installed_librairie == F)){
  install.packages(librairie[!installed_librairie])
}
#Chargement des librairies
invisible(lapply(librairie,library,character.only = T))

#### Fonctions utiles ####
`%!in%` = Negate(`%in%`)

arrondi <- function (x,digit=0){
  x<-x*10^digit
  x<-ifelse(x-trunc(x)>=0.5,trunc(x)+1,trunc(x))
  x/10^digit
}

#### Import des données ####
de = read_sas("C:/~/ech_de.sas7bdat")

d2 = read_sas("C:/~/ech_d2.sas7bdat")

e0 = read_sas("C:/~/ech_e0.sas7bdat")%>%
  #Heures d'activité réduite au format numérique
  mutate(NBHEUR = as.numeric(NBHEUR)) 

#### Calcul du début et de la fin du mois ####
deb_fin_mois = data.frame(debut_mois = seq.Date(as.Date("2018/01/01"),as.Date("2021/12/01"),by= "+1 months"))%>%
  mutate(fin_mois = (debut_mois + months(1)) - 1,
         mois = paste0(substr(debut_mois,1,4),substr(debut_mois,6,7)))

liste_mois = c(deb_fin_mois$mois)

#### Récupération des DEFM ####
defm = data.frame()
for (moisboucle in liste_mois){
  print(moisboucle)
  temp =  de %>%
    #Date d'inscription inférieure ou égale à la fin du mois 
    filter(DATINS <= pull(deb_fin_mois%>%filter(mois==moisboucle)%>%
                            select(fin_mois))
           #Date d'annulation supérieure à la fin du mois
           & (is.na(DATANN) | DATANN >  pull(deb_fin_mois%>%filter(mois==moisboucle)%>%
                                               select(fin_mois)))) %>% 
    mutate(mois = moisboucle)
  defm = defm %>% bind_rows(temp)
}

#Allègement de la table DEFM pour faciliter la deuxième jointure
defm2 = defm %>% 
  filter(Region %!in% c(6,7,8,9,97)) %>% 
  filter(CATREGR %in% c("1","2","3")) %>%
  select(id_midas,mois)


#### Récuperation des indemnisables ####
indem = data.frame()
for (moisboucle in liste_mois){
  print(moisboucle)
  temp =  d2 %>%
    #Jour de fin de validité supérieur ou égal à fin du mois 
    filter(jourfv >  pull(deb_fin_mois %>% filter(mois==moisboucle) %>%
                            select(fin_mois))) %>%
    mutate(mois = moisboucle)
  indem = indem %>% bind_rows(temp)
}

indem = indem %>%
  select(id_midas,mois,indem)

#### Jointure des heures d'activité réduite aux DEFM (tables DE et E0) ####
jointure_defm_e0 = defm %>%
  left_join(e0, by = c("id_midas" = "id_midas", "mois" = "MOIS"))


#### Jointure des indemnisables aux DEFM (tables DE et D2) ####
jointure_defm_indem = defm2 %>%
  inner_join(indem, by = c("id_midas" = "id_midas", "mois" = "mois")) %>% 
  mutate(indemnisable = 1)



######## PARTIE 1 : Nombre de DEFM inscrits + ancienneté d'inscription ########

#### Catégorisation ####
defm_categorie = jointure_defm_e0 %>% 
  mutate(catnouv = case_when(CATREGR %in% c("1","2","3") & (is.na(NBHEUR)==TRUE | NBHEUR == 0)  ~ "A",
                             CATREGR %in% c("1","2","3") & (NBHEUR > 0 & NBHEUR <= 78) ~ "B",
                             CATREGR %in% c("1","2","3") & (NBHEUR > 78)  ~ "C",
                             CATREGR == "4" ~ "D",
                             CATREGR == "5" ~ "E",
                             TRUE ~ "NP"))%>%
  #Récupère la date de fin de mois pour le calcul de l'ancienneté
  left_join(deb_fin_mois %>% select(mois,fin_mois), by = c("mois"="mois"))%>%
  mutate(ancien_jour = difftime(fin_mois,DATINS,units = "day"),
         ancien_mois = time_length(difftime(fin_mois, DATINS),"month"))%>%
  #Catégorisation de l'ancienneté 
  mutate(categorie_ancien = case_when(ancien_mois < 3 ~ "1 - Moins de 3 mois",
                                      ancien_mois >= 3 & ancien_mois < 6 ~ "2 - 3 mois - 6 mois",
                                      ancien_mois >= 6 & ancien_mois < 12 ~ "3 - 6 mois - 1 an",
                                      ancien_mois >= 12 & ancien_mois < 24 ~ "4 - 1 an - 2 ans",
                                      ancien_mois >= 24 & ancien_mois < 36 ~ "5 - 2 ans - 3 ans",
                                      ancien_mois >= 3 ~ "6 - 3 ans et plus",
                                      TRUE ~ "NP"))

#### DEFM en moyenne annuelle : France entière hors Mayotte ####
indicateur_defm_catnouv = defm_categorie %>% 
  #Hors Mayotte 
  filter(Region != 6)%>%
  #Par mois et catégories
  group_by(mois,catnouv) %>%
  summarise(defmA = n_distinct(id_midas) * 100)%>%
  pivot_wider(names_from = catnouv,
              values_from = c(defmA))%>%
  mutate(ABC = A + B + C,
         ABCDE = A + B + C + D + E)

defm_mean_an = indicateur_defm_catnouv %>%
  mutate(an = substr(mois,1,4))%>%
  group_by(an)%>%
  summarise(mean_A = mean(A),
            mean_B = mean(B),
            mean_C = mean(C),
            mean_ABC = mean(ABC),
            mean_D = mean(D),
            mean_E = mean(E))

#Moyenne annuelle 2020
#Catégorie A : 3 875 500 observées, 3 865 283 MIDAS
#Catégorie B : 757 000 observées, 758 950 MIDAS
#Catégorie C : 1 352 600 observées, 1 358 842 MIDAS
#Catégorie ABC : 5 985 100 observées, 5 983 075 MIDAS
#Catégorie D : 341 200 observées, 345 516.7 MIDAS
#Catégorie E : 360 400 observées, 364 458 MIDAS

#### DEFM ABC par ancienneté ####
defm_anciennete = defm_categorie %>% filter(catnouv %in% c("A","B","C"))%>%
  group_by(mois,categorie_ancien) %>%
  summarise(defm = n_distinct(id_midas) * 100) %>% 
  pivot_wider(names_from = categorie_ancien,
              values_from = c(defm))

#### Export des données ####
export_midas = file.path("C:/Users/gaetan.guillermin/Documents/MIDAS/Indicateurs_MIDAS_GG.xlsx")
onglet_midas = list("Catégories" = indicateur_defm_catnouv,
                    "Ancienneté" = defm_anciennete)

write_xlsx(onglet_midas, export_midas)

#### Vérifications ####
doublon = defm %>% 
  select(id_midas,Region,id_fhs,DATINS,DATANN,CATREGR,mois) %>%
  mutate(doublon = case_when(id_midas == lag(id_midas) ~ "doublon",
                             TRUE ~ "NP"))%>%
  filter(doublon == "doublon")%>% distinct(id_midas)

save(doublon,file = "C:/Users/gaetan.guillermin/Documents/MIDAS/id_midas_doublon.R")

#Exemple d'un identifiant MIDAS ayant 2 périodes d'inscription simultanées
exemple_doublon_de = de %>% filter(id_midas == "MIDAS0017979145")%>%
  select(id_midas,Region,id_fhs,DATINS,DATANN)


exemple_doublon_e0 = e0 %>% filter(id_midas == "MIDAS0017979145")



######## PARTIE 2 : Nombre d'indemnisables + taux de couverture ########

#### Indicateur 1 : Nombre d'indemnisables ABC, France métropolitaine ####
indicateur_indem1 = jointure_defm_indem %>% 
  group_by(mois) %>% 
  summarise(indemABC = n_distinct(id_midas) * 100)

#### Indicateur 2 : Nombre d'indemnisables ABC, France métropolitaine - assurance chomage ####
defm_indem2 = jointure_defm_indem %>%
  mutate(cat_indem = case_when(indem %in% c("29","33","34","35","40","41","43","47","48","54","55",
                                            "61","62","64","65","66","67","70","82","83","84","85",
                                            "86","91","AC","AD","AG","AI","AK","AL","AM","AN","AQ",
                                            "AR","BB","BC","BK","BM","BN","BO","BP","BQ","BU","BW",
                                            "BY","BZ","CB","CC","CJ","CK","CL","CM","CN","CO","CP",
                                            "CQ","CR","CS","CU","CY","CZ","DM","DO","DP","DQ","DR",
                                            "DT","DU","EA","EB","EF","EI","EL","EM","EN","EO") ~ "AC",
                               indem %in% c("24","25","28","39","44","45","50","51","52","56","57",
                                            "58","59","60","63","75","76","77","78","81","92","93",
                                            "95","96","97","99","AT","AY","BD","BE","BG","BJ","BR",
                                            "CI","CX","DV","DW","DY","DX") ~ "Etat",
                               TRUE ~ "NA"))

indicateur_indem2 = defm_indem2 %>% 
  filter(cat_indem == "AC") %>%
  group_by(mois) %>% 
  summarise(indemABC_AC = n_distinct(id_midas) * 100)


#### Indicateur 3 : Nombre d'indemnisables ABC, France métropolitaine - etat ####
indicateur_indem3 = defm_indem2 %>% 
  filter(cat_indem == "Etat") %>%
  group_by(mois) %>% 
  summarise(indemABC_Etat = n_distinct(id_midas) * 100)

#### Indicateur 4 : Taux de couverture des DEFM ABC, France métropolitaine ####
jointure_defm_indem$mois <- NULL
jointure_defm_indem$Region <- NULL
jointure_defm_indem$CATREGR <- NULL

defm2 = defm %>%
  left_join(jointure_defm_indem, by = c("id_midas" = "id_midas"))

defm3 = defm2 %>% 
  group_by(mois) %>% 
  summarise(defmABC = n_distinct(id_midas) * 100)

indicateur_indem4 = indicateur_indem1 %>% 
  left_join(defm3,by = c("mois" = "mois")) %>%
  mutate(couverture = indemABC/defmABC)

#### Indicateur 5 : Taux de couverture des DEFM ABC, France métropolitaine - assurance chômage ####
indicateur_indem5 = indicateur_indem2 %>%
  left_join(defm3,by = c("mois" = "mois")) %>%
  mutate(couverture_AC = indemABC_AC/defmABC)

#### Indicateur 6 : Taux de couverture des DEFM ABC, France métropolitaine - Etat ####
indicateur_indem6 = indicateur_indem3 %>%
  left_join(defm3,by = c("mois" = "mois")) %>%
  mutate(couverture_Etat = indemABC_Etat/defmABC)

#1 Nombre d'indemnisables ABC en décembre 2021 : 3 627 000 PE, 3 803 800 MIDAS
#2  - Dont Assurance-chômage : 3 204 800 PE, 3 402 100 MIDAS
#3  - Dont Etat : 345 200 PE, 431 800 MIDAS
#4 Taux de couverture en décembre 2021 : 68,2 % PE, 67,1 % MIDAS
#5  - Dont Assurance-chômage : 60,2 % PE, 60,1 % MIDAS
#6  - Dont Etat : 6,5 % PE, 7,6 % MIDAS

