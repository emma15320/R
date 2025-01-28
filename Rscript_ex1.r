> "M1 - Afficher une phrase indiquant le % d’abstention à l’échelle nationale."
    
 abs <- mean (resultats_pres2022$Part_abs)
 abs

 paste("L'abtention moyenne est de ", round(abs, digits=2), "%")

 "M2 – Trouver le nombre de communes pour lesquelles le vote « Jadot » se situe entre 20% et 40%. Le résultat sera créé dans une nouvelle variable nommée « voteJadot2040 »"

 voteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ]
 View (voteJadot2040) # permet de visualiser la nouvelle extraction

 "M3 – Réaliser une extraction du tableau de données sur la commune de Saintes.
 + + Trouver un moyen d’exporter le résultat en format csv (utiliser la fonction write.csv() ). Ouvrir le résultat sur Excel sous forme d’un tableau compréhensible et sans erreur d’encodage"

 voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' ,]
 View (voteSaintes) # permet de visualiser la nouvelle extraction
 
 # Exporter les données en format CSV
 write.csv(voteSaintes, file = "voteSaintes.csv", row.names = FALSE)
 print("Le fichier voteSaintes.csv a été créé avec succès.")

 "M4 – Extraire les parts de vote des 3 principaux candidats (Macron, Le Pen & Mélenchon) sur la commune de Saintes.Le tableau se nommera « voteSaintes_Candidats1_3 »).
 + Vous comparerez ensuite les écarts types des 3 échantillons."

 voteSaintes_Candidats1_3 <- voteSaintes [ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
 View (voteSaintes_Candidats1_3) # permet de visualiser la nouvelle extraction

 # Calcul et affichage des écarts types pour chaque candidat
 ecart_type_macron <- sd(voteSaintes_Candidats1_3$Part_Macron)
 ecart_type_lepen <- sd(voteSaintes_Candidats1_3$Part_LePen)
 ecart_type_melenchon <- sd(voteSaintes_Candidats1_3$Part_Melenchon)
  
  # Affichage des résultats
  cat("Ecart type Macron :", ecart_type_macron, "\n")
    Ecart type Macron : 4.587577 
  cat("Ecart type Le Pen :", ecart_type_lepen, "\n")
    Ecart type Le Pen : 4.207913 
  cat("Ecart type Mélenchon :", ecart_type_melenchon, "\n")
    Ecart type Mélenchon : 3.846068 

 "M5 – Ajouter une nouvelle colonne dans le tableau « voteSaintes_Candidats1_3 ».
 + Cette nouvelle colonne prendra la valeur « MacronSup30 » si les % de votes Macron sont supérieurs à 30%. + Compter ensuite le nombre de lignes du tableau."

 # Ajouter une nouvelle colonne avec les conditions spécifiées
 voteSaintes_Candidats1_3$MacronSup30 <- (ifelse(voteSaintes_Candidats1_3$Part_Macron > 30, print("MacronSup30"), "N/A"))

 # Compter le nombre de lignes du tableau
 nombre_lignes <- nrow(voteSaintes_Candidats1_3)
 cat("Nombre de lignes du tableau:", nombre_lignes)

 "M6 – Calculez les maximums de % vote « Arthaud » dans chaque commune de Vaucluse (une commune pouvant comporter plusieurs bureaux de vote)"

 vaucluse_results <- filter(resultats_pres2022, Code_dep == '84')

 tapply(vaucluse_results$Part_Arthaud,vaucluse_results$Code_com, mean )

 tapply(vaucluse_results$Part_Arthaud,vaucluse_results$Libelle, mean )


    "M7 – Représentez un graphique sectoriel composé des % de voix des principaux partis de gauche à
    l’échelle de la ville de La Rochelle. Les couleurs devront être personnalisées."

    rochelle_results <- filter(resultats_pres2022, Libelle == 'La Rochelle' & resultats_pres2022$Code_dep == '17' ,)
    View (rochelle_results)
    col1 <- c("Hidalgo", "Jadot", "Melenchon")
    col2 <- c(mean(rochelle_results$Part_Hidalgo), mean(rochelle_results$Part_Jadot), mean(rochelle_results$Part_Melenchon))
    data <- data.frame(group=col1, value=col2)
    ggplot(data, aes(x="", y=value , fill=group)) +
    geom_bar(stat="identity", width=1) +
    geom_col() +
    coord_polar("y", start=0) + # coord_polar permet de convertir des barres en cercles
    geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#fdfdfd", "#172cca", "#ca1717"))


    "M8 – Réalisez un multi-graphe sectoriel illustrant les parts de vote de votre choix pour chacun des bureaux de vote de
    la ville de Royan."

 library(ggplot2)
 library(dplyr)
 library(tidyr)

 # Filtrer les données pour Royan
 voteRoyan <- filter(resultats_pres2022, Libelle == 'Royan' & Code_dep == '17')

 # Sélection des colonnes pertinentes
 voteRoyan_Candidats1_4 <- voteRoyan %>% 
  select(Code_BV, Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse)

 # Réorganisation des données au format long
 reformat <- pivot_longer(voteRoyan_Candidats1_4, 
                         cols = c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"), 
                         names_to = 'candidats', 
                         values_to = 'parts') 

 # Création du graphique avec coord_polar
 ggplot(data = reformat, aes(x = candidats, y = parts, fill = candidats)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Code_BV) +
  ggtitle("Parts de vote dans les BV de Royan") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", face = "bold", size = 20, hjust = 0.5, color = "#555555"),
    axis.text.x = element_blank(), # Supprime le texte de l'axe x
    axis.ticks.x = element_blank(), # Supprime les ticks de l'axe x
    ) +
  scale_fill_manual(values = c("#0a3895", "#b831f3", "#f33157", "#6091f6")) +
  coord_polar("y", start = 0)


 "M9 – A l’aide d’une requête SQL, supprimer les types de demande = 1 de la table php.form."

 library(DBI)
library(RPostgres)

# Informations de connexion à la base de données
db <- "postgres"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "272628foot"

# Connexion à la base de données
conn <- dbConnect(RPostgres::Postgres(), dbname = db, host = db_host, port = db_port, user = db_user, password = db_pass)

# Vérification de la connexion
print(conn)

# Requête de sélection
requete <- dbGetQuery(conn, 'SELECT * FROM php.from;')
View(requete)

# Suppression des lignes où type_de_demande = 1
requete_sql <- "DELETE FROM php.from WHERE typedemande = '1'"
resultat <- dbExecute(conn, requete_sql)
View(resultat)

"Markdown"
---
output:
  word_document: default
  html_document: default
---
# Test
---
title: "Rapport R"
output: html_document
---
```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```
## Parts de vote des principaux partis de gauche à l'échelle de la ville de La Rochelle
```{r echo=FALSE, message=FALSE, warning=FALSE}
library(readxl)
library(ggplot2)
resultats_pres2022 <- read_excel("C:/1_LPSIG/6_Cours_R/1_Donnees/donnees/resultats_pres2022.xlsx")
voteLaRochelle <- resultats_pres2022 [resultats_pres2022$Libelle =='La Rochelle' & resultats_pres2022$Code_dep == '17' ,]
col1 <- c("Melenchon", "Jadot", "Hidalgo")
col2 <- c(mean(voteLaRochelle$Part_Melenchon), mean(voteLaRochelle$Part_Jadot), mean(voteLaRochelle$Part_Hidalgo))
data <- data.frame(group=col1, value=col2)
ggplot(data, aes(x="", y=value , fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) + scale_fill_manual(values =
c("#ffadf8", "#0cb240", "#f03e18"))
```

"M10 – Sur la base du baromètre de satisfaction client issu de la SNCF, (barometre_sncf_2019.xls), vous
allez : 
- extraire les gares de typologie « 123 » et correspondant à la région « Nouvelle-Aquitaine »
- calculer dans une nouvelle colonne la moyenne des notes des gares
- réaliser des statistiques descriptives sur le résultat précédent
- extraire les notes moyennes calculées > à 7,5 et exporter dans un fichier CSV
- réaliser un multi-graphe représentant les différents types de note par gare, exportable en Png
- créer un fichier html composé des résultats obtenus via R Markdown
- déposez vos résultats sur le Drive"

 library(readxl)
 barometre_sncf_2019 <- read_excel("C:/1_LPSIG/6_Cours_R/1_Donnees/donnees/barometre_sncf_2019.xlsx")
 View(barometre_sncf_2019)
 
 resultat123 <- filter(barometre_sncf_2019, Typologie_gare == "123", Agence == "Nelle Aquitaine")
 View(resultat123)


 gares_moyenne <- barometre_sncf_2019 %>% rowwise() %>% mutate(Moyenne = mean(c_across(c(Infos, Deplacement, Proprete, Agreable, Services, Satisfaction))))
 View(gares_moyenne)
 
 summary(gares_moyenne$Moyenne)

 # Filtrer les enregistrements avec une moyenne supérieure à 7,5
 gares_fil <- gares_moyenne %>%
 filter(Moyenne > 7.5)
 # Exporter les données filtrées dans un fichier CSV
 write.csv(gares_fil, file = "gares_fil.csv", row.names = FALSE) 
 # Afficher un message de confirmation
 print("Le fichier gares_fil.csv a été créé avec succès.")



# Chargement des bibliothèques
 library(ggplot2)
 library(tidyr)
 library(dplyr)
 
 # Transformation des données au format long
 gares_long <- resultat123 %>%
     pivot_longer(
         cols = c(Infos, Deplacement, Proprete, Agreable, Services, Satisfaction),
         names_to = "Type_Note",
         values_to = "Valeur_Note"
     )
 
 # Création du multi-graphe avec facettes
 multi_graphe <- ggplot(gares_long, aes(x = Type_Note, y = Valeur_Note, fill = Type_Note)) +
     geom_bar(stat = "identity", width = 0.8) +  # Barres plus larges
     facet_wrap(~ Gare, scales = "free_y", ncol = 3) +  # Facettes par gare, 3 colonnes
     theme_minimal() +  # Thème simple et épuré
     labs(
         title = "Multi-graphe des différents types de notes par gare",
         x = NULL,  # Supprime le label de l'axe X
         y = "Valeur des Notes",
         fill = "Type de note"
     ) +
     scale_fill_brewer(palette = "Set2") +  # Palette de couleurs agréable
     theme(
         strip.text = element_text(size = 7, face = "bold"),  # Taille réduite des labels des facettes (noms des gares)
         axis.text.x = element_blank(),  # Supprime les types de notes sous les diagrammes
         axis.ticks.x = element_blank(),  # Supprime les ticks de l'axe X
         axis.text.y = element_text(size = 10),  # Taille des labels des valeurs
         axis.title = element_text(size = 12, face = "bold"),  # Taille des titres des axes
         legend.title = element_text(size = 12, face = "bold"),  # Titre de la légende
         legend.text = element_text(size = 10),  # Texte de la légende
         plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Titre centré
         plot.margin = margin(10, 10, 10, 10)  # Marges autour du graphique
     ) 
 # Affichage du graphique
 print(multi_graphe)

 # Exportation en PNG
 ggsave("multi_graphe_notes.png", plot = multi_graphe, width = 14, height = 10, dpi = 300)





---
title: "Analyse des données SNCF"
author: "Emma"
date: "2025-01-27"
output:
  html_document: default
  word_document: default
---
# Test
---
title: "Rapport R"
output: html_document
---
```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```
{r echo=FALSE, message=FALSE, warning=FALSE}
library(readxl)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

barometre_sncf_2019 <- read_excel("C:/1_LPSIG/6_Cours_R/1_Donnees/donnees/barometre_sncf_2019.xlsx")
View(barometre_sncf_2019)

resultat123 <- filter(barometre_sncf_2019, Typologie_gare == "123", Agence == "Nelle Aquitaine")
View(resultat123)

gares_moyenne <- barometre_sncf_2019 %>% 
  rowwise() %>% 
  mutate(Moyenne = mean(c_across(c(Infos, Deplacement, Proprete, Agreable, Services, Satisfaction))))
View(gares_moyenne)
summary(gares_moyenne$Moyenne)

gares_fil <- gares_moyenne %>%
  filter(Moyenne > 7.5)
write.csv(gares_fil, file = "gares_fil.csv", row.names = FALSE)
print("Le fichier gares_fil.csv a été créé avec succès.")


# Transformation des données au format long
gares_long <- resultat123 %>%
  pivot_longer(
    cols = c(Infos, Deplacement, Proprete, Agreable, Services, Satisfaction),
    names_to = "Type_Note",
    values_to = "Valeur_Note"
  )

# Création du multi-graphe avec facettes
multi_graphe <- ggplot(gares_long, aes(x = Type_Note, y = Valeur_Note, fill = Type_Note)) +
  geom_bar(stat = "identity", width = 0.8) +  # Barres plus larges
  facet_wrap(~ Gare, scales = "free_y", ncol = 3) +  # Facettes par gare, 3 colonnes
  theme_minimal() +  # Thème simple et épuré
  labs(
    title = "Multi-graphe des différents types de notes par gare",
    x = NULL,  # Supprime le label de l'axe X
    y = "Valeur des Notes",
    fill = "Type de note"
  ) +
  scale_fill_brewer(palette = "Set2") +  # Palette de couleurs agréable
  theme(
    strip.text = element_text(size = 7, face = "bold"),  # Taille réduite des labels des facettes (noms des gares)
    axis.text.x = element_blank(),  # Supprime les types de notes sous les diagrammes
    axis.ticks.x = element_blank(),  # Supprime les ticks de l'axe X
    axis.text.y = element_text(size = 10),  # Taille des labels des valeurs
    axis.title = element_text(size = 12, face = "bold"),  # Taille des titres des axes
    legend.title = element_text(size = 12, face = "bold"),  # Titre de la légende
    legend.text = element_text(size = 10),  # Texte de la légende
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Titre centré
    plot.margin = margin(10, 10, 10, 10)  # Marges autour du graphique
  ) 

# Affichage du graphique
print(multi_graphe)

# Exportation en PNG
ggsave("multi_graphe_notes.png", plot = multi_graphe, width = 14, height = 10, dpi = 300)
```