
# Daten Einlesen
bib_bestand <- read_excel(here::here("daten/raw/R_Projekt.xlsx")) |> 
  janitor::clean_names() 


#Die Funktion gibt die Anzahl der Einträge in Publication Date zurück, bei denen ein Wert vorhanden ist: 8462
bib_gefiltert<-bib_bestand$publication_date
sum(!is.na(bib_bestand$publication_date))

#2 Aus der Spalte "Publication Date" das Minuszeichen entfernen, da sonst eine Sortierung nach Jahreszahlen (auch filter) nicht möglich ist.

bib_bestand_besenrein <- bib_bestand
bib_bestand_besenrein$publication_date <- gsub("-", "", bib_bestand_besenrein$publication_date)
glimpse(bib_bestand_besenrein)


#3 Einzig die Spalte publication_date aus dem Datensatz extrahiert.    
bib_nur_publikation <- subset(bib_bestand_besenrein, select = publication_date)

#4 In nummerischen Vektor umgewandelt für den Filter nach Jahreszahlen
bib_bestand_besenrein$publication_date <- as.numeric(bib_bestand_besenrein$publication_date)

#5 In 50er-Jahr-Schritten jeweils die einzelnen Veröffentlichungsdaten aus "bib_bestand_besenrein" gefiltert, um daraus am Ende 6 Grafiken erstellen zu können. 

bib_gefiltert_vor_1800 <- filter(bib_bestand_besenrein,(publication_date <=1800))

bib_gefiltert_1800_1850 <- filter(bib_bestand, between(publication_date, 1800, 1850))

bib_gefiltert_1851_1899 <- filter(bib_bestand_besenrein, between(publication_date, 1850, 1900)) 

bib_gefiltert_1900_1950 <- filter(bib_bestand_besenrein, between(publication_date, 1900, 1950)) 

bib_gefiltert_1951_1999 <- filter(bib_bestand_besenrein, between(publication_date, 1951, 1999)) 

bib_gefiltert_2000_2050 <- filter(bib_bestand_besenrein, between(publication_date, 2000, 2050)) 


#Versuch, nun auch noch nach der jeweiligen Standort-Signatur (A-E) zu sortieren

#A Geschichte des Auslands
bib_gefiltert_1600_2025_A <- filter(
  bib_bestand_besenrein,
  between(publication_date, 1600, 2025),
  grepl("^A", permanent_call_number) | grepl("A.*III|III.*A", permanent_call_number))
#Visualisierung
ggplot(bib_gefiltert_1600_2025_A, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bibliotheksbestand A nach Veröffentlichungszeit der einzelnen Medien",
                  subtitle="in der StAZH Bibliothek")+theme_stat()

#B Schweizer Geschichte
bib_gefiltert_1600_2025_B <- filter(
  bib_bestand_besenrein,
  between(publication_date, 1600, 2025),
  grepl("^B", permanent_call_number) | grepl("B.*III|III.*B", permanent_call_number))
#Visualisierung
ggplot(bib_gefiltert_1600_2025_B, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bibliotheksbestand B nach Veröffentlichungszeit der einzelnen Medien",
                  subtitle="in der StAZH Bibliothek")+theme_stat()

#C Geschichte der Kantone
bib_gefiltert_1600_2025_C <- filter(
  bib_bestand_besenrein,
  between(publication_date, 1600, 2025),
  grepl("^C", permanent_call_number) | grepl("C.*III|III.*C", permanent_call_number))
#Visualisierung
ggplot(bib_gefiltert_1600_2025_C, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bibliotheksbestand C nach Veröffentlichungszeit der einzelnen Medien",
                  subtitle="in der StAZH Bibliothek")+theme_stat()

#D Zürcher Geschichte
bib_gefiltert_1600_2025_D <- filter(
  bib_bestand_besenrein,
  between(publication_date, 1600, 2025),
  grepl("^D", permanent_call_number) | grepl("D.*III|III.*D", permanent_call_number))
#Visualisierung
ggplot(bib_gefiltert_1600_2025_D, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bibliotheksbestand D nach Veröffentlichungszeit der einzelnen Medien",
                  subtitle="in der StAZH Bibliothek")+theme_stat()

#E Hilfswissenschaften
bib_gefiltert_1600_2025_E <- filter(
  bib_bestand_besenrein,
  between(publication_date, 1600, 2025),
  grepl("^E", permanent_call_number) | grepl("E.*III|III.*E", permanent_call_number))
#Visualisierung
ggplot(bib_gefiltert_1600_2025_E, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bestand E nach Erscheinungsdatum d. Bücher",
                  subtitle="in der StAZH Bibliothek")+theme_stat()+labs(x = "Erscheinungsdatum", y = "Anzahl Medien")


#Probe-Visualisierung 
ggplot(bib_gefiltert_1800_1850, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Publikationen 1800-1850",
                  subtitle="in der StAZH Bibliothek")

ggplot(bib_gefiltert_1600_2025_E, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Bestand E nach Erscheinungsdatum d. Bücher",
                  subtitle="in der StAZH Bibliothek")+theme_stat()+labs(x = "Erscheinungsdatum", y = "Anzahl Medien")


#Grafik über den gesamten Zeitraum mit allen Medien, welche eine Veröffentlichungsangabe haben
ggplot(bib_bestand_besenrein, mapping=aes(x=publication_date))+
  geom_bar()+labs(title="Publikationen 1800-2025",
                  subtitle="in der StAZH Bibliothek")



ggplot(bib_bestand_besenrein, mapping=aes(x=publication_date,y=permanent_call_number))+
  geom_point()+labs(title="Bestand A nach Erscheinungsdatum (vor 1800)",
                    subtitle="in der StAZH Bibliothek")+labs(x = "Erscheinungsdatum", y = "Anzahl Medien")


ggplot(bib_A_bis_E, aes(x = publication_date, y = signatur_gruppe)) +
  geom_point() +
  labs(
    title = "Bestand A-E nach Erscheinungsdatum (vor 1800)",
    subtitle = "in der StAZH Bibliothek",
    x = "Erscheinungsdatum",
    y = "Signatur-Gruppe"
  )



# Linien-Plot
ggplot(bib_A_bis_E_agg, aes(x = publication_date, y = anzahl, color = signatur_gruppe)) +
  geom_line(size = 1) +
  labs(
    title = "Medienzahl nach Erscheinungsjahr und Signaturgruppe (A–E)",
    subtitle = "in der StAZH Bibliothek",
    x = "Erscheinungsdatum",
    y = "Anzahl Medien",
    color = "Signatur"
  ) +
  theme_minimal()





