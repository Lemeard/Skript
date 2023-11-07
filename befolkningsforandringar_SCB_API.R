###############################
### Befolkningsförändringar ###

# Följande skript drar ut ett antal variabler med den senaste statistiken som finns tillgänglig hos SCB. 
# Skriptet namnger utskriftsfilerna med den senaste månaden som det finns statistik för samt vilken geografi det gäller

# För att använda skriptet behöver variabeln "geo" ändras till den kommun man är intresserad av
# Ändra även input för utskriftsmapp och bearbetning/författare efter tycke
# Sen för att använda är det endast att markera hela skriptet, köra det och kika i utskriftsmappen

### Läs in paket  ------------------------------------------------------
library(pxweb)
library(tidyverse)
library(RColorBrewer)
library(stringr)
library(writexl)

######## Inställningar ###########################################################################################################################
geo <- c("2580") # ändra geografi som ska användas här. Helsingborg är exempelvis 1283.                                                        ###
                                                                                                                                               ###
# Kommunkoderna återfinns här:                                                                                                                 ###
# https://www.scb.se/hitta-statistik/regional-statistik-och-kartor/regionala-indelningar/lan-och-kommuner/lan-och-kommuner-i-kodnummerordning/ ###
                                                                                                                                               ###
# Här väljs vilka diagram som kommer att skrivas ut.                                                                                           ###
# NA innebär att variabeln exkluderas.                                                                                                         ###
                                                                                                                                               ###
folkökning <- c("folkökning")                                      # Folkökning                                                                ###
födda <- c("födda")                                                # Födda                                                                     ###
döda <- c("döda")                                                  # Döda                                                                      ###
födelseöverskott <- c("födelseöverskott")                          # Födelseöverskott                                                          ###                                                                                            ###
flyttningsöverskott_län <-  c("flyttningsöverskott eget län")      # Flyttnetto mot eget län                                                   ###
flyttningsöverskott_övriga_sverige <-  c("flyttningsöverskott övriga Sverige") # Flyttnetto mot Sverige utom det egna länet                    ###
flyttningsöverskott_sverige_totalt <-  c("flyttningsöverskott inrikes totalt") # Flyttnetto mot Sverige (totalt)                               ###
invandringsöverskott <- c("invandringsöverskott")                  # Invandringsnetto/överskott                                                ###
utvandring   <- c("utvandringar")                                  # Utvandring                                                                ###
invandring   <- c("invandringar")                                  # Invandring                                                                ###                                                                                                                                              ###                                                                                                                                    ###                                                                    ###
                                                                                                                                               ###
utskriftsmapp <- c("G:/Avd Strategisk samhällsutveckling/Staben/06 Statistik/12. R/Utskrift/") # Utskriftsmapp                                 ###                                                                                                            ###
bearbetning <- c("Skript av Thomas Lassi, Helsingborgs Stad")      # Till förklaring i nedre högra hörnet                                      ###
                                                                                                                                               ###
##################################################################################################################################################

### Läser in färger som används längre ner i skriptet 
farger <- brewer.pal(9, "Blues")[c(TRUE, FALSE)] #Här går det såklart också att justera och använda andra färger också

### Drar hem statistik från SCB över befolkningsförändringar på månadsbasis
pxweb_query_list <- 
  list("Region"=geo,
       "Forandringar"=c("100","110","115","130","135","140","150","155","160","170","175","179","180","190","220","230","235","240","250","260","270"),
       "Kon"=c("1+2"),
       "ContentsCode"=c("000003KD"),
       "Tid"=c("*"))

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/ManadBefStatRegion",
            query = pxweb_query_list)
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Skapar två variabler som behövs för att skapa diagrammet ---------------------------------------
geo_namn <- unique(px_data_frame$region)
variabler <- c(na.omit(folkökning), na.omit(födda), na.omit(döda), na.omit(födelseöverskott), na.omit(flyttningsöverskott_län), na.omit(invandringsöverskott), na.omit(utvandring), na.omit(invandring), na.omit(flyttningsöverskott_övriga_sverige), na.omit(flyttningsöverskott_sverige_totalt))

# Skriver ut en excel-fil med alla variabler per år ----------------------------------------------
excel_output <- px_data_frame |> 
                      select(region, månad, förändringar, Befolkning) |> 
                      pivot_wider(id_cols = c("region", "månad"), names_from = "förändringar", values_from = "Befolkning")

write_xlsx(excel_output,paste0(utskriftsmapp,geo_namn, "_",max(px_data_frame$månad),".xlsx"))

# Skapar diagram ---------------------------------------------------------------------------------
for (i in 1:length(variabler)) {
    filtrerad_df <- px_data_frame |> 
                                      separate(månad, into = c("år", "månad"), sep = "M", convert = TRUE) |># Här splittar vi månads-kolumnen eftersom SCB skriver ihop år och månad med ett M mitt i.
                                      mutate(år = as.numeric(år),
                                             månad = as.numeric(månad)) |> #Försäkrar mig om att både års och månadskolumnerna är numeriska och inte character
                                      filter(region == geo_namn,  #Applicerar filter för att bara ta ut det vi för närvarande är intresserade av
                                             år>(as.numeric(format(Sys.Date(), "%Y"))-5), # Vi behöver också veta vat dagens datum är
                                             förändringar %in% variabler[i]) |> # Vi vill bara ha variablerna/förändringarna som vi har valt ovan
                                      select(år, månad, Befolkning) |> #Välj kolumnerna vi behöver
                                      group_by(år) |>
                                      mutate(kum_befolkning = cumsum(Befolkning)) #Gruppera efter år och skapa en kolumn där variabeln (befolkning) är kumulativ och summerar året tillochmed den innevarande månaden

    filtrerad_df <- filtrerad_df[order(filtrerad_df$år), ] # försäkrar oss om att dataframen är sorterad på år (för då kommer färgerna läggas in i rätt ordning senare)
    
    rubrik <- paste(toupper(substr(variabler[i], 1, 1)), tolower(substr(variabler[i], 2, nchar(variabler[i]))), sep = "") # Inte en ideal lösning, men skapar rubriker som börjar med stor bokstav                 
    
    #Nedan skapas ett diagram per vald variabel
    kumulativ_plot <-  ggplot(filtrerad_df, aes(x = månad, y = kum_befolkning, color = as.factor(år))) + # den här raden väljer variabler för x och y, samt gruppen som ska bestämma färgerna
                            geom_line(size=1.1)+
                            scale_color_manual(values = farger) +
                            scale_x_continuous(
                              limits = c(1, 12),
                              expand = c(0, 0),
                              breaks = seq(1, 12, by = 1)) +
                            theme(panel.border = element_blank(),
                                  axis.line = element_line(color = 'black'),
                                  legend.position = "bottom",
                                  panel.background = element_rect(fill = "white"),
                                  legend.key = element_blank(),
                                  legend.title = element_blank(),
                                  panel.grid.major.y = element_line(color = "lightgray", size = 0.3),
                                  axis.ticks.length.y = unit(0, "mm"),
                                  axis.ticks.length.x = unit(2, "mm"),
                                  plot.title = element_text(hjust = 0.5),
                                  axis.title = element_blank(),
                                  plot.caption = element_text(size = 5))+ ## detta är olika element/delar av temat som jag använder
                            labs(title = paste0(rubrik," (ackumulerad) per månad för ", paste0(unique(px_data_frame$region)), "\nmellan ", paste0(min(filtrerad_df$år), " och ", paste0(max(filtrerad_df$år)))), caption = paste0("Förklaring: ",variabler[i]," (ackumulerad) per månad\nKälla: SCB\nBearbetning: ", bearbetning))
                          
# Skriver ut diagrammet per månad och år (kumulativt)
ggsave(paste0(utskriftsmapp, "kumulativ","_", variabler[i],"_",paste0(max(px_data_frame$månad)), "_", paste0(unique(px_data_frame$region)), ".png"), kumulativ_plot,width = 7,height=4, dpi=300, units = "in")
}