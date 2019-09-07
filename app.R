# Zrób dashboard podobny do tego: https://kjedrzejewski.shinyapps.io/podyplomowe2/
# - Filtrowanie po kolorze włosów (jeżeli wybrano kilka kolorów, pokazanie wszystkich bohaterów którzy mają przynajmniej jeden z nich)
# - Filtrowanie po gatunku (jeżeli wybrano kilka gatunków, pokazanie wszystkich bohaterów którzy mają przynajmniej jeden z nich)
# - Scatterplot:
#   - X - wzrost
#   - Y - rodzinna planeta
#   - kolor - płeć (zwróćcie uwagę jakie płcie są wymieniane w przykładzie, jest kilka przypadków, które płci nie ma, ale też są na wykresie)
#   - domyślnym trybem jest zaznaczanie
# - Domyślnie tabelka pod wykresem pokazuje wszystkich bohaterów
#   - ale po zaznaczeniu punktów na scatterplocie, ta tabela zostaje ograniczona tylko do wierszy odpowiadających tym punktom
#   - w tabeli na raz można zaznaczyć tylko jeden wiersz
# - Po kliknięciu wiersza w tabelce, na panelu bocznym, pod wyszukiwarkami, pojawiają się szczegółowe informacje o danym bohaterze
# - Dane o bohaterach Gwiezdnym Wojen możesz wziąć z datasetu starwars z pakietu dplyr (?starwars)



library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)

######

# uzupełnijmy trochę brakujących informacji
starwars_data = starwars %>%
  mutate(
    height = case_when(
      name == 'Finn' ~ as.integer(178),
      name == 'Rey' ~ as.integer(170),
      name == 'Poe Dameron' ~ as.integer(172),
      name == 'BB8' ~ as.integer(67),
      name == 'Captain Phasma' ~ as.integer(200),
      TRUE ~ height
    ),
    mass = case_when(
      name == 'Finn' ~ 73,
      name == 'Rey' ~ 54,
      name == 'Poe Dameron' ~ 80,
      name == 'BB8' ~ 18,
      name == 'Captain Phasma' ~ 76,
      TRUE ~ mass
    )
  )

### uzupełniam dane, gdy wartości nieznane
starwars_data2 = starwars_data %>%
  tibble::rownames_to_column(var = 'id') %>%
  mutate(hair_color = ifelse(is.na(hair_color), 'not applicable', hair_color),
         species = ifelse(is.na(species), 'unknown', species),
         gender = ifelse(is.na(gender), 'not applicable', gender)
  )

#początkowa lista kolorów włosów i gatunków
hair_color_all =  starwars_data2 %>%
  select(hair_color) %>%
  separate_rows(., hair_color,sep=",\\s+") %>%
  distinct() %>%
  arrange(hair_color) %>%
  .$hair_color

species_all = starwars_data2 %>%
  select(species) %>%
  distinct() %>%
  arrange(species) %>%
  .$species


ui <- fluidPage(
  sidebarLayout
  (
    sidebarPanel
    (
      selectInput('hair_color', 'Kolor włosów', hair_color_all, multiple = TRUE),
      selectInput('species', 'Gatunek', species_all, multiple = TRUE),
      htmlOutput('char_info')
    ),
    mainPanel
    (
      plotlyOutput('plot'),
      dataTableOutput('tab')
    )
  )
)

srv <- function(input, output, session)
{
  
  hair_color_all = reactive({
    dane = NULL
    
    #wybieramy tylko kolory włosów odpowiadające wybranym gatunkom
    if(length(input$species>0)){ 
      dane = starwars_data2 %>%
        filter(species %in% input$species) %>%
        select(hair_color) %>%
        separate_rows(., hair_color,sep=",\\s+") %>%
        distinct() %>%
        arrange(hair_color) %>%
        .$hair_color
    } else{
      dane = starwars_data2 %>%
        select(hair_color) %>%
        separate_rows(., hair_color,sep=",\\s+") %>%
        distinct() %>%
        arrange(hair_color) %>%
        .$hair_color
    }
    
    dane
  })
  
  
  species_all = reactive({
    dane = NULL
    
    #wybieramy tylko gatunki odpowiadające wybranym kolorom włosów
    if(length(input$hair_color)>0){
      dane = starwars_data2 %>%
        filter(hair_color %in% input$hair_color) %>%
        select(species) %>%
        distinct() %>%
        arrange(species) %>%
        .$species
    } else {
      dane = starwars_data2 %>%
        select(species) %>%
        distinct() %>%
        arrange(species) %>%
        .$species
    }
    
    dane
  })
  
  #odświeżanie listy kolorów włosów i gatunków
  observe({
    updateSelectInput(session, inputId = 'species', choices = species_all(), selected = input$species)
  })
  
  observe({
    updateSelectInput(session, inputId = 'hair_color', choices = hair_color_all(), selected = input$hair_color)
  })
  
  #informacje o postaciach o danych włosach i gatunkach  
  char_info_data = reactive({
    dane = starwars_data2
    
    if(length(input$hair_color)>0){
      dane = dane %>%
        filter(hair_color %in% input$hair_color)
    }
    
    if(length(input$species)>0){
      dane = dane %>%
        filter(.$species %in% input$species)
    }
    
    dane
  })
  
  
  #wykres postaci o danych włosach i gatunkach
  output$plot = renderPlotly({
    plot_ly(
      char_info_data(),
      source = 'scatter') %>%  
      add_markers(
        x = ~height,
        y = ~homeworld,
        color = ~gender,
        key = ~id
      )%>%
      layout(dragmode =  "select") #można zaznaczać dane
  })
  
  #zaznaczanie danych z wykresu
  selected_data = reactive({
    sel_data = NULL
    
    ed = event_data('plotly_selected', source = 'scatter')
    
    if(!is.null(ed)){
      sel_data = char_info_data() %>%
        filter(id %in% ed$key) #filtrujemy po zaznaczonych id
      
    } else {
      sel_data = char_info_data()
    }
    
    sel_data %>%
      select(-1) %>% #usuwamy stare id
      mutate(hair_color = ifelse(hair_color == 'not applicable', '', hair_color), 
             no_films = lengths(films), no_vehicles = lengths(vehicles), 
             no_starships = lengths(starships)) %>%
      tibble::rownames_to_column(var = 'id') #dajemy nowe id, biorąc pod uwagę tylko wyświetlanych
    
  })
  
  #tabela postaci wybranych z wykresu
  output$tab = renderDataTable(
    selected_data() %>% select(id, name, height, mass, hair_color, birth_year, no_films, no_vehicles, no_starships ),
    colnames = c("ID", "Imię", "Wzrost", "Waga", "Włosy", "Rok urodzenia", "L. filmów", "L. pojazdów", "L. statków"),
    rownames = FALSE,
    selection = 'single'
  )
  
  #dane kilkniętej w tabeli postaci
  clicked_data = reactive({
    dane = NULL
    tekst = NULL
    
    sel = input$tab_rows_selected #zwraca id klikniętego wiersza
    
    if(length(sel)){
      dane = selected_data() %>% filter(id == sel) #bierzemy selected_data, bo tam są te nowe id
      
      # dane = starwars_data2 %>% filter(id == 1)
      
      #tworzymy listy i konwertujemy na tekst
      pojazdy <-  paste(paste0('<li>', unlist(dane$vehicles),'</li>'), collapse = '')
      statki <- paste(paste0('<li>', unlist(dane$starships),'</li>'), collapse = '')
      filmy <- paste(paste0('<li>', unlist(dane$films),'</li>'), collapse = '')
      
      #tworzymy tekst w formie HTML
      tekst = paste("Imię: ","<b>", dane$name, "</b><br>","\nPłeć: ", "<b>",dane$gender, "</b><br>",
                    '\nUrodziny(a): ',"<b>",dane$birth_year, ' BBY', "</b><br>",
                    '\nPlaneta: ', "<b>",dane$homeworld,"</b><br>", '\nGatunek: ', "<b>",dane$species,"</b><br>",
                    '\nWzrost: ', "<b>", dane$height, ' cm', "</b><br>",'\nWaga: ', "<b>",dane$mass,' kg', "</b><br>",
                    '\nWłosy: ', "<b>",dane$hair_color,"</b><br>", '\nSkóra: ', "<b>",dane$skin_color,"</b><br>",
                    '\nOczy: ', "<b>", dane$eye_color,"</b><br><br>", 'Pojazdy: ',"<b>", pojazdy, "</b><br>",
                    '\n\nStatki: ',"<b>", statki, "</b><br>", '\n\nFilmy: ', "<b>", filmy, "</b>")
      
    }
    
    tekst
  })
  
  #wyrzucamy tabelkę z informacjami o klikniętej postaci
  output$char_info = renderUI({
    
    HTML(clicked_data() )
  })
  
}

shinyApp(ui, srv)

