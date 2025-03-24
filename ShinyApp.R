library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

# Load Google Font
font_add_google("Lato")
showtext_auto()

#indlæs mens events fra rds
mens_events <- readRDS("SB_mens_events,rds")

#indlæs womens events fra rds
womens_events <- readRDS("SB_womens_events.rds")

ui <- fluidPage(
  titlePanel("StatsBomb Analyse - Kvinder vs. Mænd"),
  sidebarLayout(
    sidebarPanel(
      # Shared filter for gender
      selectInput("gender_choice", "Vælg køn:",
                  choices = c("Alle", "Men", "Women"))
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        # Landing page: Info tabel
        tabPanel("Info tabel",
                 value = "info",
                 tableOutput("event_table")
        ),
        # Skud tab
        tabPanel("Skud",
                 value = "skud",
                 selectInput("plot_choice_skud", "Vælg plot:",
                             choices = c(
                               "Skud og mål" = "shots_goals",
                               "Konverteringsrate" = "conversion_rate"
                             )),
                 plotOutput("plot_skud", height = "600px")
        ),
        # Frispark tab
        tabPanel("Frispark",
                 value = "frispark",
                 selectInput("plot_choice_frispark", "Vælg plot:",
                             choices = c(
                               "Frispark" = "fouls",
                               "Kort" = "cards"
                             )),
                 plotOutput("plot_frispark", height = "600px")
        ),
        # Afleveringer tab
        tabPanel("Afleveringer",
                 value = "afleveringer",
                 selectInput("plot_choice_afleveringer", "Vælg plot:",
                             choices = c(
                               "Afleveringsnøjagtighed" = "passing_accuracy",
                               "Gennemsnit afleveringer pr. kamp" = "avg_passes"
                             )),
                 plotOutput("plot_afleveringer", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Skud tab output
  output$plot_skud <- renderPlot({
    gender <- input$gender_choice
    if (input$plot_choice_skud == "shots_goals") {
      data <- both_conversions_long
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Category, y = Count, fill = Metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Mænd har flere skud og skorer mindre", x = "Køn", y = "Count") +
        theme_minimal() +
        scale_fill_manual(values = c("#649AFF", "#401152")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
      
    } else if (input$plot_choice_skud == "conversion_rate") {
      data <- both_conversions
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Category, y = Conversion_Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = paste0(round(Conversion_Percentage, 1), "%")),
                  vjust = -0.5, size = 6, color = "black") +
        labs(title = "Kvinder har en højere konverteringsrate på skud", x = "Køn", y = "Konverteringsprocent") +
        theme_minimal() +
        scale_fill_manual(values = c("#649AFF", "#401152")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    }
  })
  
  # Frispark tab output
  output$plot_frispark <- renderPlot({
    gender <- input$gender_choice
    if (input$plot_choice_frispark == "fouls") {
      data <- foul_data
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Category, y = Fouls_Committed, fill = Category)) +
        geom_bar(stat = "identity", width = 0.5) +
        labs(title = "Frispark begåede af kvinder og mænd", x = "Køn", y = "Antal frispark") +
        theme_minimal() +
        scale_fill_manual(values = c("#649AFF", "#401152")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
      
    } else if (input$plot_choice_frispark == "cards") {
      data <- cards_combined
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Card_Type, y = Count, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Mænd får flere kort i alle kategorier", x = "Kort type", y = "Antal kort") +
        theme_minimal() +
        scale_fill_manual(values = c("#649AFF", "#401152")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    }
  })
  
  # Afleveringer tab output
  output$plot_afleveringer <- renderPlot({
    gender <- input$gender_choice
    if (input$plot_choice_afleveringer == "passing_accuracy") {
      data <- pass_acc_combined
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Category, y = Percentage, fill = Var1)) +
        geom_bar(stat = "identity", position = "stack", width = 0.5) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                  position = position_stack(vjust = 0.5), color = "white", size = 5) +
        labs(title = "Mænds afleveringer er mere præcise", x = "Køn", y = "Procent (%)") +
        theme_minimal() +
        scale_fill_manual(values = c("#401152", "#649AFF")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
      
    } else if (input$plot_choice_afleveringer == "avg_passes") {
      data <- avg_passes_combined
      if (gender != "Alle") {
        data <- data %>% filter(Category == gender)
      }
      
      ggplot(data, aes(x = Category, y = Avg_Passes_Per_Match, fill = Category)) +
        geom_bar(stat = "identity", width = 0.5) +
        labs(title = "Gennemsnitlige afleveringer pr. kamp (Mænd vs. Kvinder)",
             x = "Køn", y = "Gennemsnit") +
        theme_minimal() +
        scale_fill_manual(values = c("#649AFF", "#401152")) +
        theme(text = element_text(family = "Lato", size = 16),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    }
  })
  
  # Event table output for Info tabel tab
  output$event_table <- renderTable({
    
    # --- MENS PASSES ---
    mens_passes <- mens_events %>%
      filter(match_id %in% selected_games) %>%
      filter(type.name == "Pass" & play_pattern.name == "Regular Play") %>%
      mutate(pass.outcome.name = replace(pass.outcome.name, is.na(pass.outcome.name), "Accurate")) %>%
      filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>%
      mutate(pass.outcome.name = ifelse(pass.outcome.name %in% c("Out", "Pass Offside"), "Incomplete", pass.outcome.name))
    
    men_pass_count <- as.data.frame(table(mens_passes$pass.outcome.name))
    colnames(men_pass_count) <- c("Event", "Count")
    men_pass_count$Category <- "Men"
    men_pass_count$Event <- ifelse(men_pass_count$Event == "Accurate", "Accurate Pass", "Incomplete Pass")
    
    # Other men events
    men_evt_count <- mens_events %>%
      filter(match_id %in% selected_games) %>%
      filter(type.name %in% c("Shot", "Foul Committed", "Offside")) %>%
      mutate(Event = type.name) %>%
      count(Event, name = "Count") %>%
      mutate(Category = "Men")
    
    men_evt_count <- bind_rows(men_evt_count, men_pass_count)
    
    # --- WOMEN PASSES ---
    womens_passes <- womens_events %>%
      filter(type.name == "Pass" & play_pattern.name == "Regular Play") %>%
      mutate(pass.outcome.name = replace(pass.outcome.name, is.na(pass.outcome.name), "Accurate")) %>%
      filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>%
      mutate(pass.outcome.name = ifelse(pass.outcome.name %in% c("Out", "Pass Offside"), "Incomplete", pass.outcome.name))
    
    wmn_pass_count <- as.data.frame(table(womens_passes$pass.outcome.name))
    colnames(wmn_pass_count) <- c("Event", "Count")
    wmn_pass_count$Category <- "Women"
    wmn_pass_count$Event <- ifelse(wmn_pass_count$Event == "Accurate", "Accurate Pass", "Incomplete Pass")
    
    # Other women events
    wmn_evt_count <- womens_events %>%
      filter(type.name %in% c("Shot", "Foul Committed", "Offside")) %>%
      mutate(Event = type.name) %>%
      count(Event, name = "Count") %>%
      mutate(Category = "Women")
    
    wmn_evt_count <- bind_rows(wmn_evt_count, wmn_pass_count)
    
    # Combine all
    evt_count <- bind_rows(men_evt_count, wmn_evt_count)
    
    # Wide format
    evt_count_wide <- pivot_wider(evt_count,
                                  names_from = Category,
                                  values_from = Count)
    
    # Calculate pass totals
    total_men_passes <- sum(evt_count$Count[evt_count$Category == "Men" &
                                              evt_count$Event %in% c("Accurate Pass", "Incomplete Pass")])
    total_wmn_passes <- sum(evt_count$Count[evt_count$Category == "Women" &
                                              evt_count$Event %in% c("Accurate Pass", "Incomplete Pass")])
    
    # Convert pass counts to %
    evt_count_wide <- evt_count_wide %>%
      mutate(
        Men = ifelse(Event %in% c("Accurate Pass", "Incomplete Pass"),
                     paste0(round((as.numeric(Men) / total_men_passes) * 100, 1), "%"),
                     Men),
        Women = ifelse(Event %in% c("Accurate Pass", "Incomplete Pass"),
                       paste0(round((as.numeric(Women) / total_wmn_passes) * 100, 1), "%"),
                       Women)
      )
    
    # ---- Add Injury per Foul Row ----
    injury_merge <- merge(injuries, foul_data, by = "Category")
    
    injury_pct <- injury_merge %>%
      mutate(pct = round((Count / Fouls_Committed) * 100, 1)) %>%
      select(Category, pct)
    
    injury_pct_wide <- pivot_wider(injury_pct, names_from = Category, values_from = pct)
    
    injury_row <- data.frame(
      Event = "Injury per Foul (%)",
      Men = ifelse(!is.na(injury_pct_wide$Mænd), paste0(injury_pct_wide$Mænd, "%"), NA),
      Women = ifelse(!is.na(injury_pct_wide$Kvinder), paste0(injury_pct_wide$Kvinder, "%"), NA)
    )
    
    evt_count_wide <- bind_rows(evt_count_wide, injury_row)
    
    # ---- Add Card per Foul Row ----
    cards_summary <- cards_combined %>%
      mutate(Category = recode(Category, "Men" = "Mænd", "Women" = "Kvinder")) %>%
      group_by(Category) %>%
      summarise(Cards = sum(Count))
    
    foul_data_local <- foul_data %>%
      mutate(Category = recode(Category, "Men" = "Mænd", "Women" = "Kvinder"))
    
    card_pct <- merge(cards_summary, foul_data_local, by = "Category") %>%
      mutate(pct = round((Cards / Fouls_Committed) * 100, 1)) %>%
      select(Category, pct)
    
    card_pct_wide <- pivot_wider(card_pct, names_from = Category, values_from = pct)
    
    card_row <- data.frame(
      Event = "Card per Foul (%)",
      Mænd = if ("Mænd" %in% colnames(card_pct_wide)) paste0(card_pct_wide$Mænd, "%") else NA,
      Kvinder = if ("Kvinder" %in% colnames(card_pct_wide)) paste0(card_pct_wide$Kvinder, "%") else NA
    )
    
    # Rename columns to Danish
    colnames(evt_count_wide) <- c("Event", "Mænd", "Kvinder")
    
    evt_count_wide <- bind_rows(evt_count_wide, card_row)
    
    # Return final result
    evt_count_wide
  })
  
}

shinyApp(ui = ui, server = server)
