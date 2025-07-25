# load in packages 

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reactable)
library(bslib)
install.packages("shinythemes")
library(shinythemes)

#upload game by game YT data
folder_path <- "/Users/isabellaholtz/Desktop/tennesseYTdata"
file_paths <- list.files(folder_path, pattern = '\\.csv$', full.names = TRUE)

#combine data into one file 
dfs <- lapply(file_paths, function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  df$GameFile <- basename(path)
  return(df)
})
combined_data1 <- bind_rows(dfs)
combined_data1 <- combined_data1 %>%
  # delete extra times in pitcher team column data
  mutate(PitcherTeam = str_trim(str_remove(PitcherTeam, "\\s*\\[.*\\]")))

# create new data frame with movement summary columns 
movement_sum <- combined_data1 %>%
  group_by(Pitcher, TaggedPitchType) %>%
  summarise(
    HorzBreak = mean(HorzBreak, na.rm = TRUE),
    VertBreak = mean(InducedVertBreak, na.rm=TRUE),
    Velocity = mean(RelSpeed, na.rm=TRUE),
    HorzBreakRange = max(HorzBreak, na.rm = TRUE) - min(HorzBreak, na.rm=TRUE),
    VertBreakRange = max(InducedVertBreak, na.rm=TRUE) - min(InducedVertBreak, na.rm=TRUE),
    AvgHorzBreak = mean(HorzBreak, na.rm=TRUE),
    AvgVertBreak = mean(InducedVertBreak, na.rm=TRUE),
    AvgVelocity = mean(RelSpeed, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  mutate( 
    TotalBreak = sqrt(HorzBreak^2 + VertBreak^2),
    ScaledBreak = as.numeric(scale(TotalBreak)),
    ScaledVelocity = as.numeric(scale(AvgVelocity)),
    CompositeScore = 0.5*ScaledBreak + 0.5 * ScaledVelocity
  )
write_csv(movement_sum, "/Users/isabellaholtz/Desktop/CSE 2025/Pitcher_Movement/movement_sum.csv")

# load in yakertech pitching data 
pitch_data <- read_csv("/Users/isabellaholtz/Desktop/CSE 2025/Pitcher_Movement/combined_data1.csv", show_col_types = FALSE)
pitch_data <- pitch_data %>%
  filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
# load in pitching data with break columns 
movement_sum <- read_csv("/Users/isabellaholtz/Desktop/CSE 2025/Pitcher_Movement/movement_sum.csv", show_col_types = FALSE)

# create function to make tier labels
get_tier <- function(value, ref_vector) {
  case_when(
    value >= quantile(ref_vector, 0.9, na.rm=TRUE) ~ 'Elite',
    value >= quantile(ref_vector, 0.7, na.rm= TRUE) ~ 'Above Average',
    value >= quantile(ref_vector, 0.4, na.rm=TRUE) ~ 'Average',
    value >= quantile(ref_vector, 0.1, na.rm=TRUE) ~ 'Below Average',
    TRUE ~ "Poor"
  )
}

#create function to make tier colors
get_tier_color <- function(tier) {
  case_when(
    tier == "Elite" ~ "#1a9850",
    tier == "Above Average" ~ "#66bd63",
    tier == "Average" ~ "#fdae61",
    tier == "Below Average" ~ "#f46d43",
    tier == "Poor" ~ "#d73027",
    TRUE ~ "white"
  )
}

columnHeaderStyle <- list(whiteSpace = 'normal', lineHeight = '1.4em') # create style for tables

# define UI 
ui <- fluidPage(
  theme = bs_theme(bootswatch = 'flatly', base_font = font_google('Playfair Display')),
  titlePanel("The Movement Matrix"), 
  
  # create all the dashboard panels 
  
  tabsetPanel(
    tabPanel("Movement Plot",
             #create sidebar with filters
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Options"),
                 selectInput("team", "Select Team", choices = sort(unique(pitch_data$PitcherTeam))),
                 selectInput("color_by", "Color By", choices = c("Pitch Type" = 'TaggedPitchType')),
                 uiOutput("Pitcher_selector"),
                 uiOutput("game_selector"),
                 uiOutput("hitter_selector"),
                 uiOutput("pitch_type_selector")
               ), # create movement profile on main screen
               mainPanel(
                 h4("Pitch Movement Scatter Plot"),
                 plotOutput("movement_plot", height = '500px')
               )
             )
    
    ),
             
    # create movement summary table and pitcher selector 
    tabPanel('Movement Summary',
             sidebarLayout(
               sidebarPanel(
                 h4("Select Pitcher"),
                 selectInput('summary_pitcher', "Pitcher", 
                             choices = sort(unique(movement_sum$Pitcher)))
               ),
               
               # add borders and extra space for aesthetics on table 
               mainPanel(
                 tags$div(
                   style = "margin-bottom: 15px;",
                   HTML("<strong> Legend:</strong><br>
                      <span style='background-color:#1a9850;color:white;padding:2px 6px;'>Elite</span> 
                      <span style='background-color:#66bd63;color:white;padding:2px 6px;'>Above Avg</span> 
                      <span style='background-color:#fdae61;color:white;padding:2px 6px;'>Avg</span> 
                      <span style='background-color:#f46d43;color:white;padding:2px 6px;'>Below Avg</span> 
                      <span style='background-color:#d73027;color:white;padding:2px 6px;'>Poor</span>")
                 ),
                 h4('Movement Summary'),
                 div(
                  reactableOutput('pitch_overview'),
                  style = 'margin-bottom: 75px;'
                 )
               )
             )
             
    ),
    # create heatmap tab on dashboard
    tabPanel("Pitch Location Heatmap", 
             sidebarLayout(
               sidebarPanel(
                 # add all the filters on sidebar
                 h4("Heatmap Filters"),
                 selectInput("heat_pitcher", "Select Pitcher", choices = sort(unique(pitch_data$Pitcher))),
                 selectInput("heat_batter_hand", "Batter Handedness",
                             choices = c("All", sort(unique(pitch_data$BatterSide))), selected = 'All'),
                 selectInput("heat_pitch_type", "Pitch Type",
                             choices = c("All", sort(unique(pitch_data$TaggedPitchType))), selected = 'All'),
                 selectInput('heat_batter', 'Batter',
                             choices = c('All', sort(unique(pitch_data$Batter))), selected = 'All'),
                 selectInput('heat_team', 'Opponent Team',
                             choices = c('All', sort(unique(pitch_data$BatterTeam))), selected = 'All')
               ),
               mainPanel(
                 h4("Strike Zone Heatmap"),
                 plotOutput("heatmap_plot", height = "500px", width = "500px")
               )
             )
    )
  )
)
   
  
            
# define server logic
server <- function(input, output, session) {
  
filtered_data <- reactive({
  req(input$team)
  pitch_data %>% filter(PitcherTeam == input$team)
})
  
# create pitcher selector tab
    output$Pitcher_selector <- renderUI({
        req(filtered_data())
        df<- filtered_data()
        selectInput("Pitcher", "Select Pitcher", choices = unique(df$Pitcher))
    })
    
#create game selector 
    output$game_selector <- renderUI({
      req(filtered_data(), input$Pitcher)
      
      df<- filtered_data() %>% filter(Pitcher == input$Pitcher)
      selectInput("Game", "Select Game", choices = c("All", sort(unique(df$Date))))
    })
    
#create hitter selector 
    output$hitter_selector <- renderUI({
      req(filtered_data(), input$Pitcher, input$Game)
      
      df<- filtered_data() %>%
        filter(Pitcher == input$Pitcher)
      
      if(input$Game != "All") {
        df <- df %>% filter(Date == input$Game)
      }
      selectInput("Hitter", "Select Hitter", choices = c("All", sort(unique(df$Batter))))
    })
    
# create pitch type selector 
    output$pitch_type_selector <- renderUI ({
      req(filtered_data(), input$Pitcher, input$Game)
      df<-filtered_data() %>%
        filter(Pitcher == input$Pitcher)
      
      if (input$Game != "All") {
        df <- df %>% filter(Date == input$Game)
      }
      
      selectInput("PitchType", "Select Pitch Type", choices = c("All", sort(unique(df$TaggedPitchType))))
    })
    
    # create pitch summary table with avg stats
    output$pitch_overview <- renderReactable({
      summary_data <- movement_sum %>%
        filter(Pitcher == input$summary_pitcher) %>%
        group_by(TaggedPitchType) %>%
        summarise(
          AvgHorzBreak = mean(HorzBreak, na.rm = TRUE),
          AvgVertBreak = mean(VertBreak, na.rm = TRUE),
          AvgHorzBreakRange = mean(HorzBreakRange, na.rm = TRUE),
          AvgVertBreakRange = mean(VertBreakRange, na.rm = TRUE),
          AvgVelo = mean(Velocity, na.rm = TRUE),
          AvgTotalBreak = mean(TotalBreak, na.rm = TRUE),
          AvgCompositeScore = mean(CompositeScore, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # add the tier colors to each of the stats in summary table 
      reactable(
        summary_data,
        columns = list(
          AvgHorzBreak = colDef(
            name = 'Average Horizontal Break',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$HorzBreak)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgVertBreak = colDef(
            name = 'Average Vertical Break',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$VertBreak)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgHorzBreakRange = colDef(
            name = 'Average Horizontal Break Range',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$HorzBreakRange)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgVertBreakRange = colDef(
            name = 'Average Vertical Break Range',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$VertBreakRange)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgVelo = colDef(
            name = 'Average Velocity',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$Velocity)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgTotalBreak = colDef(
            name = 'Average Total Break',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$TotalBreak)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          ),
          AvgCompositeScore = colDef(
            name = 'Average Composite Score',
            headerStyle = columnHeaderStyle,
            format = colFormat(digits = 3),
            style = function(value) {
              tier <- get_tier(value, movement_sum$CompositeScore)
              color <- get_tier_color(tier)
              list(background = color, color = 'white')
            }
          )
        ),
        defaultColDef = colDef(headerStyle = columnHeaderStyle),
        compact = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })
    
    #create heatmap
    output$heatmap_plot <- renderPlot({
      req(input$heat_pitcher,
          input$heat_batter_hand,
          input$heat_pitch_type,
          input$heat_batter, 
          input$heat_team)
      
      # add filters to heatmap
      df <- pitch_data %>%
        filter(Pitcher == input$heat_pitcher) %>%
        filter(
          input$heat_batter_hand == 'All' | BatterSide == input$heat_batter_hand,
          input$heat_pitch_type == 'All' | TaggedPitchType == input$heat_pitch_type,
          input$heat_batter == 'All' | Batter == input$heat_batter,
          input$heat_team == 'All' | BatterTeam == input$heat_team
        )
      if (nrow(df) == 0) return(NULL)
      
      # add strike zone to heat map 
      df_fixed <- df %>%
        bind_rows(
          tibble(PlateLocSide = c(-2.5, 2.5), PlateLocHeight = c(0,5))
        )
      
      # create heat map grid 
      ggplot(df_fixed, aes(x=PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes( fill = after_stat(density)),
          geom = 'raster',
          contour = FALSE, 
          n = 300
        ) +
        scale_fill_viridis_c(option = 'plasma', name = 'Density') +
        guides(fill = guide_colorbar(barwidth = 1, barheight = 15)) +
        annotate('rect', xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5,
                 color = 'white', fill = NA, linetype = 'dashed') +
        coord_fixed(xlim = c(-2.5,2.5), ylim = c(0,5)) +
        theme_void(base_size = 14) + 
        labs(
          title=paste("Pitch Location Heatmap:", input$heat_pitcher),
          x = "Horizontal Plate Location (Side)",
          y = "Vertical Plate Location (Height)"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = 'bold'),
          legend.position = 'right'
        )
    })
    
        
        # draw the movement profile for pitcher
    output$movement_plot <- renderPlot({
      req(filtered_data(), input$Pitcher)
      
      print("Starting plot rendering...")
      
      df <- filtered_data() %>% filter(Pitcher == input$Pitcher)
      
      if(!is.null(input$Game) && input$Game != "All") {
        df <- df %>% filter(Date == input$Game)
      }
      
      if(!is.null(input$Batter) && input$Batter != "All") {
        df <- df %>% filter(Batter == input$Batter)
      }
      
      if(!is.null(input$PitchType) && input$PitchType != "All") {
        df <- df %>% filter(TaggedPitchType == input$PitchType)
      }
      if (is.null(input$color_by) || !input$color_by %in% colnames(df)) {
        showNotification("Selected color variable is not available in the data", type = "error")
        return(NULL)
      }
     
      
      ggplot(df, aes(x = HorzBreak, y = InducedVertBreak, color = .data[[input$color_by]])) +
        geom_point(size = 3, alpha = 0.8) + 
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
        xlim(-20, 20) +
        ylim(-20, 20) + 
        theme_minimal () + 
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
          axis.title.y = element_text(size = 14, face = "bold", angle = 0, vjust = 0.5, hjust = 1, margin = margin(r = 15)),
          legend.title = element_blank()
        ) +
        labs(
          x = "Vertical Break", 
          y = "Horizontal Break", 
          title = paste("Pitch Movement -", input$Pitcher)
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank()
        )
    })
}
    
    

# Run the application 
shinyApp(ui = ui, server = server) 


