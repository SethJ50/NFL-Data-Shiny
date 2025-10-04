library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(nflreadr)
library(nflplotR)
library(dplyr)
library(tidyverse)
library(ggrepel)

pbp_data <- load_pbp(2025)

ui <- dashboardPage(
  dashboardHeader(title = "NFL Data Shiny", disable = FALSE),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      id = "tabs",
      menuItem("Pace of Play", tabName = "paceOfPlayTab", icon = NULL),
      menuItem("RB Expected Fantasy Points", tabName = "rbXfpTab", icon = NULL),
      menuItem("WR Expected Fantasy Points", tabName = "wrXfpTab", icon = NULL),
      menuItem("Rushing Defense", tabName = "rushDefenseTab", icon = NULL),
      menuItem("Receiving Defense", tabName = "receivingDefenseTab", icon = NULL)
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .main-header {
        background-color: #0C234B !important;  /* same as sidebar */
      }
      
      .main-header .logo {
        color: white !important;               /* header title text color */
        font-weight: bold;
        font-size: 20px;
        background-color: #0C234B !important;
      }
      
      .main-header .navbar {
        background-color: #0C234B !important; /* optional: match navbar area */
      }
  
      .main-sidebar {
        background-color: #0C234B !important;
        height: 100vh !important;
        position: fixed !important;
        overflow-y: auto;
        overflow-x: auto;
      }
      
      .content-wrapper {
        background-color: white;
        width: 100vw;
        height: 100vh;
        overflow-y: auto;
        overflow-x: auto;
      }
    ")),
    tabItems(
      tabItem(
        tabName = "paceOfPlayTab",
        h2("Pace of Play"),
        fluidRow(
          column (
            width = 3,
            card(
              h4("Filters")
            )
          ),
          column (
            width = 7,
            card(
              div(
                style = "
                  width: 75%;
                  margin-left: auto !important;
                  margin-right: auto !important;
                ",
                plotOutput("pacePlot", width = "100%", height = "600px")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "rbXfpTab",
        h2("RB Expected Fantasy Points"),
        fluidRow(
          column (
            width = 3,
            card(
              h4("Filters"),
              numericInput(
                inputId = "rbXfp_lastNWeeks",
                label = "Last N Weeks",
                value = max(pbp_data$week),
                min = 1,
                max = 17
              ),
              numericInput(
                inputId = "rbXfp_minAdjOpp",
                label = "Min. Adj. Opps / Gm.",
                value = 14,
                min = 1
              ),
              actionButton(
                inputId = "rbXfp_updatePlot",
                label = "Update Plot"
              )
            )
          ),
          column (
            width = 7,
            card(
              div(
                style = "
                  width: 75%;
                  margin-left: auto !important;
                  margin-right: auto !important;
                ",
                plotOutput("xfpRushPlot", width = "100%", height = "600px")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "wrXfpTab",
        h2("WR Expected Fantasy Points"),
        fluidRow(
          column (
            width = 3,
            card(
              h4("Filters"),
              numericInput(
                inputId = "wrXfp_lastNWeeks",
                label = "Last N Weeks",
                value = max(pbp_data$week),
                min = 1,
                max = 17
              ),
              numericInput(
                inputId = "wrXfp_minTargets",
                label = "Min. Targets / Gm.",
                value = 5,
                min = 1
              ),
              actionButton(
                inputId = "wrXfp_updatePlot",
                label = "Update Plot"
              )
            )
          ),
          column (
            width = 7,
            card(
              div(
                style = "
                  width: 75%;
                  margin-left: auto !important;
                  margin-right: auto !important;
                ",
                plotOutput("xfpRecPlot", width = "100%", height = "600px")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "rushDefenseTab",
        h2("Rushing Defense"),
        fluidRow(
          column (
            width = 3,
            card(
              h4("Filters"),
              numericInput(
                inputId = "rushDef_lastNWeeks",
                label = "Last N Weeks",
                value = max(pbp_data$week),
                min = 1,
                max = 17
              ),
              actionButton(
                inputId = "rushDef_updatePlot",
                label = "Update Plot"
              )
            )
          ),
          column (
            width = 7,
            card(
              div(
                style = "
                  width: 75%;
                  margin-left: auto !important;
                  margin-right: auto !important;
                ",
                plotOutput("rushDefPlot", width = "100%", height = "600px")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "receivingDefenseTab",
        h2("Receiving Defense"),
        fluidRow(
          column (
            width = 3,
            card(
              h4("Filters"),
              numericInput(
                inputId = "recDef_lastNWeeks",
                label = "Last N Weeks",
                value = max(pbp_data$week),
                min = 1,
                max = 17
              ),
              actionButton(
                inputId = "recDef_updatePlot",
                label = "Update Plot"
              )
            )
          ),
          column (
            width = 7,
            card(
              div(
                style = "
                  width: 75%;
                  margin-left: auto !important;
                  margin-right: auto !important;
                ",
                plotOutput("recDefPlot", width = "100%", height = "600px")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$tabs, {
    if(input$tabs == "paceOfPlayTab") {
      serverPaceOfPlay(input, output, session)
    } else if(input$tabs == "rbXfpTab") {
      serverRbXfp(input, output, session)
    } else if(input$tabs == "wrXfpTab"){
      serverWrXfp(input, output, session)
    } else if(input$tabs == "rushDefenseTab") {
      serverRushDef(input, output, session)
    } else if(input$tabs == "receivingDefenseTab") {
      serverRecDef(input, output, session)
    }
  })
}



teams <- load_teams()

pbp_off <- pbp_data %>% 
  left_join(teams, by = c('posteam' = 'team_abbr'))

pbp_def <- pbp_data %>% 
  left_join(teams, by = c('defteam' = 'team_abbr'))

expected_fantasy_points <- load_ff_opportunity(most_recent_season(), "weekly", "latest") %>% 
  left_join(teams, by = c('posteam' = 'team_abbr'))

serverPaceOfPlay <- function(input, output, session) {
  neutralScriptPace <- pbp_off %>% 
    filter(wp > 0.2 & wp < 0.8 & down <= 3 & qtr <= 4 & half_seconds_remaining > 120) %>% 
    group_by(posteam) %>% 
    summarise(
      mean_pass = mean(pass),
      plays = n(),
      team = last(posteam),
      team_color = last(team_color)
    ) %>% 
    arrange(-mean_pass)
  
  output$pacePlot <- renderPlot({
    neutralScriptPace %>% 
      ggplot(aes(x = reorder(posteam, mean_pass) , y = mean_pass)) +
      geom_hline(yintercept = mean(neutralScriptPace$mean_pass), color = 'red', linetype = "dashed", alpha = 0.5) +
      geom_bar(stat = "identity", width = 0.5, fill = neutralScriptPace$team_color) +
      geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.75) +
      labs(
        x = "Team",
        y = "Pass Rate",
        title = "Neutral Script Pass Rate",
        subtitle = "Team Win % between 20% and 80%"
      ) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 8, hjust = 0.5)
      ) +
      coord_flip()
  })
}

serverRbXfp <- function(input, output, session) {
  
  plot_data <- eventReactive(input$rbXfp_updatePlot, {
    currWeek <- max(pbp_data$week)
    lastNWeeks <- input$rbXfp_lastNWeeks
    minAdjOpp <- input$rbXfp_minAdjOpp
    
    minWeek <- ifelse(currWeek - lastNWeeks < 1, 1, (currWeek - lastNWeeks) + 1)
    
    adj_opps <- pbp_data %>% 
      filter(down <= 4, play_type == 'pass' | play_type == 'run', week >= minWeek) %>%
      group_by(fantasy_player_id) %>% 
      summarise(
        fp_name = last(fantasy_player_name),
        rushAtt = sum(rush, na.rm = TRUE),
        targets = sum(pass, na.rm = TRUE)
      ) %>% 
      filter(fantasy_player_id != 'NA')
    
    xfp_plot_data <- expected_fantasy_points %>% 
      filter(position == "RB", week >= minWeek) %>% 
      group_by(full_name) %>% 
      summarise(
        xfp = sum(total_fantasy_points_exp),
        fp = sum(total_fantasy_points),
        weeks_played = n(),
        xfp_pg = round(xfp / weeks_played, 2),
        fp_pg = round(fp / weeks_played, 2),
        team = last(posteam),
        player_id = last(player_id)
      ) %>% 
      left_join(adj_opps, by = c('player_id' = 'fantasy_player_id')) %>% 
      mutate(
        adj_opps = rushAtt + 2 * targets,
        adj_opps_pg = round(adj_opps / weeks_played, 2)
      ) %>% 
      filter(adj_opps_pg >= minAdjOpp)
    
    list(
      data = xfp_plot_data,
      minWeek = minWeek,
      currWeek = currWeek
    )
  })
  
  output$xfpRushPlot <- renderPlot({
    plot_info <- plot_data()
    df <- plot_info$data
    
    ggplot(df, aes(x = xfp_pg, y = fp_pg, name = full_name)) + 
    geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed", alpha = 0.6) +
    geom_text_repel(aes(label = full_name), nudge_x = 0.2, size = 3, segment.color = "grey50") +
    labs(
      x = "Expected Fantasy Points / Game",
      y = "Actual Fantasy Points / Game",
      title = "RB XFP vs Actual FP",
      subtitle = paste0("Weeks ", plot_info$minWeek, " to ", plot_info$currWeek)
    ) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  })
}

serverWrXfp <- function(input, output, session) {
  
  plot_data <- eventReactive(input$wrXfp_updatePlot, {
    currWeek <- max(pbp_data$week)
    lastNWeeks <- input$wrXfp_lastNWeeks
    minTargets <- input$wrXfp_minTargets
    
    minWeek <- ifelse(currWeek - lastNWeeks < 1, 1, (currWeek - lastNWeeks) + 1)
    
    adj_opps <- pbp_data %>% 
      filter(down <= 4, play_type == 'pass' | play_type == 'run', week >= minWeek) %>%
      group_by(fantasy_player_id) %>% 
      summarise(
        fp_name = last(fantasy_player_name),
        rushAtt = sum(rush, na.rm = TRUE),
        targets = sum(pass, na.rm = TRUE)
      ) %>% 
      filter(fantasy_player_id != 'NA')
    
    xfp_plot_data <- expected_fantasy_points %>% 
      filter(position == "WR" | position == "TE", week >= minWeek) %>% 
      group_by(full_name) %>% 
      summarise(
        xfp = sum(total_fantasy_points_exp),
        fp = sum(total_fantasy_points),
        weeks_played = n(),
        xfp_pg = round(xfp / weeks_played, 2),
        fp_pg = round(fp / weeks_played, 2),
        team = last(posteam),
        player_id = last(player_id)
      ) %>% 
      left_join(adj_opps, by = c('player_id' = 'fantasy_player_id')) %>% 
      mutate(
        adj_opps = rushAtt + 2 * targets,
        adj_opps_pg = round(adj_opps / weeks_played, 2),
        targets_pg = targets / weeks_played
      ) %>% 
      filter(targets_pg >= minTargets)
    
    list(
      data = xfp_plot_data,
      minWeek = minWeek,
      currWeek = currWeek
    )
  })
  
  output$xfpRecPlot <- renderPlot({
    plot_info <- plot_data()
    df <- plot_info$data
    
    ggplot(df, aes(x = xfp_pg, y = fp_pg, name = full_name)) + 
      geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.75) +
      geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed", alpha = 0.6) +
      geom_text_repel(aes(label = full_name), nudge_x = 0.2, size = 3, segment.color = "grey50") +
      labs(
        x = "Expected Fantasy Points / Game",
        y = "Actual Fantasy Points / Game",
        title = "WR XFP vs Actual FP",
        subtitle = paste0("Weeks ", plot_info$minWeek, " to ", plot_info$currWeek)
      ) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
      )
  })
}

makeHalfPprData <- function() {
  rem_NA <- pbp_def %>% 
    mutate_at(c('rushing_yards', 'receiving_yards', 'complete_pass',
                'pass_touchdown', 'rush_touchdown', 'fumble_lost'), ~replace_na(.,0))
  
  pbp_data_half_ppr <- rem_NA %>% 
    mutate(halfPPR = ((rushing_yards + receiving_yards) / 10) + (complete_pass * 0.5) +
             ((pass_touchdown + rush_touchdown) * 6) + (fumble_lost*-2))
  
  return(pbp_data_half_ppr)
}

serverRushDef <- function(input, output, session) {
  
  plot_data <- eventReactive(input$rushDef_updatePlot, {
    currWeek <- max(pbp_data$week)
    lastNWeeks <- input$rushDef_lastNWeeks
    
    minWeek <- ifelse(currWeek - lastNWeeks < 1, 1, (currWeek - lastNWeeks) + 1)
    
    pbp_data_halfppr <- makeHalfPprData()
    
    res_data <- pbp_data_halfppr %>% 
      filter(qtr <= 4 & down <= 4 & play_type == 'run' & rush == 1 & week >= minWeek) %>% 
      group_by(defteam) %>% 
      summarise(
        rushAtt = n(),
        totFantPts = sum(halfPPR),
        ptsPerAtt = totFantPts / rushAtt,
        totEPA = sum(epa),
        epaPerAtt = totEPA / rushAtt,
        team = last(defteam),
        team_color = last(team_color)
      ) %>% 
      arrange(-ptsPerAtt)
    
    list(
      data = res_data,
      minWeek = minWeek,
      currWeek = currWeek
    )
  })
  
  output$rushDefPlot <- renderPlot({
    plot_info <- plot_data()
    df <- plot_info$data
    
    ggplot(df, aes(x = epaPerAtt, y = ptsPerAtt)) +
      geom_hline(yintercept = mean(df$ptsPerAtt, na.rm = TRUE), color = 'red', linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = mean(df$epaPerAtt, na.rm = TRUE), color = 'red', linetype = "dashed", alpha = 0.5) +
      geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.75) +
      labs(
        x = "EPA Per Rush Attempt",
        y = "Fantasy Points Per Rush Attempt",
        title = "EPA vs. Fantasy Points Per Rush Attempt",
        subtitle = paste0("Weeks ", plot_info$minWeek, " to ", plot_info$currWeek, " (Half PPR)")
      ) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
      )
  })
}

serverRecDef <- function(input, output, session) {
  
  plot_data <- eventReactive(input$recDef_updatePlot, {
    currWeek <- max(pbp_data$week)
    lastNWeeks <- input$recDef_lastNWeeks
    
    minWeek <- ifelse(currWeek - lastNWeeks < 1, 1, (currWeek - lastNWeeks) + 1)
    
    pbp_data_halfppr <- makeHalfPprData()
    
    res_data <- pbp_data_halfppr %>% 
      filter(qtr <= 4 & down <= 4 & play_type == 'pass' & pass == 1 & week >= minWeek) %>% 
      group_by(defteam) %>% 
      summarise(
        passAtt = n(),
        totFantPts = sum(halfPPR),
        ptsPerAtt = totFantPts / passAtt,
        totEPA = sum(epa),
        epaPerAtt = totEPA / passAtt,
        team = last(defteam),
        team_color = last(team_color)
      ) %>% 
      arrange(-ptsPerAtt)
    
    list(
      data = res_data,
      minWeek = minWeek,
      currWeek = currWeek
    )
  })
  
  output$recDefPlot <- renderPlot({
    plot_info <- plot_data()
    df <- plot_info$data
    
    ggplot(df, aes(x = epaPerAtt, y = ptsPerAtt)) +
      geom_hline(yintercept = mean(df$ptsPerAtt, na.rm = TRUE), color = 'red', linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = mean(df$epaPerAtt, na.rm = TRUE), color = 'red', linetype = "dashed", alpha = 0.5) +
      geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.75) +
      labs(
        x = "EPA Per Pass Attempt",
        y = "Fantasy Points Per Pass Attempt",
        title = "EPA vs. Fantasy Points Per Pass Attempt",
        subtitle = paste0("Weeks ", plot_info$minWeek, " to ", plot_info$currWeek, " (Half PPR)")
      ) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
      )
  })
}

shinyApp(ui, server)