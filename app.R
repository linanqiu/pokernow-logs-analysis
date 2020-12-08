#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(shinycssloaders)

source('calcs.R')

options(spinner.color='#3c8dbc', spinner.type = 1)

datatable_default <- function(x, paging = FALSE, searching = FALSE, escape = FALSE, rownames = FALSE, ...) {
    x %>% mutate_if(is.numeric, signif, 5) %>% datatable(options = list(paging = paging, scrollX = TRUE, searching = searching, ...), escape = escape, rownames = rownames)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = 'PokerNow Analysis'),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
            fluidRow(
                box(title = 'File Upload', width = 4,
                    fileInput('input_log_file', label = 'Upload Your Own Log File', multiple = FALSE, accept = '.csv')
                ),
                box(title = 'Player Summary', width = 4,
                    dataTableOutput('tab_player_summary') %>% withSpinner()
                ),
                box(title = 'Game Details', width = 4,
                    dataTableOutput('tab_game_details') %>% withSpinner()
                )
            ),
            fluidRow(
                box(title = 'Player Stack Sizes', width = 12,
                    plotlyOutput('plot_levels') %>% withSpinner()
                )
            ),
            fluidRow(
                box(title = 'Full Logs', width = 12, collapsible = TRUE, collapsed = TRUE,
                    dataTableOutput('tab_logs_full')
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d <- reactive({
        f <- input$input_log_file
        
        if (is.null(f))
            return(read_csv('logs.csv'))
        
        read_csv(f$datapath)
    })
    
    output$tab_logs_full <- renderDataTable({ d() })
    
    d_deltas <- reactive({
        d_hands <- d() %>% 
            arrange(order) %>% 
            mutate(hand = map_dbl(entry, function(e) {
                if(str_detect(e, '-- starting hand')) {
                    m <- str_match(e, '-- starting hand #([0-9]+)')
                    hand <- as.numeric(m[, 2])
                    hand
                } else {
                    NA
                }
            })) %>% 
            mutate(hand = na.locf0(hand)) %>% 
            drop_na() %>% 
            group_by(hand) %>% 
            nest(data = -hand)
        
        d_parsed <- d_hands %>% 
            mutate(parsed = map(data, function(d) {
                parse_entries(d$entry)
            })) %>% 
            select(-data) %>% 
            unnest_wider(parsed)
        d_parsed$stacks_curr <- d_parsed$stacks
        d_parsed$stacks_next <- lead(d_parsed$stacks)
        d_parsed
        
        d_deltas <- d_parsed %>% 
            mutate(deltas = pmap(list(stacks_curr, stacks_next, parts, stands, sitbacks, adds, quits), calc_deltas)) %>% 
            mutate(delta_sum = map_dbl(deltas, function(d) sum(d$delta)))
        
        d_deltas
    })
    
    id_names <- reactive({
        names_id <- d_deltas() %>% 
            ungroup() %>% 
            select(stacks) %>% 
            unnest(stacks) %>% 
            select(name, id) %>% 
            unique()
        
        id_names <- names_id %>% 
            group_by(id) %>% 
            summarize(names = str_c(name, collapse = ' / '))
        
        id_names
    })
    
    deltas <- reactive({
        id_names_list <- setNames(as.list(id_names()$names), id_names()$id)
        
        ids_to_names <- function(ids) {
            Vectorize(function(id) id_names_list[[id]])(ids)
        }
        
        deltas <- d_deltas() %>% 
            select(hand, deltas, delta_sum) %>% 
            unnest(deltas) %>% 
            select(hand, id, delta) %>% 
            mutate(name = ids_to_names(id)) %>% 
            select(-id) %>% 
            ungroup()
        deltas
    })
    
    levels <- reactive({
        levels <- deltas() %>% 
            pivot_wider(names_from = name, values_from = delta, values_fill = 0) %>% 
            mutate_at(vars(-hand), cumsum)
        levels
    })
    
    output$tab_player_summary <- renderDataTable({
        l <- levels() %>% 
            pivot_longer(-hand) %>%
            group_by(name) %>% 
            summarize(`Ending Stack` = last(value)) %>% 
            ungroup() %>% 
            rename(Name = name)
        
        d <- deltas() %>% 
            group_by(name) %>% 
            summarize(`Per Hand Mean` = mean(delta), `Per Hand SD` = sd(delta)) %>% 
            ungroup() %>% 
            rename(Name = name)
        
        l %>% inner_join(d, by = 'Name') %>% datatable_default()
    })
    
    output$tab_game_details <- renderDataTable({
        tib <- tibble(
            `Total Hands` = max(d_deltas()$hand) %>% as.character(),
            `Start Time` = min(d()$at) %>% as.character(),
            `End Time` = max(d()$at) %>% as.character()
        )
        tib <- as_tibble(cbind(nms = names(tib), t(tib)))
        colnames(tib) <- c('Name', 'Value')
        tib %>% datatable_default()
    })
    
    output$plot_levels <- renderPlotly({
        p <- levels() %>% 
            pivot_longer(cols = -hand) %>% 
            mutate(Name = name) %>% 
            ggplot(mapping = aes(x = hand, y = value, color = Name)) +
            geom_line() +
            ggtitle('Stack Size') +
            xlab('Hand') +
            ylab('Stack Size') +
            theme_bw()
        
        p %>% ggplotly()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
