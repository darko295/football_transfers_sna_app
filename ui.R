library(shiny)
library(visNetwork)
library(igraph)
library(DT)


navbarPage(
    title = "SNA Football Transfers",
    id = "nav",
    # shiny::tags$head(
    #  tags$link(rel = "stylesheet", type = "text/css", href= "www/style.css")
    # ),
    tabPanel("Main graph",
             column(2,
                    selectInput("year","Year:",choices = c("2018"=2018,"2004"=2004),selected = "2018"),
                    selectInput("division","Division",choices = c("All"="all", "Premier league"="England","La liga" = "Spain",
                                "Serie A"= "Italy","Liga 1"="France","Bundesliga"="Germany"),multiple = T,
                                selected = "all"),
                    #selectInput("node_color","Color nodes by:",choices = c("Division"="division","Profit/Loss"="profit_loss"),selected = "division"),
                    selectInput("node_size","Size nodes by:",choices = c("Earned"="earned","Spent"="spent", "In degree"="in_degree",
                                                                         "Out degree"="out_degree","Total degree"="total_degree","Betweenness"="betweenness"),selected = "earned"),
                    selectInput("layout","Layout:",choices = c("Layout nicely"="layout_nicely","Layout as star"="layout_as_star", "Layout in circle"="layout_in_circle",
                                                               "Layout on grid"="layout_on_grid", "Layout with dh"="layout_with_dh","Layout with fr"="layout_with_fr",
                                                               "Layout with kk" = "layout_with_kk","Layout with lgl" = "layout_with_lgl", "Layout with mds" = "layout_with_mds"),selected = "layout_with_dh",
                                ),
                    conditionalPanel("(typeof input.current_node_id.length !== 'undefined' && input.current_node_id.length > 0",
                                     div(DT::dataTableOutput("selected_club_dt"), style = "font-size: 95%;")
                    ),
                    div(DT::dataTableOutput("graph_data_dt"), style = "font-size: 95%;")
                    
                    ),
             column(10,
             visNetworkOutput("full_graph",width = "100%", height = "890px")
    )
        ),
    tabPanel("By division comparison",
             fluidRow(
             column(2,
                    h4("Graph 1"),
                    selectInput("comp_year_1","Year:",choices = c("2018"=2018,"2004"=2004),selected = "2004"),
                    selectInput("comp_division_1","Division",choices = c("All"="all", "Premier league"="England","La liga" = "Spain",
                                                                  "Serie A"= "Italy","Liga 1"="France","Bundesliga"="Germany"),multiple = T,
                                selected = "England")
             ),
             column(2,offset = 3,
                    h4("Common"),
                    selectInput("node_size_comp","Size nodes by:",choices = c("Earned"="earned","Spent"="spent", "In degree"="in_degree",
                                                                         "Out degree"="out_degree","Total degree"="total_degree","Betweenness"="betweenness"),selected = "earned"),
                    selectInput("layout_comp","Layout:",choices = c("Layout nicely"="layout_nicely","Layout as star"="layout_as_star", "Layout in circle"="layout_in_circle",
                                                               "Layout on grid"="layout_on_grid", "Layout with dh"="layout_with_dh","Layout with fr"="layout_with_fr",
                                                               "Layout with kk" = "layout_with_kk","Layout with lgl" = "layout_with_lgl", "Layout with mds" = "layout_with_mds"),selected = "layout_with_dh",
                    )
                    ),
             column(2,offset = 3,
                    h4("Graph 2"),
                    selectInput("comp_year_2","Year:",choices = c("2018"=2018,"2004"=2004),selected = "2018"),
                    selectInput("comp_division_2","Division",choices = c("All"="all", "Premier league"="England","La liga" = "Spain",
                                                                         "Serie A"= "Italy","Liga 1"="France","Bundesliga"="Germany"),multiple = T,
                                selected = "England")
             )),
             fluidRow(
             column(6,
                      visNetworkOutput("comp_graph_1",width = "100%",height = "650px")
             ),
             column(6,
                    visNetworkOutput("comp_graph_2",width = "100%",height = "650px")
             ))
    ),
    tabPanel("Edges data",
             fluidRow(
                 column(10,offset = 1,
                        div(DT::dataTableOutput("transfers_data"), style = "font-size: 95%;")
                 )
             )
    ),
    tabPanel("Nodes data",
             fluidRow(
                 column(10,offset = 1,
                        div(DT::dataTableOutput("club_data"), style = "font-size: 95%;")
                 )
             )
    ),
    tags$head(tags$style("#graphfull_graph{margin-top: -100px;}")),
    tags$head(tags$style(".dropdown{z-index:5000;}"))
)