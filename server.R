library(shiny)
library(plyr)
library(dplyr)
source("functions.R")


df <- read.csv("data/td.csv", header = T, sep = ",", stringsAsFactors = F)
club_df <- read.csv("data/cd.csv", header = T, sep = ",", stringsAsFactors = F)

top5 <- c("England","Spain","Italy","Germany","France")
color_pallete = data.frame(Div = top5, Color = c("#97c2fc","#ffff00","#fb7e81","#eb7df4","#7be141"))



shinyServer(function(input, output) {
  myNode <- reactiveValues(selected = '')
  
  observe({
    
    
    size <- input$node_size
    if(!is.null(input$division)){
      if(!any(input$division %in% "all")){top5 <- input$division}
    }
    
    df <- df %>% filter(Year_F == input$year & DivTo %in% top5 & DivFrom %in% top5)
    club_df <- club_df %>% filter(Year_F == input$year & Division %in% top5 & Club %in% c(unique(df$ClubFrom),unique(df$ClubTo))) 
    
    #compute metrics
    measures <- compute_measures(df,club_df)
    club_df <- join(club_df, measures, by="ID")
    
    
    
    nodes <- data.frame(id = club_df$ID,
                        value = club_df[,size],
                        label = club_df$Club,
                        group = club_df$Division,
                        title = club_df$Club,
                        color = apply(as.array(club_df$Division) ,MARGIN = 1,function(x){color_pallete$Color[color_pallete$Div == x]})
    )
    
    edges <- data.frame(from = df$ClubFromID, 
                        to = df$ClubToID,
                        smooth = TRUE,
                        value = df$Amount,
                        title = paste0("Player: ",df$Name,
                                       "<br>From: ",df$ClubFrom,
                                       "<br>To: ",df$ClubTo,
                                       "<br>Amount: ",df$Amount,"M€")
                        
    )
    
    
    if((input$division == "all" && length(input$division) == 1)  || length(input$division) > 3){
      
      output$full_graph <- renderVisNetwork({
        visNetwork(nodes,edges) %>%
          visGroups(groupname = "England", color = "#97c2fc") %>% 
          visGroups(groupname = "Spain", color = "#ffff00") %>%
          visGroups(groupname = "Italy", color = "#fb7e81") %>%
          visGroups(groupname = "Germany", color = "#eb7df4") %>%
          visGroups(groupname = "France", color = "#7be141") %>%
          visLegend(width = 0.07, position = "right",zoom = F) %>%
          visNodes(scaling = list("min" = 10,"max"=50)) %>%
          visEdges(scaling = list("min" = 1,"max"=20),
                   arrows = "to",arrowStrikethrough = F) %>%
          visIgraphLayout(layout = input$layout) %>%
          visOptions(highlightNearest = T,selectedBy = "group", nodesIdSelection = T) %>%
          visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>%
          visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 110))
      })
    }else{
      
      output$full_graph <- renderVisNetwork({
        visNetwork(nodes,edges) %>%
          visGroups(groupname = "England", color = "#97c2fc") %>% 
          visGroups(groupname = "Spain", color = "#ffff00") %>%
          visGroups(groupname = "Italy", color = "#fb7e81") %>%
          visGroups(groupname = "Germany", color = "#eb7df4") %>%
          visGroups(groupname = "France", color = "#7be141") %>%
          visLegend(width = 0.07, position = "right",zoom = F) %>%
          visNodes(scaling = list("min" = 10,"max"=25)) %>%
          visEdges(scaling = list("min" = 1,"max"=10),
                   arrows = "to",arrowStrikethrough = F) %>%
          visIgraphLayout(layout = input$layout) %>%
          visOptions(highlightNearest = T, nodesIdSelection = T) %>%
          visEvents(select = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes.nodes);
                    ;}") %>%
          visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 50))
      })
    }
    graph_info <- compute_graph_info(df %>% select(ClubFromID,ClubToID))
    output$graph_data_dt <- DT::renderDataTable({
      datatable(t(graph_info),caption = "Graph info",colnames = c("Indicator","Value"),
                rownames = c("Number of components (dir)","Number of components (undir)",
                             "Number of nodes","Number of edges","Density","Links per node",
                             "Diametar (dir)","Diametar (undir)"),
                options = list(autoWidth = F, searching = FALSE, scrollX = F,
                               bPaginate= FALSE, dom = 'Bt',
                               buttons = list(
                                 list(
                                   extend = 'excel',
                                   text = '.XLSX',
                                   filename = 'Clubs data'
                                 )
                               )))
    })
    
    
    #edges data tab
    output$transfers_data <- DT::renderDataTable({
      datatable(df %>% select(-InOut, -ClubFromID, -ClubToID, - Year_F), options = list(
        autoWidth = TRUE, searching = FALSE, scrollX = F, bPaginate= FALSE, dom = 'Bt',
        buttons = list(
          list(
            extend = 'excel',
            text = '.XLSX',
            filename = 'Transfers data'
          )
        )))
    })
    
    
    output$club_data <- DT::renderDataTable({
      datatable(club_df %>% select(-ID, -Year_F), options = list(
        autoWidth = TRUE, searching = FALSE, scrollX = F, bPaginate= FALSE, dom = 'Bt',
        buttons = list(
          list(
            extend = 'excel',
            text = '.XLSX',
            filename = 'Clubs data'
          )
        )))
    })
    
  })
  
  observe({
    if(!is.null(input$comp_division_1)){
      if(!any(input$comp_division_1 %in% "all")){top5 <- input$comp_division_1}
    }
    
    
    df_1 <- df %>% filter(Year_F == input$comp_year_1 & DivTo %in% top5 & DivFrom %in% top5)
    club_df_1 <- club_df %>% filter(Year_F == input$comp_year_1 & Division %in% top5 & Club %in% c(unique(df_1$ClubFrom),unique(df_1$ClubTo))) 
    
    #compute metrics
    measures_1 <- compute_measures(df_1,club_df_1)
    club_df_1 <- join(club_df_1, measures_1, by="ID")
    
    
    
    
    size_comp <- input$node_size_comp
    nodes_1 <- data.frame(id = club_df_1$ID,
                          value = club_df_1[,size_comp],
                          label = club_df_1$Club,
                          group = club_df_1$Division,
                          title = club_df_1$Club
    )
    
    edges_1 <- data.frame(from = df_1$ClubFromID, 
                          to = df_1$ClubToID,
                          smooth = TRUE,
                          value = df_1$Amount,
                          title = paste0("Player: ",df_1$Name,
                                         "<br>From: ",df_1$ClubFrom,
                                         "<br>To: ",df_1$ClubTo,
                                         "<br>Amount: ",df_1$Amount,"M€")
                          
    )
    
    
    if(!is.null(input$comp_division_2)){
      if(!any(input$comp_division_2 %in% "all")){top5 <- input$comp_division_2}
    }
    
    
    df_2 <- df %>% filter(Year_F == input$comp_year_2 & DivTo %in% top5 & DivFrom %in% top5)
    club_df_2 <- club_df %>% filter(Year_F == input$comp_year_2 & Division %in% top5 & Club %in% c(unique(df_2$ClubFrom),unique(df_2$ClubTo))) 
    
    #compute metrics
    measures_2 <- compute_measures(df_2,club_df_2)
    club_df_2 <- join(club_df_2, measures_2, by="ID")
    
    nodes_2 <- data.frame(id = club_df_2$ID,
                          value = club_df_2[,size_comp],
                          label = club_df_2$Club,
                          group = club_df_2$Division,
                          title = club_df_2$Club
    )
    
    edges_2 <- data.frame(from = df_2$ClubFromID, 
                          to = df_2$ClubToID,
                          smooth = TRUE,
                          value = df_2$Amount,
                          title = paste0("Player: ",df_2$Name,
                                         "<br>From: ",df_2$ClubFrom,
                                         "<br>To: ",df_2$ClubTo,
                                         "<br>Amount: ",df_2$Amount,"M€")
                          
    )
    
    
    
    output$comp_graph_1 <- renderVisNetwork({
      visNetwork(nodes_1,edges_1,background = "#e6e6e630") %>%
        visGroups(groupname = "England", color = "#97c2fc") %>% 
        visGroups(groupname = "Spain", color = "#ffff00") %>%
        visGroups(groupname = "Italy", color = "#fb7e81") %>%
        visGroups(groupname = "Germany", color = "#eb7df4") %>%
        visGroups(groupname = "France", color = "#7be141") %>%
        visLegend(width = 0.1, position = "right",zoom = F) %>%
        visNodes(scaling = list("min" = 10,"max"=30)) %>%
        visEdges(scaling = list("min" = 1,"max"=10),
                 arrows = "to") %>%
        visIgraphLayout(layout = input$layout_comp) %>%
        #visLayout(randomSeed = T) %>%
        visOptions(highlightNearest = T)
      
    })
    
    output$comp_graph_2 <- renderVisNetwork({
      visNetwork(nodes_2,edges_2,background = "#e6e6e630") %>%
        visGroups(groupname = "England", color = "#97c2fc") %>% 
        visGroups(groupname = "Spain", color = "#ffff00") %>%
        visGroups(groupname = "Italy", color = "#fb7e81") %>%
        visGroups(groupname = "Germany", color = "#eb7df4") %>%
        visGroups(groupname = "France", color = "#7be141") %>%
        visLegend(width = 0.1, position = "right",zoom = F) %>%
        visNodes(scaling = list("min" = 10,"max"=30)) %>%
        visEdges(scaling = list("min" = 1,"max"=10),
                 arrows = "to") %>%
        visIgraphLayout(layout = input$layout_comp) %>%
        #visLayout(randomSeed = T) %>%
        visOptions(highlightNearest = T)
      
    })
    
    
    
    
  })
  
  
  observeEvent(input$current_node_id, {
    df <- df %>% filter(Year_F == input$year & DivTo %in% top5 & DivFrom %in% top5)
    club_df <- club_df %>% filter(Year_F == input$year & Division %in% top5) 
    #compute metrics
    print(paste0("DIVISION:",input$division))
    measures <- compute_measures(df,club_df)
    club_df <- join(club_df, measures, by="ID")
    
    print(paste0("SELECTED NODES:",input$current_node_id))
    
    output$selected_club_dt <- DT::renderDataTable({
      datatable(t(club_df %>% filter(ID %in% input$current_node_id & Year_F == input$year) %>%
                    select(-ID, -Year_F)), caption = "Last selected club info", colnames = c("Node param","Value"),
                options = list(autoWidth = TRUE, searching = FALSE, scrollX = F, bPaginate= FALSE,
                               dom = 'Bt', buttons = list(
                                 list(
                                   extend = 'excel',
                                   text = '.XLSX',
                                   filename = 'Selected club data'
                                 )
                               )))
    })
  })
  
  
  
  
  
})
