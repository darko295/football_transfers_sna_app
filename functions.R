compute_measures <- function(df, club_df){
  c_measures <- compute_centrality_measures(df %>% select(ClubFromID,ClubToID,DivFrom,DivTo))
  o_measures <- compute_other_measures(df,club_df)
  measures <- merge(c_measures,o_measures)

  measures$DomesticPerc <- round(measures$Domestic/measures$total_degree*100,0)
  measures
  }


compute_centrality_measures <- function(df){
  
  g <- graph_from_data_frame(df,T)
  
  m_df <- data.frame()
  in_degree <- as.data.frame(degree(g, mode = "in"))
  out_degree <- as.data.frame(degree(g, mode = "out"))
  total_degree <- as.data.frame(degree(g, mode = "total"))
<<<<<<< HEAD
  closeness <- as.data.frame(round(closeness(g,mode = "all"),2))
=======
>>>>>>> Initial commit
  betweenness <- as.data.frame(round(betweenness(g, directed = T),2))
  
  in_degree$Club <- row.names(in_degree)
  out_degree$Club <- row.names(out_degree)
  total_degree$Club <- row.names(total_degree)
<<<<<<< HEAD
  closeness$Club <- row.names(closeness)
  betweenness$Club <- row.names(betweenness)

  m_df <- merge(merge(merge(merge(in_degree,out_degree,by="Club"),total_degree,by="Club"),closeness,by="Club"),betweenness,by="Club")
  colnames(m_df) <- c("ID","in_degree","out_degree","total_degree","closeness","betweenness")
=======
  betweenness$Club <- row.names(betweenness)

  m_df <- merge(merge(merge(in_degree,out_degree,by="Club"),total_degree,by="Club"),betweenness,by="Club")
  colnames(m_df) <- c("ID","in_degree","out_degree","total_degree","betweenness")
>>>>>>> Initial commit
  m_df$ID <- as.integer(m_df$ID)
  m_df
  
}

compute_other_measures <- function(df,club_df){
  #TRANSFER BALANCE
  earned <- df %>% group_by(ClubFromID) %>% dplyr::summarise(earned = sum(Amount))
  spent <- df %>% group_by(ClubToID) %>% dplyr::summarise(spent = sum(Amount))
  
  names(earned)[1] <- "ID"
  names(spent)[1] <- "ID"
  
  transfer_balance <- merge(earned,spent,all = T)
  
  transfer_balance[is.na(transfer_balance)] <- 0
  transfer_balance$profit_loss <- transfer_balance$earned - transfer_balance$spent
  
  #DOMESTIC PERCENT
  dt_perc <- compute_domestic_transfer_percent(df,club_df)
  
  other_m <- merge(transfer_balance,dt_perc, by = "ID")
  other_m
}

compute_domestic_transfer_percent <- function(df, club_df){
  domestic_df <- data.frame()
  for (i in 1:nrow(club_df)) {
    filtered <- df %>% filter(ClubFrom == club_df$Club[i] | ClubTo == club_df$Club[i])
    domestic <- length(which(filtered$DivFrom == filtered$DivTo))
    temp <- data.frame(ID = club_df$ID[i], Domestic = domestic)
    domestic_df <- rbind(domestic_df,temp)
  }
  domestic_df
}

compute_graph_info <- function(df){

  g <- graph_from_data_frame(df,T)
  
  ncomp_dir <- count_components(g,mode = "strong")
  ncomp_undir <- count_components(g,mode = "weak")
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  diam_dir <- diameter(g,directed = T)
  diam_undir <- diameter(g,directed = F)
  
  gi_df <- data.frame("Num of components (dir)" = ncomp_dir,
                      "Num of components (undir)" = ncomp_undir,
                      "Num of nodes" = nodes_n,
                      "Num of edges" = edges_n,
                      "Density" = density,
                      "Links per node" = links_per_node,
                      "Diametar (dir)" = diam_dir,
                      "Diametar (undir)" = diam_undir)
  
 gi_df
  
}

attr_based_color_gradient <- function(g_attr, pal_end_points) {
  require(dplyr)
  # 1) Set the resolution, that is, how many color nuances are to be created
  col_resolution = n_distinct(g_attr)
  # 2) Set palette end points 
  col_palette = colorRampPalette(pal_end_points)
  # 3) Get the max value of the attribute to make the ratio: 
  max_val = max(g_attr, na.rm = TRUE)
  # 4) Create a vector of values which will determine the color values 
  # for each node: 
  value_vector = g_attr / max_val
  # 5) Create the vector of color values, based on the value_vector, the 
  # palette end points and the resolution. This will produce a vector of 
  # color values with the correct color value in the correct location for 
  # the chosen graph attribute: 
  g_colors = col_palette(col_resolution)[as.numeric(cut(value_vector, breaks=col_resolution))]
  return(g_colors)
}
<<<<<<< HEAD

#computing graph info by division by year f
compute_graph_info_by_div_year <- function(df,div,year=2004){
  df <- df %>% filter(Year_F == year & DivFrom == div & DivTo == div) %>% select(ClubFromID,ClubToID)
  g <- graph_from_data_frame(df,T)
  ncomp_dir <- count_components(g,mode = "strong")
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  diam_dir <- diameter(g,directed = T)
  diam_undir <- diameter(g,directed = F)
  
  gi_df <- data.frame("Division" = as.character(div),
                      "Num of components (dir)" = ncomp_dir,
                      "Density" = density,
                      "Links per node" = links_per_node
  )
  
  gi_df
  
}

#computing divisions with most interactions f
compute_interactions <- function(df){
  eng_eng <- 0
  spa_spa <- 0
  ita_ita <- 0
  ger_ger <- 0
  fra_fra <- 0
  eng_spa <- 0
  eng_ita <- 0
  eng_ger <- 0
  eng_fra <- 0
  spa_ita <- 0
  spa_ger <- 0
  spa_fra <- 0
  ita_ger <- 0
  ita_fra <- 0
  ger_fra <- 0
  
  for (i in 1:nrow(df)) {
    cur <- df[i, ]
    if (cur$DivFrom == "England" & cur$DivTo == "England"){
      eng_eng <- eng_eng + 1
    }
    if (cur$DivFrom == "Spain" & cur$DivTo == "Spain"){
      spa_spa <- spa_spa + 1
    }
    if (cur$DivFrom == "Italy" & cur$DivTo == "Italy"){
      ita_ita <- ita_ita + 1
    }
    if (cur$DivFrom == "Germany" & cur$DivTo == "Germany"){
      ger_ger <- ger_ger + 1
    }
    if (cur$DivFrom == "France" & cur$DivTo == "France"){
      fra_fra <- fra_fra + 1
    }
    
    if (cur$DivFrom == "England" &
        cur$DivTo == "Spain" ||
        cur$DivFrom == "Spain" & cur$DivTo == "England"){
      eng_spa <- eng_spa + 1
    }
    if (cur$DivFrom == "England" &
        cur$DivTo == "Italy" ||
        cur$DivFrom == "Italy" & cur$DivTo == "England"){
      eng_ita <- eng_ita + 1
    }
    if (cur$DivFrom == "England" &
        cur$DivTo == "Germany" ||
        cur$DivFrom == "Germany" & cur$DivTo == "England"){
      eng_ger <- eng_ger + 1
    }
    if (cur$DivFrom == "England" &
        cur$DivTo == "France" ||
        cur$DivFrom == "France" & cur$DivTo == "England"){
      eng_fra <- eng_fra + 1
    }
    if (cur$DivFrom == "Spain" &
        cur$DivTo == "Italy" ||
        cur$DivFrom == "Italy" & cur$DivTo == "Spain"){
      spa_ita <- spa_ita + 1
    }
    if (cur$DivFrom == "Spain" &
        cur$DivTo == "Germany" ||
        cur$DivFrom == "Germany" & cur$DivTo == "Spain"){
      spa_ger <- spa_ger + 1
    }
    if (cur$DivFrom == "Spain" &
        cur$DivTo == "France" ||
        cur$DivFrom == "France" & cur$DivTo == "Spain"){
      spa_fra <- spa_fra + 1
    }
    if (cur$DivFrom == "Italy" &
        cur$DivTo == "Germany" ||
        cur$DivFrom == "Germany" & cur$DivTo == "Italy"){
      ita_ger <- ita_ger + 1
    }
    if (cur$DivFrom == "Italy" &
        cur$DivTo == "France" ||
        cur$DivFrom == "France" & cur$DivTo == "Italy"){
      ita_fra <- ita_fra + 1
    }
    if (cur$DivFrom == "Germany" &
        cur$DivTo == "France" ||
        cur$DivFrom == "France" & cur$DivTo == "Germany"){
      ger_fra <- ger_fra + 1
    }
  }
  #eng_eng,spa_spa,ita_ita,ger_ger,fra_fra,
  dist <- data.frame(eng_eng,spa_spa,ita_ita,ger_ger,fra_fra,eng_spa,eng_ita,eng_ger,eng_fra,spa_ita,spa_ger,spa_fra,ita_ger,ita_fra,ger_fra)
  dist <- as.data.frame(t(dist))
}
=======
>>>>>>> Initial commit
