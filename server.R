

library(tidyverse)
library(shiny)
library(rlang)
library(viridis)

trees_2015 <- read_csv("large_tree_data/2015_tree_raw.csv", na = c("", "NA", "Unknown")) |>
  janitor::clean_names() %>%
  mutate(spc_common = str_to_title(spc_common)) %>%
  mutate(health = fct_relevel(health, c("Good", "Fair", "Poor")))

tree_status_df <- trees_2015 %>%
  select(curb_loc, guards, sidewalk, health, status,
         borough,
         root_stone, root_grate, root_other,
         trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other) %>%
  mutate(health = fct_relevel(health, c("Good", "Fair", "Poor")),
         borough = as.factor(borough),
         All = as.factor("All"),
         curb_loc = case_match(curb_loc,
                               "OffsetFromCurb" ~ "Offset From Curb",
                               "OnCurb" ~ "On Curb"),
         curb_loc = as.factor(curb_loc),
         guards = as.factor(guards),
         sidewalk = case_match(sidewalk,
                               "NoDamage" ~ "No Damage",
                               "Damage" ~ "Damage"),
         sidewalk = as.factor(sidewalk),
         status = as.factor(status)
  ) %>%
  rename(`Curb Location` = curb_loc,
         `All of NYC` = All,
         Borough = borough,
         Guards = guards,
         Sidewalk = sidewalk,
         `Alive, Dead, or Stump` = status,
         Health = health,
         `Roots with Stones` = root_stone,
         `Roots in Grate` = root_grate,
         `Root Problem (Other)` = root_other,
         `Trunk has Wires` = trunk_wire,
         `Trunk has Lights` = trnk_light,
         `Trunk has Other Problem` = trnk_other,
         `Branches have Lights` = brch_light,
         `Branches with Shoes` = brch_shoe,
         `Branches Problem (Other)` = brch_other) %>%
  select(Borough, `All of NYC`, everything())

exposure_options <- colnames(tree_status_df)[3:length(tree_status_df)]
outcome_options <- colnames(tree_status_df)[3:length(tree_status_df)]


# Define function

effective_tree_protection <- function(df, exposure_str, outcome_str){
  
  # Convert character strings to symbols
  exposure_sym <- rlang::sym(exposure_str)
  outcome_sym <- rlang::sym(outcome_str)
  
  # Make contingency table for the chi-square test
  contingency_table <- df %>%
    filter(!is.na(!!exposure_sym), !is.na(!!outcome_sym)) %>%
    group_by(!!outcome_sym, !!exposure_sym) %>%
    count(!!outcome_sym, !!exposure_sym) %>%
    pivot_wider(names_from = !!exposure_sym, values_from = n)
  
  # Perform the chi-square test
  chisq_test_result <- chisq.test(contingency_table[-1])
  chi_square <- chisq_test_result$statistic
  p_value <- chisq_test_result$p.value
  
  # Visualize the distribution differences 
  plot_title <- str_c("Distribution of ", exposure_str, 
                      " by ", outcome_str)
  
  df %>%
    filter(!is.na(!!exposure_sym), !is.na(!!outcome_sym)) %>%
    group_by(!!exposure_sym, !!outcome_sym) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(!!exposure_sym) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ggplot(aes(x = !!exposure_sym, y = Percent, fill = !!outcome_sym)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_text(aes(label = sprintf("%.1f%%", Percent)),
              position = position_dodge(width = 0.9),
              size = 3.5, 
              vjust = -0.3) +
    labs(title = plot_title,
         subtitle = sprintf("p value = %.1e (Chi-square = %.2f)", p_value, chi_square)) 
}



# library(shiny)
# library(ggplot2)

function(input, output) {

  df <- reactive({
    tree_status_df
  })

  output$plot <- renderPlot({
    effective_tree_protection(df = tree_status_df, 
                              exposure_str = input$exposure, 
                              outcome_str = input$outcome)
  })

}





# library(shiny)
# library(ggplot2)

# function(input, output) {
#   
#   dataset <- reactive({
#     diamonds[sample(nrow(diamonds), input$sampleSize),]
#   })
#   
#   output$plot <- renderPlot({
#     
#     p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
#     
#     if (input$color != 'None')
#       p <- p + aes_string(color=input$color)
#     
#     facets <- paste(input$facet_row, '~', input$facet_col)
#     if (facets != '. ~ .')
#       p <- p + facet_grid(facets)
#     
#     if (input$jitter)
#       p <- p + geom_jitter()
#     if (input$smooth)
#       p <- p + geom_smooth()
#     
#     print(p)
#     
#   }, height=700)
#   
# }
