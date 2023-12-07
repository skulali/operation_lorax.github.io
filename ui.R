library(shiny)
library(ggplot2)
library(tidyverse)

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

fluidPage(

  titlePanel("Trees"),

  sidebarPanel(

    # Input: Select variable for 'exposure'
    selectInput('exposure', 'Exposure',
     # inputId = "exposure", 
          #      label = "Select Category (Exposure):", 
                choices = exposure_options,
                selected = "Curb Location"),
    
    # Input: Select variable for 'outcome'
    selectInput('outcome', 'Tree Status',
     # inputId = "outcome", 
         #       label = "Select Tree Status (Outcome):",
                choices = outcome_options,
                selected = "Health")
  ),

  mainPanel(
    plotOutput('plot')
  )
)



# dataset <- diamonds
# 
# fluidPage(
#   
#   titlePanel("Diamonds Explorer"),
#   
#   sidebarPanel(
#     
#     sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
#                 value=min(1000, nrow(dataset)), step=500, round=0),
#     
#     selectInput('x', 'X', names(dataset)),
#     selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
#     selectInput('color', 'Color', c('None', names(dataset))),
#     
#     checkboxInput('jitter', 'Jitter'),
#     checkboxInput('smooth', 'Smooth'),
#     
#     selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#     selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#   ),
#   
#   mainPanel(
#     plotOutput('plot')
#   )
# )
