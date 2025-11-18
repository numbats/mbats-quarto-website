
# libraries ---------------------------------------------------------------

library(tidygraph)
library(ggraph)
library(tidyverse)
library(readxl)
library(networkD3)
library(ggiraph)


# data cleaning -----------------------------------------------------------

raw_data <- read_xlsx("data/unit_descriptions.xlsx") |>
  janitor::clean_names() 

mbat_unit <- unique(raw_data$code)

data <- raw_data |> 
  select(code, pre_requisites) |> 
  mutate(pre_requisites = str_split(pre_requisites, ", ")) |> 
  unnest(pre_requisites) |> 
  mutate(pre_requisites = toupper(pre_requisites)) |> 
  filter(pre_requisites %in% c(NA, mbat_unit),
         !(code == "ETC5450" & pre_requisites %in% c("ETC5580", "ETC5250", "ETC5550"))) |> 
  rename(to = code,
         from = pre_requisites) |> 
  mutate(from = ifelse(is.na(from), "MBAT", from))  


part_a <- c("ETC5242", "ETC5250", "ETC5510", "ETC5550")

part_b_1 <- c("ETC5512", "ETC5513", "ETC5521", "ETC5523", "ETC5543")

part_b_2 <- c("ETC5450", "ETC5555", "ETC5580", "ETX5500")

first_first <- c("ETC5510", "ETC5512", "ETC5513")

first_second <- c("ETC5242", "ETC5521", "ETC5523")

second_first <- c("ETC5250", "ETC5450", "ETC5580")

second_second <- c("ETC5550", "ETC5543", "ETC5555", "ETX5500")

graph <- as_tbl_graph(data, directed = TRUE) |> 
  mutate(part = 
           case_when(
             name %in% part_a ~ "Part A. Advanced preparatory studies",
             name %in% part_b_1 ~ "Part B. Core studies",
             name %in% part_b_2 ~ "Part B. Core studies electives",
             .default = "Course"),
         part = factor(part, levels = c("Course", 
                                        "Part A. Advanced preparatory studies",
                                        "Part B. Core studies",
                                        "Part B. Core studies electives")),
         layer = as.interger(case_when(
           name %in% first_first ~ 2,
           name %in% first_second ~ 3,
           name %in% second_first ~ 4,
           name %in% second_second ~ 5,
           .default = 1
         )),
         link = case_when(
           name == "MBAT" ~ "https://handbook.monash.edu/current/courses/B6022",
           .default = paste0("https://handbook.monash.edu/current/units/", name)
         )
  )


# visualization -----------------------------------------------------------

graph |> 
  ggraph(layout = "sugiyama", layer = layer) +
  geom_node_label(aes(label = name, fill = part)) +
  geom_edge_diagonal(aes(start_cap = label_rect(node1.name),
                         end_cap = label_rect(node2.name)),
                     arrow = grid::arrow(length = unit(2, "mm")),
                     strength = 0.7) +
  scale_fill_manual(name = "Course Structure",
                    values = c("white", "#92CDDC", "#DAEEF3", "#DAEEF3")) +
  theme_void()


# interactive -------------------------------------------------------------

course <- graph |> 
  ggraph(layout = "sugiyama") +
  geom_label_interactive(aes(x = x, y = y, label = name, fill = part, 
                             tooltip = part, data_id = name,
                             onclick = sprintf("window.open(\"%s\")", link))) +
  geom_edge_diagonal(aes(start_cap = label_rect(node1.name),
                         end_cap = label_rect(node2.name)),
                     arrow = grid::arrow(length = unit(1.5, "mm")),
                     strength = 0.8) +
  scale_fill_manual(name = "Course Structure",
                    values = c("white", "#92CDDC", "#DAEEF3", "#DAEEF3")) +
  theme_void()

interactive_course <- girafe(ggobj = course, width_svg = 12,
                             options = list(
                               opts_hover(css = "fill:lightblue;stroke:grey;stroke-width:0.5px")
                             ))

htmltools::save_html(interactive_course, "course.html")
