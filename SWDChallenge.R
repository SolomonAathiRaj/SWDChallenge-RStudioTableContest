#SWDChallenge #RStudioTableContest

library(tidytuesdayR)
library(tidyverse)
library(gt)

# Import the data 
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
#edit(kids)

data <- kids %>% 
  select(state, variable, year, inf_adj_perchild)

#summary(data)

years <- c(2007, 2016)

# Filter and arrange the rows
table <- data %>% 
  filter(year %in% years,
         variable == "pubhealth") %>% 
  mutate(inf_adj_perchild = round(inf_adj_perchild, digits = 2)) %>% 
  pivot_wider(names_from = year, values_from = inf_adj_perchild) %>% 
  mutate(change = round((`2016` - `2007` ) / `2007` * 100)) %>%
  #arrange(desc(`2016`)) %>%
  top_n(15) %>%
  rename(State = state) %>% 
  select(-variable) 


# sparklines for every state

pub_health <- data %>% 
  filter(variable == "pubhealth") %>% 
  rename(State = state)


plot_group <- function(df){
  plot_object <- 
    ggplot(data = df, aes(x = year, y = inf_adj_perchild,  color = inf_adj_perchild, group = 1)) +
    geom_line(size = 20) +
    scale_colour_gradient2(low = "#4be28f", mid = "#7199b0", high="#9850d3", midpoint=median(df$inf_adj_perchild)) + 
   
    geom_point(data = subset(df, year == 2016), color = "#7a89b8", size = 40) +
    theme_void()+
    theme(legend.position="none") 
    return(plot_object)
}

tibble_plot <- pub_health %>%
  group_by(State) %>% 
  nest() %>% 
  mutate(plot = map(data, plot_group)) %>%
  select(-data)

# creating the table

table %>% 
  mutate(ggplot = NA) %>% 
  gt() %>% 
  tab_header(title = html("<b>Change in Public Spending on Healthcare in the US"),
             subtitle = md("Dollars in 1000s spent per child (inflation-adjusted)<br><br>")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(gt::everything())) %>% 
  data_color(columns = vars(change),
             colors = scales::col_numeric(c("#d6f7e5","#daf0e9", "#dbeaea", "#dfe5ee", "#8ac9bb","#e3dff0","#e5daf4","#d0b9ea", "#9ca8ca"), domain = NULL)) %>% 
  cols_align(align = "center",
             columns = 2:4) %>% 
  opt_table_font(font = list(c("IBM Plex Sans"))) %>% 
  tab_options(heading.title.font.size = 20,
              table.font.names = "Century Gothic",
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  cols_label(ggplot = "Trend",
             change = "% Change") %>% 
  text_transform(locations = cells_body(columns = vars(ggplot)),
                 fn = function(x) {map(tibble_plot$plot, ggplot_image, height = px(20), aspect_ratio = 5)}) %>% 
  tab_source_note(md("**Table**: @solomon_1195  | **Data**: Urban Institute<br>**Inspiration**: @thomas_mock")) %>% 
  gtsave("SWDChallenge.png")

