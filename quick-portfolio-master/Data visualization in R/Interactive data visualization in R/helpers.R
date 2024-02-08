

library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(forcats)
library(ggplot2)
library(kableExtra)
library(areaplot)
library(DT)
library(data.table)

fitness_members <- read_csv("fitness_members.csv")
fitness_tracking <- read_csv("fitness_tracking.csv")

categorical_variables <- c("bmi_category", "gender", "m_category")
numerical_variables <-  c("bmi_vs_baseline", "bmi", "weight")

categorical_labels <- c(
  "bmi_category" = "BMI Category",
  "gender" = "Gender",
  "m_category" = "M Category"
)

numerical_labels <- c(
  "weight" = "Weight",
  "bmi" = "BMI",
  "bmi_vs_baseline" = "BMI vs Baseline"
)

fitness_members_tracking<- fitness_members %>%
  left_join(fitness_tracking, by=c("id"))

fitness_long<-fitness_members_tracking %>%
  select(-recommendation_from, -registration_date, -birth_date) %>%
  rename(wk_000=weight) %>%
  pivot_longer(
    cols=starts_with("wk"),
    names_to= "week",
    names_prefix="wk_",
    values_to="weight",
    values_drop_na = TRUE) %>%
  relocate(m_category, .before=gender) %>%
  mutate(
    bmi = round(weight / (height / 100)^2, 2),
    bmi_category = case_when(
      bmi <= 18.5 ~ "Underweight",
      bmi <= 25.0 ~ "Healthy",
      bmi <= 30.0 ~ "Overweight",
      TRUE ~ "Obese"
    )
  )%>%
  group_by(id) %>%
  mutate(bmi_vs_baseline = round(100 * bmi / head(bmi, 1), 1)) %>%
  ungroup() %>%
  relocate(bmi_vs_baseline, .before = bmi_category) %>%
  select(-height) %>%
  mutate(
        week=as.numeric(week),
         m_category=factor(m_category,levels=c("Economy","Balance", "Premium")),
         bmi_category=factor(bmi_category,levels=c("Underweight","Healthy", "Overweight","Obese"))
        ) %>%
  drop_na()

output_graph1<- function(
  fitness_data = fitness_long,
  variable = "bmi_category"
) {
  fitness_data %>%
    ungroup() %>%
    count(.data[[variable]], week, name="Count") %>%
    filter(week==0) %>%
    select(-week) %>%
    ggplot(aes(x=.data[[variable]])) +
    geom_bar(aes(y=Count, fill=.data[[variable]]), stat = "identity")+
    theme(legend.position = "none")+
    labs(x = categorical_labels[variable], y = "Count")
}

output_graph2<- function(
  fitness_data = fitness_long,
  variable_1 = "bmi_category", #reacts to the user input
  variable_2 = "gender", #reacts to the user input
  weeks=0
){
fitness_data %>%
    ungroup() %>%
    filter(week %in% weeks) %>%
    count(.data[[variable_1]], .data[[variable_2]], name="Count") %>%
    group_by(.data[[variable_1]]) %>%
    mutate(Percent=round(Count/sum(Count)*100,1)) %>%
    ggplot(aes(x=.data[[variable_1]], y=Percent, fill=.data[[variable_2]])) +
    geom_bar(aes(y=Percent), stat = "identity", position="stack")+
    labs(x = categorical_labels[variable_1], y = "Percent",
         fill=categorical_labels[variable_2])
}

output_table1<- function(
  fitness_data = fitness_long,
  variable = "bmi_category"
) {
  fitness_data %>%
  count(.data[[variable]], week, name="N") %>%
  group_by(week) %>%
  mutate(Percent = paste0(round(N/sum(N)*100,1), " %")) %>%
  #dplyr::rename_with(toupper) %>%
  ungroup() %>%
  filter(week==0) %>%
  select(-week) %>%
  DT::datatable()
}


output_table2<- function(
  fitness_data = fitness_long,
  variable_1 = "bmi_category", #reacts to the user input
  variable_2 = "gender", #reacts to the user input
  weeks = 0
) {
  fitness_data %>%
    ungroup() %>%
    filter(week %in% weeks) %>%
    count(.data[[variable_1]], .data[[variable_2]], name="Count") %>%
    group_by(.data[[variable_1]]) %>%
    mutate(Percent = paste0(round(Count/sum(Count)*100,1), " %")) %>%
    dplyr::rename_with(toupper) %>%
    ungroup()
    #DT::datatable()
  # Reorder & capetalize column names
 }

fitness_data <- fitness_long
 numerical_variables <-  c("bmi_vs_baseline", "bmi", "weight")
 id_vector<-fitness_long %>%
   pull(id) %>%
   unique()
 ids<-c("000001", "000042", "000117", "000221", "000300", "000058")
 week<-fitness_data$week %>% unique()

output_graph3<-function(
  fitness_data = fitness_long,
  variable_ids=c("000001", "000042", "000117", "000221", "000300", "000058"),
  variable_3 ="bmi_vs_baseline",
  variable_week= 0:52
){

  fitness_data <- fitness_data %>%
    filter(week %in% variable_week)

  filtered_fitness_data <- fitness_data %>%
    filter(id %in% variable_ids) #I am creating for easiness the filtered_fitness_data before, so to use in the second geom_line

  fitness_data %>%
    ggplot() +
    geom_line(aes(x = week, y = .data[[variable_3]], group = id), alpha = 0.05) + # why group by id here
    geom_line(
      aes(x = week, y = .data[[variable_3]], color = id),
      data = filtered_fitness_data, size = 0.7 #here using filtered_data
    ) +
    facet_grid(rows = vars(gender), cols = vars(m_category)) +
    labs(x = "Week", y = numerical_labels[variable_3], color = "ID")

}

# Table for Part  III

output_table3<-function(
  fitness_data = fitness_long,
  variable_ids=c("000001", "000042", "000117", "000221", "000300", "000058"),
  variable_3 ="bmi_vs_baseline",
  variable_week= 0:52
){
  fitness_data <- fitness_data %>%
    filter(week %in% variable_week)

  filtered_fitness_data <- fitness_data %>%
    filter(id %in% variable_ids) #I am creating for easiness the filtered_fitness_data before, so to use in the second geom_line

  fitness_data %>%
    ungroup() %>%
    filter(week %in% variable_week) %>%
    filter(id %in% variable_ids) %>%
    janitor::clean_names() %>%
    ungroup() %>%
    dplyr::rename_with(toupper) %>%
    DT::datatable()

}

# Plot for Part IV

 week_vector<-0:52 #reacts to the user input

output_graph4<- function(fitness_data = fitness_long,
                          variable_4 = "bmi_category",
                          variable_week = c("0", "3", "6")){
  fitness_data %>%
    ungroup() %>%
    filter(week %in% variable_week) %>%
    count(.data[[variable_4]], week, name = "Count") %>%
    group_by(.data[[variable_4]], week) %>%
    ggplot(aes(x=.data[[variable_4]], y=Count)) +
    geom_bar(aes(fill=.data[[variable_4]]), stat = "identity")+
    facet_wrap(vars(week))+
    labs(x = categorical_labels[variable_4], y = "Count")+
    theme(legend.position = "none")
}

# Table for Part  IV

output_table4<-function(
  fitness_data = fitness_long,
  variable_4 ="bmi_category" , #reacts to the user input
  variable_week= c("0", "3", "6")#reacts to the user input
){ fitness_long %>%
    ungroup() %>%
    filter(week %in% variable_week) %>%
    group_by(.data[[variable_4]], week) %>%
    count(.data[[variable_4]],week,name="N") %>%
    mutate(Percent = paste0(round(N/sum(N)*100,1), " %")) %>%
    janitor::clean_names() %>%
    ungroup() %>%
    dplyr::relocate(.data[[variable_4]] ,.before = NULL, .after = week) %>%
    dplyr::rename_with(toupper) %>%
    DT::datatable()
    #DT::datatable(colnames = categorical_labels[variable_4], 'Week', 'N', 'Percent') @unable to capetalize
}

# # Plot for Part V

output_graph5<-function(
  fitness_data = fitness_long,
  variable_5 = "bmi_category", #reacts to the user input
  variable_6 = "gender", #reacts to the user input
  variable_week = 10:20 #reacts to the user input
){
variable_7<-categorical_variables[categorical_variables!=variable_5 &
                                    categorical_variables!=variable_6]

fitness_data%>%
  ungroup() %>%
  filter(week %in% variable_week,) %>%
  count(.data[[variable_5]],.data[[variable_7]],week, .data[[variable_6]],  name="Count") %>%
  group_by(week,.data[[variable_6]],  .data[[variable_7]]) %>%
  mutate(Percent = round(Count/sum(Count),2)) %>%
  ggplot(aes(x=week, y=Percent)) +
  geom_area(aes(fill=as.character(.data[[variable_5]])), alpha=1)+
  facet_grid(rows=vars(.data[[variable_6]]), cols=vars(.data[[variable_7]]))+
  labs(x = "Week", y = "Percent",
       fill=categorical_labels[variable_5])
}

output_table5 <- function(fitness_data = fitness_long,
                          variable_5 = "bmi_category",
                          variable_6 = "gender",
                          variable_week = c(10, 20))
{
  variable_7 <-
    categorical_variables[categorical_variables != variable_5 &
                            categorical_variables != variable_6]

  fitness_long %>%
    ungroup() %>%
    filter(week %between% variable_week) %>%
    count(.data[[variable_5]], .data[[variable_7]], week, .data[[variable_6]],  name =
            "N") %>%
    mutate(Percent = round(N / sum(N), 2)) %>%
    dplyr::rename_with(toupper) %>%
    DT::datatable()
}



## Part VI

output_graph6<-function(
  fitness_data = fitness_long,
  variable_7 = "bmi_category", #reacts to the user input
  variable_8 = "gender", #reacts to the user input
  variable_week = 0:3 #reacts to the user input
){
fitness_data  %>%
  ungroup() %>%
  filter(week %in% variable_week) %>%
  count(.data[[variable_7]],week, .data[[variable_8]],  name="N") %>%
  group_by(week, .data[[variable_7]]) %>%
  mutate(Percent = round(N/sum(N),2)) %>%
  ggplot(aes(x=.data[[variable_7]], y=Percent,fill = .data[[variable_8]])) +
  geom_bar(stat = "identity")+
  facet_wrap(vars(week))+
  theme_bw()+
    labs(x = categorical_labels[variable_7], y = "Percent",
         fill=categorical_labels[variable_8])
}

output_table6 <- function(fitness_data = fitness_long,
                          variable_7 = "bmi_category",
                          variable_8 = "gender",
                          variable_week = 0:3)
{

  fitness_data  %>%
    ungroup() %>%
    filter(week %in% variable_week) %>%
    count(.data[[variable_7]], week, .data[[variable_8]],  name = "N") %>%
    group_by(week, .data[[variable_7]]) %>%
    mutate(Percent = round(N / sum(N), 2)) %>%
    dplyr::rename_with(toupper) %>%
    DT::datatable()

}
