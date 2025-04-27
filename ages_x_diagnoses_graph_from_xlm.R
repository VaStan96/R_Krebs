library(xml2)
library(lubridate)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

source('utils.R')

# get data from xml
cases_xml <- read_xml('data/cases.xml')
# convert to dataframe
cases_df <- xml_to_df(cases_xml)
# convert id in INT and dates in DATE
cases_df = mutate(cases_df, 
                  date_of_birth = ymd(date_of_birth), 
                  diagnosis_date = ymd(diagnosis_date), 
                  treatment_date = ymd(treatment_date),
                  patient_id = as.integer(patient_id),
                  gender = factor(gender, levels = c('M', 'F')))

# add column with alt
cases_df <- mutate(cases_df, age = floor(interval(date_of_birth, today())/years(1)))
#make factor for grouping age
cases_df <- mutate(cases_df, age_groups = cut(age,
                                              breaks = c(-Inf, 19, 29, 39, 49, 59, Inf),
                                              labels = c("20-", "20+", "30+", "40+", "50+", "60+"),
                                              right = TRUE))

#-------------------------------------------------------------------------------------------------------------------------------
# Plot 1
# Histogramm with grouping by gender- and alt-groups
plot1 <- ggplot(cases_df, aes(x = age_groups, fill = gender)) +                           
                  
                  geom_bar(position = position_dodge2(preserve = "single", width = 0.7),  #all column for gender
                           color = "black", width = 0.5) +                                #color for border and space
                  
                  geom_text(stat = 'count', aes(label = ..count..),                       #count the number for groups and gender combination and place in "..count.."
                            position = position_dodge2(width = 0.5),                      #place label in center for column
                            vjust = 2, size = 4, color = "black") +                       #vertical position, size and color for labels
                  
                  labs(title = "Gender- and alt-groups", x = "ages", y = "count", fill = "Gender",
                       subtitle = "Histogramm with grouping by gender- and alt-groups") +
                  
                  scale_x_discrete(drop = FALSE) +                                        #show groups without values
                  scale_fill_manual(values = c('M' = 'lightblue', 'F' = 'gold')) +        #color for column
                  
                  theme_minimal() +
                  theme(
                    panel.grid.major.x = element_blank(),                                 # delete main vertical linies of grid
                    panel.grid.minor.x = element_blank(),                                 # and extra
                    panel.grid.major.y = element_line(color = "gray90")                   # set horizontal
                  )


#-------------------------------------------------------------------------------------------------------------------------------
# Plot 2
# make new datafreim for pie-chart
# only with code = "C*" and with aggregation 
krebs_code_df <- count(filter(cases_df, grepl("^C", icd10_code)), icd10_code, disease_name)
# pie-chart with type of Krebs
plot2 <- ggplot(krebs_code_df, aes(x = 1, y = n, fill = disease_name)) +                 #pie make always with aggregation data and x="", y=n
                  
                  geom_bar(stat = 'identity') +                                          #"identity" for aggregation data
                                  
                  coord_polar(theta = 'y') +                                             #pie make always with theta = 'y'
                  
                  geom_text(aes(label = paste0(n/sum(n)*100, "%\n", n)),                 #set label for column (% and count)
                            stat = 'identity',                                           #aggregation data
                            position = position_stack(vjust = 0.5),                      #in center 
                            size = 4, color = 'black') +                                 #size and color
  
                  xlim(c(-0.5, 1.5)) +                                                    #make the hole
  
                  labs(title = "Type of Krebs", x = "", y = "", fill = "Diseases",
                       subtitle = "Pie-chart with distribution of patients by Krebs-type") +
  
                  theme_void()

#-------------------------------------------------------------------------------------------------------------------------------
# Plot 3
# count of diagnosis (all and krebs) per year
count_diagnosis_df <- count(cases_df, year(diagnosis_date)) %>% 
  mutate(krebs = count(filter(cases_df, grepl("^C", icd10_code)), year(diagnosis_date))$n) %>%
  rename('year' = 'year(diagnosis_date)', 'all' = 'n')

# Variant with 2 linien without legend
# plot3 <- ggplot(count_diagnosis_df, aes(x = year)) +                                               #Years by x-axis
#
#                 geom_line(aes(y = n), color = 'blue', size = 1.5) +                                #first linie with point
#                 geom_point(aes(y = n),color = 'darkblue', size = 3) +
#
#                 geom_line(aes(y = n_krebs), color = 'red', size = 1.5) +                           #second lines
#                 geom_point(aes(y = n_krebs),color = 'darkred', size = 3) +
#
#                 scale_x_continuous(breaks = count_diagnosis_df$year) +                             #all years
#                 scale_y_continuous(breaks = seq(0, max(count_diagnosis_df$n) + 1, by = 1)) +       #scale 0-max+1 and every step
#
#                 labs(title = "Count of diagnosis per years", x = "Year", y = "Count",
#                      fill = "Type", subtitle = "Count of all and krebs diagnosis per years") +
#
#                 theme_minimal()

# make new df with column type (all or krebs) and count for every type
count_diagnosis_lang <- pivot_longer(count_diagnosis_df, cols = c(all, krebs), names_to = 'type', values_to = 'count')

# make new df with smoothed data
smoothed_spline <- group_by(count_diagnosis_lang, type) %>% #grouping by type
  group_split() %>% #spliting by groups
  lapply(function(df) { #every group in anonym func, return list
    spline_points <- as.data.frame(spline(df$year, df$count, n = 100)) #interpolieren data by 100 points
    spline_points$type <- unique(df$type) 
    return(spline_points)
  }) %>%
  bind_rows() #make df

plot3 <- ggplot(smoothed_spline, aes(color = type)) +                                               #color - lines, fill - shape
  
                geom_area(aes(x = x, y = y, fill = type, color = type, linetype = type),                             #alpha-transparency, NA-without lines
                          alpha = 0.3, position = 'identity') +                                     #position: 'identity'-aggregation, 'stack'-summ all
                                                                                                                       
          
                geom_point(data = count_diagnosis_lang, aes(x = year, y = count, fill = type), 
                           shape = 21 , size = 4) +                                                 #shape = 21 is round
                
      
                scale_color_manual(values = c('all' = 'blue', 'krebs' = 'red')) +                   #colors and types depends on type
                scale_fill_manual(values = c('all' = 'darkblue', 'krebs' = 'darkred')) +
                scale_linetype_manual(values = c('all' = 'solid', 'krebs' = 'dashed')) +
                
                scale_x_continuous(breaks = count_diagnosis_lang$year) +                            #all years
                scale_y_continuous(breaks = seq(0, max(count_diagnosis_lang$count) + 1, by = 1)) +  #scale 0-max+1 and every step
  
                labs(title = "Count of diagnosis per years", x = "Year", y = "Count",
                     color = "Type", fill = "Type", linetype = "Type",                              #summ all legends
                     subtitle = "Count of all and krebs diagnosis per years") +
  
                theme_minimal() +
                theme(
                  panel.grid.major.x = element_line(color = "gray"),                                # set main vertical linies of grid
                  panel.grid.minor.x = element_blank(),                                             # and extra
                  panel.grid.major.y = element_line(color = "gray"),                                # set horizontal
                  panel.grid.minor.y = element_blank(),                                             
                )

#-----------------panel.grid.minor.y = #-------------------------------------------------------------------------------------------------------------------------------
# Plot 4
# Heatmap diseases x treatments

#Variant 1
#use geom_bin2d()
# plot4 <- ggplot(cases_df, aes(x=disease_name, y = treatment_type)) +
#   geom_bin2d() +
#   stat_bin2d(geom='text', aes(label = after_stat(count)), color = 'black', size = 3) +
#   labs(title = "Heatmap of diseases by treatments", x = "Diseases", 
#        y = "Treatments") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   coord_fixed(ratio = 1) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   )

# Variant 2
#use geom_tile() and aggregieren data

#make new df with aggregieren data
binn_df <- count(cases_df, disease_name, treatment_type)

plot4 <- ggplot(binn_df, aes(x=disease_name, y = treatment_type, fill = n)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = n), color = 'black', size = 3) +
  labs(title = "Heatmap of diseases by treatments", x = "Diseases", 
       y = "Treatments", fill = 'Count of matches') +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



#show all
plot1 + plot2 + plot3 + plot4 + plot_layout(ncol = 2)

