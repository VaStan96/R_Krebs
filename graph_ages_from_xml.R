library(xml2)
library(lubridate)
library(dplyr)
library(ggplot2)
library(patchwork)

# get data from XML
# VARIANT 1

# patients_xml <- read_xml('data/patients.xml')
# patients_births_xml <- xml_find_all(patients_xml, './/patient/date_of_birth')
# patients_births <- xml_text(patients_births_xml)
# dats <- ymd(patients_births)
# ages <- interval(dats, today())/years(1)
# ages <- floor(ages)


# VARIANT 2

ages <- read_xml('data/patients.xml') %>% # get data
        xml_find_all('.//patient/date_of_birth') %>% #get node_set with dates
        xml_text() %>% #mutate in vector with text
        ymd %>% #mutate in date
        (\(dates) interval(dates, today()) / years(1))() %>% #anonim func for alt
        floor() #round

#test values
# ages <- c(10,10,10,10,10,12,30,30,50,10)

custom_colors <- colorRampPalette(c('white', 'yellow', 'red'))(100)


df_ages <- data.frame(age = ages) #make dataframe from vector
#associate age with color
df_ages <- mutate(df_ages, fill_color = custom_colors[age])
df_ages <- arrange(df_ages, age)

df_counts <- count(df_ages, age) #dataframe with grooping and count

plot1 <- ggplot(df_ages, aes(x = age)) + 
  geom_density(fill = 'lightblue', color = 'black') +
  labs(title = 'density diagramm', x = 'ages', y = 'density')

plot2 <- ggplot(df_ages, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "histogramm", x = "ages", y = "freq")

plot3 <- ggplot(df_ages, aes(x = "", y = age)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot", x = "", y = "ages")

plot4 <- ggplot(df_counts, aes(x = '', y = n, fill = factor(age))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  geom_text(aes(label=paste(n)), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = 'black') +
  scale_fill_manual(values = df_ages$fill_color) +
  
  labs(title = "Pie chart", x = "", y = "") +
  theme_void()

plot1 + plot2 + plot3 + plot4 + plot_layout(ncol = 2)
df_counts$n
       