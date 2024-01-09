# Title: Does Alcohol Solve All Your Problems?
# Author (in alphabetical order): [Group 8] Xiaoya Gao, Keer Ma, Yuanyi Xie, Weikun Zhuang

# Library
library(shiny)
library(DT)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Import Data
## Happiness Data
df_15 <- read_csv("2015.csv")
df_16 <- read_csv("2016.csv")
df_17 <- read_csv("2017.csv")
df_18 <- read_csv("2018.csv")
df_19 <- read_csv("2019.csv")
## Alcohol Data
alc_df <- read_csv("alcohol.csv")


# Tidy Data

## Happiness Scores
### Only keep Country/Happiness.Score/Life.Expectancy, add year
df_15 <- df_15 %>% select(1, 4)
names(df_15)[1] <- ("Country")
names(df_15)[2] <- ("Score")
df_16 <- df_16 %>% select(1, 4)
names(df_16)[1] <- ("Country")
names(df_16)[2] <- ("Score")
df_17 <- df_17 %>% select(1, 3,)
names(df_17)[1] <- ("Country")
names(df_17)[2] <- ("Score")
df_18 <- df_18 %>% select(2, 3)
names(df_18)[1] <- ("Country")
names(df_18)[2] <- ("Score")
df_19 <- df_19 %>% select(2, 3)
names(df_19)[1] <- ("Country")
names(df_19)[2] <- ("Score")
### Year Column and merge all df together
df_15 <- df_15 %>% mutate(Year = 2015)
df_16 <- df_16 %>% mutate(Year = 2016)
df_17 <- df_17 %>% mutate(Year = 2017)
df_18 <- df_18 %>% mutate(Year = 2018)
df_19 <- df_19 %>% mutate(Year = 2019)
Happiness.score <- bind_rows(df_15, df_16, df_17, df_18, df_19) %>% arrange(desc(Score))

## Alcohol Consumption
### Drop some redundant columns
drop_list <- c("IndicatorCode", "Indicator","ValueType","Location type", "Period type",
               "IsLatestYear", "Dim1 type", "Dim2 type","Dim2","Dim2ValueCode",
               "Dim3 type","Dim3","Dim3ValueCode", "DataSourceDimValueCode","DataSource",
               "FactValueNumericPrefix", "FactValueUoM", "FactValueNumericLowPrefix",
               "FactValueNumericHighPrefix", "FactValueTranslationID", "FactComments",
               "Language", "DateModified","Dim1ValueCode")
df = alc_df[,!(names(alc_df) %in% drop_list)]
df <- df[-c(19388,19235,18736,19229,18737),]
df2 <- df %>% filter(Period %in% (2015:2019)) %>% 
  select(-c(FactValueNumericLow,FactValueNumericHigh,Value)) %>% 
  spread(key=Dim1,value=FactValueNumeric)
names(df2)[names(df2) =="All types"] <-"All.types"
names(df2)[names(df2) =="Other alcoholic beverages"] <-"Other.alcoholic.beverages"
dfa <- df2 %>% arrange(desc(All.types))
dfb <- df2 %>% arrange(desc(Beer))
dfw <- df2 %>% arrange(desc(Wine))
dfs <- df2 %>% arrange(desc(Spirits))
dfo <- df2 %>% arrange(desc(Other.alcoholic.beverages))

## Radar
df30 = alc_df[,!(names(alc_df) %in% drop_list)]
df30 <- df30[-c(19388,19235,18736,19229,18737),]
df30 <- df30 %>% filter(Period %in% (2015:2019)) %>% 
  select(-c(FactValueNumericLow,FactValueNumericHigh,Value)) %>% 
  spread(key=Dim1,value=FactValueNumeric)
df30 <- df30 %>% rename(Year = Period, Country = Location, All = 'All types', 
                        Other =  "Other alcoholic beverages")
df30 <- df30 %>% mutate(Country = 
                          replace(Country, Country == "Viet Nam", "Vietnam"))
df30 <- df30 %>%
  mutate(Country = case_when(Country == "Bolivia (Plurinational State of)" ~ "Bolivia", 
                             Country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
                             Country == "Lao People's Democratic Republic" ~ "Laos",
                             Country == "Micronesia (Federated States of)" ~ "Micronesia",
                             Country == "Democratic People's Republic of Korea" ~ "North Korea",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "Republic of Korea"  ~ "South Korea",
                             Country == "The former Yugoslav Republic of Macedonia" ~ "Macedonia",
                             Country == "Syrian Arab Republic" ~ "Syria",
                             Country == "Iran (Islamic Republic of)" ~ "Iran",
                             Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                             Country == "United States of America" ~ "USA",
                             Country == "United Republic of Tanzania" ~ "Tanzania",
                             Country == "Republic of Moldova" ~ "Moldova",
                             TRUE ~ Country
  ))
avg_Country <- rep("Average Country", 5)
avg <- df30 %>% group_by(Year) %>% summarise(avg_All = mean(All), avg_Beer = mean(Beer),
                                             avg_Spirits = mean(Spirits), avg_Wine = mean(Wine),
                                             avg_Other = mean(Other))
avg_df <- cbind(avg_Country, avg) %>% 
  rename(Country = avg_Country, All = avg_All, Beer = avg_Beer, 
         Other = avg_Other, Spirits = avg_Spirits, Wine = avg_Wine)
country_list <- unique(df30$Country)

## Relationship
df99 <- dfa[,c(4,5,6)]
names(df99)[names(df99) == "Location"] <- "Country"
names(df99)[names(df99) == "Period"] <- "Year"
names(df99)[names(df99) == "All.types"] <- "Alcohol"
df99$Country[which(df99$Country == "United States of America")] <- "United States"
df99$Country[which(df99$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
df99$Country[which(df99$Country == "Russian Federation")] <- "Russia"
Relationship <- merge(Happiness.score, df99, by.x = c("Country", "Year"))


# Function
## Happiness Function
chartCol <- function(year,b) {
    Happiness.score %>%
      filter(Year == year) %>%
      slice_head(n=b) %>%
      ggplot(width = 0.2,
             aes(reorder(Country, Score), Score)) +
      geom_col(aes(fill = Country)) +
      geom_text(aes(label = Score), hjust = 1.5, size = 6)+
      coord_flip() +
      xlab("Country")+
      ylab("Happiness Score")+
      labs(title = "Top happiest countries")+
      theme(plot.title = element_text(size = 20), 
            axis.title.x = element_text(size = 15), 
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 12),
            legend.position = "None")
}

## Alcohol Consumption Function
chartCol1 <- function(year,category,b) { 
  if(category == "All.types"){
    dfa %>%
      filter(Period == year) %>%
      slice_head(n=b) %>%
      ggplot(width = 0.2, 
             aes(reorder(Location, All.types), All.types)) +
      geom_col(aes(fill = Location)) +
      geom_text(aes(label = All.types), hjust = 1.5, size = 6)+
      coord_flip() +
      xlab("Country")+
      ylab("All types of alcohol")+
      labs(title = "Top all types of alcohol consumption countries")+
      theme(plot.title = element_text(size = 20), 
            axis.title.x = element_text(size = 15), 
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 12),
            legend.position = "None")
    }
  else{if(category == "Beer"){
    dfb %>%
      filter(Period == year) %>%
      slice_head(n=b) %>%
      ggplot(width = 0.2, 
             aes(reorder(Location, Beer), Beer)) +
      geom_col(aes(fill = Location)) +
      geom_text(aes(label = All.types), hjust = 1.5, size = 6)+
      coord_flip() +
      xlab("Country")+
      ylab("Beer")+
      labs(title = "Top Beer consumption countries")+
      theme(plot.title = element_text(size = 20), 
            axis.title.x = element_text(size = 15), 
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 12),
            legend.position = "None")
    }
    else{if(category == "Spirits"){
      dfs %>%
        filter(Period == year) %>%
        slice_head(n=b) %>%
        ggplot(width = 0.2, 
               aes(reorder(Location, Spirits), Spirits)) +
        geom_col(aes(fill = Location)) +
        geom_text(aes(label = All.types), hjust = 1.5, size = 6)+
        coord_flip() +
        xlab("Country")+
        ylab("Spirits")+
        labs(title = "Top Spirits consumption countries")+
        theme(plot.title = element_text(size = 20), 
              axis.title.x = element_text(size = 15), 
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 15),
              axis.text.y = element_text(size = 12),
              legend.position = "None")
      }
      else{if(category == "Wine"){
        dfw %>%
          filter(Period == year) %>%
          slice_head(n=b) %>%
          ggplot(width = 0.2, 
                 aes(reorder(Location, Wine), Wine)) +
          geom_col(aes(fill = Location)) +
          geom_text(aes(label = All.types), hjust = 1.5, size = 6)+
          coord_flip() +
          xlab("Country")+
          ylab("Wine")+
          labs(title = "Top Wine consumption countries")+
          theme(plot.title = element_text(size = 20), 
                axis.title.x = element_text(size = 15),
                axis.text.x = element_text(size = 12),
                axis.title.y = element_text(size = 15),
                axis.text.y = element_text(size = 12),
                legend.position = "None")}
        else{dfo %>%
            filter(Period == year) %>%
            slice_head(n=b) %>%
            ggplot( width = 0.2, 
                    aes(reorder(Location, Other.alcoholic.beverages), Other.alcoholic.beverages)) +
            geom_col(aes(fill = Location)) +
            geom_text(aes(label = All.types), hjust = 1.5, size = 6)+
            coord_flip() +
            xlab("Country")+
            ylab("Other alcoholic beverages")+
            labs(title = "Top other alcoholic beverages consumption countries")+
            theme(plot.title = element_text(size = 20), 
                  axis.title.x = element_text(size = 15),
                  axis.text.x = element_text(size = 12),
                  axis.title.y = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  legend.position = "None")
        }
      }
    }
  }
}

## Radar Function
coord_radar <- function (theta = "x", start = 0, direction = 1){
  theta <- match.arg(theta, c("x","y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, 
          start = start, direction = sign(direction), is_linear = function(coord) TRUE)
}
radar_plot <- function(data){
  ggplot(data, aes(x = Alc_type, y = Value)) +
    geom_polygon(aes(group = Country, color = Country), fill = NA, 
                 show.legend = FALSE, size = 2) +
    geom_line(aes(group=Country, color=Country), size = 2)+
    coord_radar() +
    labs(x="", y="", title = "Alcohol Consumption Pattern")
}
generate_radar_random <- function(data, year){
  random_indices <- sample(1:length(country_list), 6)
  random_df <- data %>% filter(Country %in% country_list[random_indices] & Year == year) %>%
    gather("Beer", "Other", "Spirits", "Wine", key = "Alc_type", value = Value)
  plot <- radar_plot(random_df)
  plot <- plot + facet_wrap(~ Country) + theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          axis.text.x  = element_text(size = rel(1.2)),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          legend.position="None") +
    scale_color_brewer(palette = "Set1")
  return (plot)
}
generate_radar_custom <- function(data, year, country){
  country_df <- data %>% filter(Country %in% country & Year == year) %>% select(c(4:10))
  avg_world <- avg_df %>% filter(Year == year)
  combine <- rbind(country_df, avg_world) %>% 
    gather("Beer", "Other", "Spirits", "Wine", key = "Alc_type", value = Value)
  plot <- radar_plot(combine)
  plot <- plot + theme_minimal() +
    theme(axis.text.x  = element_text(size = rel(1.2)),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank())
  return(plot)
}

## Relationship Function
chartCol3 <- function(country) {
  if(country %in% Relationship$Country){
    Relationship %>%
      filter(Country == country) %>%
      ggplot(width = 0.2,
             aes(x = Score, y = Year, size = Alcohol)) +
      geom_point(shape = 21,stroke = 3) +
      scale_size_continuous(range = c(2,15)) +
      coord_flip() +
      ylab("Year")+
      xlab("Happiness Score")+
      theme(plot.title = element_text(size = 20), 
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 12))+
      labs(size = "Alcohol Consumption", title = "Relationship Between Happiness Score and Alcohol Consumption")}
  else{
    Relationship %>%
      ggplot(width = 0.2,
             aes(x = Score, y = Year, size = Alcohol)) +
      geom_point(shape = 21,stroke = 3,position = "jitter") +
      scale_size_continuous(range = c(2,15)) +
      coord_flip() +
      ylab("Year")+
      xlab("Happiness Score")+
      theme(plot.title = element_text(size = 30), 
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 12))+
      labs(size = "Alcohol Consumption", title = "The dataset does not include this country or region.")
  }
}


# UI
ui <- navbarPage("Does Alcohol Solve All Your Problems?",
                 # Alcohol Consumption Page
                 tabPanel("Alcohol Consumption",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Period",
                                          label = "Year",
                                          choices = c(choose = "List of years...",
                                                      "2015", "2016", "2017", "2018", "2019")),
                              selectInput(inputId = "category",
                                          label = "Category",
                                          choices = c(choose = "Category...", "All.types","Wine",
                                                      "Beer", "Spirits", "Other.alcoholic.beverages")),
                              sliderInput("range1",
                                          label = "Top range",
                                          min = 1, max = 100, value = 30),
                              img(src ="alcohol.png",height = 200, width = 200)
                            ),
                            mainPanel(h1("Top Alcohol Consumption Countries"),
                                      p("2015-2019"),
                                      plotOutput(outputId = "plotChart2",
                                                 width = "100%", height = "1500px")
                          ))

                 ),
                 # Radar Page
                 tabPanel("Pattern",
                          sidebarLayout(sidebarPanel(
                            width = 2,
                            radioButtons(
                              inputId = "year1",
                              label = "Year",
                              choices = c("2015","2016", "2017", "2018", "2019")
                            ),
                            selectInput(
                              width = "100%",
                              inputId = "country1",
                              label = "List of Country",
                              choices = c(country_list[order(country_list)]),
                              selectize = FALSE
                            ),
                            actionButton(
                              inputId = "random1",
                              label = "Random Countries",
                              icon = icon("caret-square-right"),
                              width = "100%"
                            ),
                            img(src ="beer.png",height = 100, width = 200)
                            ),
                            mainPanel(
                              plotOutput(
                                "plotChart3",
                                width = "100%",
                                height = "700px"
                              ))
                          )),
                 
                 #Happiness Page
                 tabPanel("Happiness",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(
                                inputId = "year",
                                label = "Year",
                                choices = c("2015","2016", "2017", "2018", "2019")
                              ),
                              sliderInput("range",
                                          label = "Top range",
                                          min = 1, max = 100, value = 30),
                              img(src ="happy.png",height = 200, width = 200),
                            ),
                            mainPanel(h1("World Happiness Report"),
                                      p("2015-2019"),
                                      plotOutput(outputId = "plotChart1",
                                                 width = "100%", height = "1500px"))
                          )),
                 
                 # Relationship Page
                 tabPanel("Relationship",
                          sidebarLayout(sidebarPanel(
                            textInput("text", "Country or Region", value = "Albania"),
                            img(src ="relationship.png",height = 200, width = 200)
                          ),
                          mainPanel(
                            plotOutput(outputId = "plotChart4",
                                       width = "100%", height = "700px")
                          )
                          ))
                 
)

# Server
server <- function(input, output, session){
  # Happiness
  output$plotChart1 <- renderPlot({chartCol(input$year, input$range)})
  
  # Alcohol Consumption
  output$plotChart2 <- renderPlot({chartCol1(input$Period, input$category, input$range1)})
  
  #Relationship
  output$plotChart4 <- renderPlot({chartCol3(input$text)})
  
  # Radar
  values <- reactiveValues(year1 = NULL,country1 = NULL)
  observeEvent(input$year1,{
    values$year1 <- input$year1
  })
  observeEvent(input$country1,{
    values$country1 <- input$country1
    output$plotChart3  <- renderPlot({
      generate_radar_custom(df30, values$year1, values$country1)
    })
  })
  observeEvent(input$random1, {
    output$plotChart3  <- renderPlot({
      generate_radar_random(df30, values$year1)
    })
  })
}





shinyApp(ui=ui, server=server)