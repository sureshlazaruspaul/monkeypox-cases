library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

wmap <- map_data("world")

wmap$region <- recode(wmap$region,
                      "USA" = "United States",
                      "UK" = "England")

fill_colors <-
  c("thistle", "blue", "violet", "pink", "green", "red", "yellow", 
    "orange", "gray", "palegreen", "palesky", "darkgreen", "magenta", 
    "skyblue", "darkbrown", "cadetblue", "cyan", "darkseagreen", "hotpink", 
    "indianred", "midnightblue", "slateblue", "turquoise", "violetred")

file_url <-
  "https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv"

mpoxdat <- subset(read.csv(file_url,
                           header = TRUE,
                           na.strings = c("", ".", "NA")),
                  Status %in% "confirmed") %>%
  mutate(Date_confirmation = as.Date(Date_confirmation))

mpoxdat1 <- mpoxdat %>%
  group_by(Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup()

#################################################################################

ui <- fluidPage(
  sliderInput("topx", "Top N Countries:", min = 1, max = 15, value = 10),
  textOutput("count"),
  plotOutput("plot"),
  plotOutput("plot1"),
  plotOutput("plot2")
)

server <- function(input, output, session) {
  
  output$count <- renderText({
    paste("Total number of confirmed world-wide cases, as of",
          paste0(format(Sys.time(), "%a %b %d %X %Y"), ","),
          "is",
          nrow(mpoxdat))
  })
  
  top_list <- reactive(
    mpoxdat1 %>%
      top_n(input$topx, n) %>%
      mutate(
        Country1 = Country
      ) %>%
      select(-c("n"))
  )
  
  mpoxdat2 <- reactive(
    merge(x = mpoxdat,
          y = top_list(),
          by = "Country",
          all.x = TRUE) %>%
      mutate(
        Region = ifelse(!is.na(Country1), Country, "Rest of The World")
      ) 
  )
  
  output$plot <- renderPlot({
    ggplot(mpoxdat2(),
           mapping = aes(x = Region)) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      ylim(0, max(mpoxdat1$n) + 500) +
      labs(
        title = "Monkeypox cases by Country",
        subtitle = "Algorithm Basics Infographics",
        x = "Country/Region",
        y = "Number of Confirmed Cases"
      ) +
      geom_bar(aes(fill = Region)) + theme_classic()
  }, res = 75)
  
  
  trend_dat <- reactive(
    mpoxdat %>%
      group_by(Date_confirmation) %>%
      summarize(ncases = n()) %>%
      ungroup() %>%
      arrange(Date_confirmation) %>%
      mutate(total_cases = cumsum(ncases))
  )
  
  output$plot1 <- renderPlot({
    ggplot(trend_dat(),
           mapping = aes(x = as.Date(Date_confirmation),
                         y = total_cases)) +
      geom_line() +
      geom_text(
        aes(label = 
              ifelse(
                trend_dat()$total_cases == max(trend_dat()$total_cases),
                trend_dat()$total_cases,"")), vjust = -1) +
      ylim(0, max(trend_dat()$total_cases) + 500) +
      labs(
        title = "Trend line",
        subtitle = "Algorithm Basics Infographics",
        caption = paste("Data source: https://github.com/globaldothealth/monkeypox",
                        "",
                        paste("Period:",
                              min(as.Date(trend_dat()$Date_confirmation)), "to",
                              max(as.Date(trend_dat()$Date_confirmation))),
                        sep="\n"),
        x = "Country/Region",
        y = "Number of Confirmed Cases"
      ) + theme_bw()
  }, res = 75)
  
  wmap1 <- reactive(
    mutate(wmap,
           colorfill = ifelse(region %in%
                                mpoxdat1[1:input$topx, ]$Country,
                              "red",
                              "white"))
  )
  
  output$plot2 <- renderPlot({
    ggplot(wmap1(),
           aes(long, lat, fill = colorfill, group = group)) +
      geom_polygon(colour = "gray") +
      labs(
        title = "Geomap of countries with active cases",
        subtitle = "Algorithm Basics Infographics",
        caption = "Data source: https://github.com/globaldothealth/monkeypox",
        x = "Longitude",
        y = "Latitude"
      ) +
      scale_fill_identity() + theme_classic()
  }, res = 75)
  
  
}

shinyApp(ui, server)
