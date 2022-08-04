library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

wmap <- map_data("world")

wmap$region <- recode(wmap$region,
                      "USA" = "United States",
                      "UK" = "England")

file_url <-
  "https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv"

mpoxdat <- subset(read.csv(file_url,
                           header = TRUE,
                           na.strings = c("", ".", "NA")),
                  Status %in% "confirmed")

mpoxdat1 <- mpoxdat %>%
  group_by(Country) %>%
  count(Country, sort = TRUE)

ui <- fluidPage(
  sliderInput("topx", "Top N Countries:", min = 1, max = 10, value = 5),
  textOutput("count"),
  plotOutput("plot"),
  plotOutput("plot1")
)

server <- function(input, output, session) {

  wmap1 <- reactive(
    mutate(wmap,
           fill = ifelse(region %in%
                           mpoxdat1[1:input$topx, ]$Country,
                         "red",
                         "white"))
  )

  mpoxdat2 <- reactive(
    merge(x = mpoxdat,
          y = mpoxdat1[1:input$topx, ],
          by = "Country",
          all.x = TRUE) %>%
      mutate(Region = ifelse(!is.na(n), Country, "Rest of The World"))
  )

  output$plot <- renderPlot({
    ggplot(mpoxdat2(),
           mapping = aes(x = Region)) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      ylim(0, max(mpoxdat1$n) + 25) +
      labs(
        title = "Monkeypox cases by Country",
        subtitle = "Algorithm Basics Infographics",
        caption = "Data source: https://github.com/globaldothealth/monkeypox",
        x = "Country/Region",
        y = "Number of Confirmed Cases"
      ) +
      geom_bar(aes(fill = Region)) + theme_classic()
  }, res = 96)

  output$plot1 <- renderPlot({
    ggplot(wmap1(),
           aes(long, lat, fill = fill, group = group)) +
      geom_polygon(colour = "gray") +
      labs(
        title = "Spread vector",
        subtitle = "Algorithm Basics Infographics",
        caption = "Data source: https://github.com/globaldothealth/monkeypox",
        x = "Longitude",
        y = "Latitude"
      ) +
      scale_fill_identity() + theme_classic()
  }, res = 96)

  output$count <- renderText({
    paste("Total number of confirmed world-wide cases, as of",
          paste0(format(Sys.time(), "%a %b %d %X %Y"), ","),
          "is",
          nrow(mpoxdat))
  })

}

shinyApp(ui, server)
