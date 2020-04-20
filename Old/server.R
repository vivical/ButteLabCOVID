library(usmap)
stateDat <- read.csv(text = RCurl::getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
function(input, output) {
  output$usaPlot <- renderPlot({
    if (input$stateData == "All") {
      dat <- stateDat
      dat <- subset(dat, as.character(date) == input$date)
    } else {
      dat <- subset(stateDat, state == input$stateData)
      dat <- subset(dat, as.character(date) == input$date)
    }
    plot_usmap(
      data = dat,
      values = "cases"
    ) +
      ggplot2::scale_fill_viridis_c(name = "cases") +
      ggplot2::theme(legend.position = "right")
  })
}
