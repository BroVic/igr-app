# server.R

library(shiny)

source("helpers.R")

igrbank <- readRDS(file = "data/igr-tidy.rds")

shinyServer(function(input, output) {

  dataInput <- reactive({
      revenues(igrbank, office = input$state ) # source code at helpers.R
    })
  
  names.arg <- levels(igrbank$revenue.cat)
  len <- length(names.arg)
  
  output$chart1 <- renderPlot({
    par(mar = c(12, 6, 4, 5) + 0.2)
    barplot(dataInput()/1000, ylim = c(0, max(dataInput()/1000)*1.25), xaxt = "n",
            xlab = "", yaxt = "n", ylab = "", col = "red",
            main = paste("Revenue from", input$state))
    axis(2, las = 2)
    text(seq(from = 1, to = len, by = 1), par("usr")[3] - 0.5, srt = 45, adj = 1,
         labels = names.arg, xpd = TRUE)
    grid(NA, ny = NULL, col = "gray", lty = "dashed")
    mtext(1, text = "Payment Categories", line = 11)
    mtext(2, text = "Amount (NGN '000)", line = 5)
  })

})