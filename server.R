
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source('globals.R')

shinyServer(function(input, output){
  ### Mana Curve Plot
  output$manacurve <- renderPlot({
    data.frame(drops = 1:8,
               number = c(input$d1, input$d2, input$d3, input$d4, 
                          input$d5, input$d6, input$d7, input$d8)
               ) %>% 
      ggplot() + geom_col(aes(x = drops, y = number)) +
      labs(title = "Mana Curve", y = "Count", x = "Converted Mana Cost") +
      scale_x_continuous(breaks = 1:8)
      })
  
  ### Curve Evaluations
  dl <- eventReactive(input$submit,{
    decklist(size = 40, curve = c(input$d1, input$d2, input$d3, input$d4, 
                                  input$d5, input$d6, input$d7, input$d8)
             )
    })
  
  output$landcount <- renderText({paste0("Land count: ",
                                         40 - sum(c(input$d1, input$d2, input$d3, input$d4, 
                                                    input$d5, input$d6, input$d7, input$d8)))})
  
  xc_sample <- reactive({
    set.seed(1337)
    curvesample(dl(), iter = 2500, turns = 20, ontheplay = TRUE)
  })
  
  mean_xc <- reactive({
    data.frame(turns = 1:20,
               mean = xc_sample() %>% rowMeans,
               CI_lower = sapply(X = seq_len(dim(xc_sample())[1]), FUN = function(i){t.test(xc_sample()[i,])$conf.int[1]}),
               CI_upper = sapply(X = seq_len(dim(xc_sample())[1]), FUN = function(i){t.test(xc_sample()[i,])$conf.int[2]})
    )
  })
  
  ### Executed Curve Plot
  output$execurve <- renderPlot({
    mean_xc() %>% filter(turns <= 15)%>% 
      ggplot() + 
      geom_errorbar(aes(x = turns, ymin = CI_lower, ymax = CI_upper), color = "darkgrey", width = 0.6) +
      geom_line(aes(x = turns, y = mean)) +
      geom_point(aes(x = turns, y = mean), size = 2) +
      labs(title = " ", y = "Mana spent", x = "Turn") +
      scale_x_continuous(breaks = 1:15) +
      coord_cartesian(ylim = c(0,max(5, max(mean_xc()$mean))))
  })
  
  xc_evals <- reactive({
    methods_list <- list("discount055", "discount06", "discount065", "discount07", "discount075", "discount08")
    optim_res <- list("discount055" = 3.515, 
                      "discount06"  = 4.135, 
                      "discount065" = 4.979, 
                      "discount07"  = 6.213, 
                      "discount075" = 8.199, 
                      "discount08"  = 11.323)
    lapply(methods_list, FUN = function(mth){
      evsample <- c(eval_csample(xc_sample(), method = mth)) / optim_res[[mth]]
      data.frame(method = mth, 
                 sample_mean = mean(evsample), 
                 CI_lower = t.test(evsample)$conf.int[1],
                 CI_upper = t.test(evsample)$conf.int[2]
                 ) %>% 
        mutate(score = 10 * min(sample_mean^pi, 1))
    }) %>% bind_rows
  })
  
  output$evaluation <- renderPlot({
    xc_evals() %>%
      mutate(score_text = sprintf("%.1f", score)) %>% 
      ggplot() +
      geom_col(aes(x = method, y = score, fill = score), width = 1.0) +
      labs(y = "Score", x = "Speed of Game") +
      coord_cartesian(ylim = c(0,10)) +
      scale_x_discrete(labels = c("discount055" = "FAST", 
                                  "discount06"  = "", 
                                  "discount065" = "", 
                                  "discount07"  = "", 
                                  "discount075" = "", 
                                  "discount08"  = "SLOW")) +
      scale_y_continuous(breaks = seq(0, 10, 2), minor_breaks = seq(0, 10, 1)) +
      scale_fill_gradient2(name = "Score", low = "#FF0000", high = "#01DF01", 
                           mid = "#F7FE2E", midpoint = 5, limits = c(0,10)) +
      geom_label(aes(x = method, y = score, label = score_text), size = 4)
  })
  
})
