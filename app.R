# inspired by this blog post https://codingclubuc3m.rbind.io/post/2019-10-15/
library(shiny)
library(Rcpp)
library(tidyverse)

ui <- fluidPage(

    titlePanel("Clifford attractors"),
    
        sidebarLayout(
            sidebarPanel(
                sliderInput("a",
                            "a",
                            min = -5,
                            max = 5,
                            value = 3.14,
                            step = 0.01),
            sliderInput("b",
                        "b",
                        min = -5,
                        max = 5,
                        value = 2.71,
                        step = 0.01),
            sliderInput("c",
                        "c",
                        min = -5,
                        max = 5,
                        value = 1.61,
                        step = 0.01),
            sliderInput("d",
                        "d",
                        min = -5,
                        max = 5,
                        value = 1.2,
                        step = 0.01),
            checkboxInput("cut", "cut?", value = FALSE),
            actionButton("refresh", "Refresh")),
        mainPanel(
           plotOutput("attractor")
        )
    )
)
server <- function(input, output) {
    cppFunction('DataFrame createTrajectory(int n, double x0, double y0,
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')
    
    a <- eventReactive(input$refresh, {
        input$a
    })
    
    b <- eventReactive(input$refresh, {
        input$b
    })
    
    c <- eventReactive(input$refresh, {
        input$c
    })
    
    d <- eventReactive(input$refresh, {
        input$d
    })
    cut <- eventReactive(input$refresh, {
        input$cut
    })
    
    my_df <- eventReactive(input$refresh, {
        createTrajectory(1000000, 0, 0, input$a, input$b, input$c, input$d)
    })
    
    output$attractor <- renderPlot({
        my_df() %>%
#            filter(if (as.logical(input$cut)) x > 5) %>% 
#            filter(if (input$cut) x < quantile(x, probs = 0.95)) %>% 
#            filter(if (input$cut) y > quantile(y, probs = 0.05)) %>% 
#            filter(if (input$cut) y < quantile(y, probs = 0.95)) %>% 
            ggplot(aes(x = x, y = y)) +
            geom_point(alpha = 0.1, shape = 46)+
            coord_equal() +
            theme_void()+
            labs(title = paste("a = ", a(),
                               ", b = ", b(),
                               ", c = ", c(),
                               ", d = ", d())) -> 
            p
        if(cut()) {
            p+
                xlim(quantile(my_df()$x, 0.05), quantile(my_df()$x, 0.95))+
                ylim(quantile(my_df()$y, 0.05), quantile(my_df()$y, 0.95)) ->
                p
        }
        p
    })
    
}

shinyApp(ui = ui, server = server)
