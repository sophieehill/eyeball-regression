library(shiny)
library(tidyverse)

theme_set(theme_classic())

## Set our sample size
n <- 20

ui <- fluidPage(

    # Application title
    titlePanel("Eyeball regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            em("Can you guess the intercept and slope of the line of best fit?"),
            br(), br(),
            numericInput("intercept",
                        "Intercept:",
                        min = -Inf,
                        max = Inf,
                        value = 30),
            numericInput("slope",
                         "Slope:",
                         min = -Inf,
                         max = Inf,
                         value = 1.5),
            br(),
            actionButton("button_go", "Go!"),
            br(), 
            strong(textOutput("guess")),
            em(textOutput("intercept_guess")),
            em(textOutput("slope_guess")),
            em(textOutput("MSE_guess")),
            br(),
            actionButton("button_show", "Show me the answer"),
            br(), 
            strong(textOutput("ols")),
            em(textOutput("intercept_true")),
            em(textOutput("slope_true")),
            em(textOutput("MSE_true")),
            br(),
            actionButton("button_clear", "Start again"),
            br(), br(),
            actionButton("button_refresh", "Fresh data please!")
        ),

        mainPanel(
           plotOutput("fitPlot")
        )
    )
)

server <- function(input, output, session) {
    
    v <- reactiveValues(p1 = NULL, 
                        p2 = NULL,
                        p3 = NULL,
                        p4 = NULL,
                        dat = NULL,
                        MSE_guess = NULL,
                        MSE_true = NULL,
                        guess = NULL,
                        intercept_guess = NULL,
                        slope_guess = NULL,
                        ols = NULL,
                        intercept_true = NULL,
                        slope_true = NULL)
    
    ## Create some x values
    x <- runif(n = n, min = 5, max = 95)
    
    ## Create a y variable 
    # y = beta_0 + beta_1*x + noise
    
    # pick a random number for the intercept
    beta_0 <- rnorm(n = 1, mean = 0, sd = 100)

    # pick a random number for the slope
    beta_1 <- rnorm(n = 1, mean = 0, sd = 2)
    
    # Create some individual-level "noise"
    noise <- rnorm(n = n, mean = 0, sd = 25*abs(beta_1))
    
    # Create the y values
    y <- beta_0 + (beta_1 * x) + noise
    
    dat <- bind_cols(x = x, y = y) 
    
    # Run a linear regression of y on x using lm()
    fit <- lm(y ~ x, data = dat)
    
    intercept_true <- coef(fit)[1]
    slope_true <- coef(fit)[2]
    
    # Plot our data and save as an object called "p"
    p <- dat %>%
        ggplot(aes(x = x,
                   y = y)) +
        geom_point(size = 4) +
        labs(x = "",
             y = "") +
        xlim(0, 100) +
        ylim(min(y) - sd(y), 
             max(y) + sd(y)) +
        theme(text = element_text(size = 20)) +
        coord_cartesian(expand  = FALSE)
    
    datx <- dat %>% 
        mutate(
            y_hat = intercept_true + (slope_true * x),
            pred_error = y - y_hat
        )
    
    MSE_true <- mean(datx$pred_error^2)
    
    v$p1 <- p

    observeEvent(input$button_go, {
        
        dat <- dat %>% 
            mutate(
                y_hat = input$intercept + (input$slope * x),
                pred_error = y - y_hat
            )
        
        v$MSE_guess <- mean(dat$pred_error^2)
        
        
        v$p2 <- geom_abline(
                intercept = input$intercept,
                slope = input$slope,
                color = "blue",
                lwd = 3
            )
        
        v$p3 <- geom_segment(data = dat,
            aes(x = x,
                             xend = x,
                             y = y,
                             yend = y_hat),
                         color = "red",
                         lty = "dotted",
            lwd = 1)
    })
    
    observeEvent(input$button_clear, {
        v$p2 <- NULL
        v$p3 <- NULL
        v$p4 <- NULL
        v$MSE_guess <- NULL
        v$MSE_true <- NULL
        v$intercept_true <- NULL
        v$slope_true <- NULL
        v$intercept_guess <- NULL
        v$slope_guess <- NULL
        v$guess <- NULL
        v$ols <- NULL
    }
    )
    
    observeEvent(input$button_show, {
        
        v$MSE_true <- MSE_true
        
        v$p4 <- geom_abline(
            intercept = intercept_true,
            slope = slope_true,
            color = "darkgrey",
            lwd = 3
        )
        
        v$ols <- "OLS coefficients"
        v$intercept_true <- intercept_true
        v$slope_true <- slope_true
    }
    )
    
    observeEvent(input$button_refresh, {
        session$reload()
    })


    output$fitPlot <- renderPlot({
        v$p1 + v$p2 + v$p3 + v$p4
    })
    
    output$guess <- renderText(
        if(is.null(v$MSE_guess)){}
        else{
            "Your guess"
        }
    )
    
    output$intercept_guess <- renderText(
        if(is.null(v$MSE_guess)){}
        else{
            paste0("Intercept: ", 
                   prettyNum(input$intercept,
                             big.mark= ",",
                             digits = 3,
                             scientific = FALSE
                   ))
        }
    )
    
    output$slope_guess <- renderText(
        if(is.null(v$MSE_guess)){}
        else{
            paste0("Slope: ", 
                   prettyNum(input$slope,
                             big.mark= ",",
                             digits = 3,
                             scientific = FALSE
                   ))
        }
    )
    
    output$MSE_guess  <- renderText(
        if(is.null(v$MSE_guess)){}
        else{
            paste0("Mean squared error: ",
                   prettyNum(v$MSE_guess,
                             big.mark= ",",
                             digits = 0,
                             scientific = FALSE
                   ))
        }
    )
    
    output$MSE_true  <- renderText(
        if(is.null(v$MSE_true)){}
        else{
            paste0("Mean squared error: ",
                   prettyNum(v$MSE_true,
                             big.mark= ",",
                             digits = 0,
                             scientific = FALSE
                   ))
        }
    )
    
    output$ols <- renderText(
        v$ols
    )
    
    output$intercept_true <- renderText(
        if(is.null(v$intercept_true)){}
        else{
            paste0("Intercept: ",
                   prettyNum(v$intercept_true,
                             big.mark= ",",
                             digits = 3,
                             scientific = FALSE
                   ))
        }
    )
    
    output$slope_true <- renderText(
        if(is.null(v$slope_true)){}
        else{
            paste0("Slope: ",
                   prettyNum(v$slope_true,
                             big.mark= ",",
                             digits = 3,
                             scientific = FALSE
                   ))
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)