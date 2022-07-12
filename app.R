
rm(list = ls())
library(shiny)
library(here)
library(tidyverse)
library(snakecase)


ui <- fixedPage(
    sidebarLayout(
        fluid = FALSE,
        sidebarPanel(
            width = 4,
            textInput("name", "Name", placeholder = "Lastname, Firstname"),
            radioButtons(
                "plusDx",
                "Plus Disease Diagnosis",
                choices = c("Normal", "Pre-Plus", "Plus")
            ),
            sliderInput(
                "plusProb",
                "Probability of Plus Disease",
                min = 0,
                max = 1,
                value = 0.0,
                step = 0.01
            ),
            # sliderInput(
            #     "vss",
            #     "Vascular Severity Score",
            #     min = 1,
            #     max = 9,
            #     value = 1,
            #     step = 1
            # ),
            actionButton("prevImage", "Previous Image"),
            actionButton("nextImage", "Next Image")
        ),
        mainPanel(
            width = 8,
            fixedRow(
                column(
                    12,
                    align="center",
                    plotOutput("image", width = "640px", height = "480px")
                )
            ),
            fixedRow(
                column(
                    12,
                    align="center",
                    textOutput("vss"),
                    tags$head(tags$style("#vss{font-size: 32px; font-weight: bold;}"))
                )
            )
        )
    )
)

server <- function(input, output, session) {
    data <- here("data", "data.csv") %>%
        read_csv(col_names = FALSE) %>%
        mutate(show_vss = TRUE,
               user_plus_dx = NA,
               user_plus_prob = NA_real_)
    
    data <- data %>%
        mutate(show_vss = FALSE) %>%
        rbind(data)
    
    counter <- reactiveValues(value = 1)
    
    observeEvent(input$prevImage, {
        if (counter$value > 1) {
            counter$value <- counter$value - 1
        } else {
            counter$value <- 1
        }
    })
    
    observeEvent(input$nextImage, {
        if (counter$value <= nrow(data)) {
            counter$value <- counter$value + 1
        } else
            counter$value <- nrow(data) + 1
    })
    
    show_image <- eventReactive(counter$value, {
        # print(counter$value)
        if (nchar(input$name) < 5) {
            counter$value <- 0
            out_list <- list("src" = "error.jpg",
                             "vss" = "",
                             "show_vss" = FALSE)
        } else {
            if (counter$value > 1 & counter$value <= nrow(data) + 1) {
                data[counter$value - 1, 4] <<- input$plusDx
                data[counter$value - 1, 5] <<- input$plusProb
            }
            if (counter$value != nrow(data) + 1) {
                filename <- here("data", "images", data[counter$value, 1])
                vss <- data[counter$value, 2]
                show_vss <- data[counter$value, 3]
                out_list <- list("src" = filename, "vss" = vss, "show_vss" = show_vss)
            } else {
                out_list <- list("src" = "recorded.jpg",
                                 "vss" = "",
                                 "show_vss" = FALSE)
                name <- to_snake_case(input$name)
                write_csv(data, paste("results-", name, ".csv", sep = ""))
            }
        }
        return(list(out_list, data))
    })
    
    output$image <- renderImage({
        call <- show_image()
        out_list <- call[[1]]
        data <- call[[2]]
        if (out_list$show_vss == TRUE) {
            output$vss <- renderText({ as.character(paste("VSS:", out_list$vss)) })
        } else {
            output$vss <- renderText({ })
        }
        return(out_list)
    }, deleteFile = FALSE)
}

shinyApp(ui, server)
