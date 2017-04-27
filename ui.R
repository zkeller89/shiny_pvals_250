#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Visualizing Normal Probabilities",
             titlePanel("Normal Probabilities"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("norm_prob_mean",
                              "Please Enter the Mean",
                              value = 0),
                 numericInput("norm_prob_sd",
                              "Please Enter the Standard Deviation",
                              min = 0, 
                              value = 1),
                 selectInput("norm_prob_type",
                             "What is the form of your probability?",
                             c("P(X < c)" = "norm_prob_type_left",
                               "P(X > c)" = "norm_prob_type_right",
                               "P(a < X < b)" = "norm_prob_type_mid",
                               "P(X < a or X > b)" = "norm_prob_type_tails")),
                 helpText("Note: the probabilities as written do not include the bound, but P(X <= C) will be the same as P(X < C)."),
                 conditionalPanel(
                   condition = "input.norm_prob_type == 'norm_prob_type_left'",
                   numericInput("left_upper_bound",
                                "Please Enter the Upper Bound",
                                value = -1)
                 ),
                 conditionalPanel(
                   condition = "input.norm_prob_type == 'norm_prob_type_right'",
                   numericInput("right_lower_bound",
                                "Please Enter the Lower Bound",
                                value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.norm_prob_type == 'norm_prob_type_mid'",
                   numericInput("mid_lower_bound",
                                "Please Enter the Lower Bound",
                                value = -1),
                   numericInput("mid_upper_bound",
                                "Please Enter the Upper Bound",
                                value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.norm_prob_type == 'norm_prob_type_tails'",
                   numericInput("tails_lower_bound",
                                "Please Enter the Lower Bound",
                                value = -1),
                   numericInput("tails_upper_bound",
                                "Please Enter the Upper Bound",
                                value = 1)
                 ),
                 textInput("norm_x_axis_label",
                           "Enter X Axis Label",
                           value = "",
                           placeholder = "Enter X-Axis Label Here"),
                 textInput("prob_attribution",
                           "Enter Name for Attribution:",
                           value = "",
                           placeholder = "Enter Attribution Here")
                ),
               
               mainPanel(
                 plotOutput("normPlot"),
                 downloadButton("download_prob_plot", "Download Plot")
               )
             )
    ),
    tabPanel("Visualizing P-Values",
             # Application title
            titlePanel("P-value Pictures"),
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
              sidebarPanel(
                 selectInput("distribution",
                             "Select Distribution:",
                             c("Standard Normal" = "normal",
                               "T-Distribution" = "tdist",
                               "F-Distribution" = "fdist",
                               "Chi-Squared Distributtion" = "chisqdist")),
                 conditionalPanel(
                   condition = "input.distribution == 'normal'",
                   radioButtons("normal_Ha",
                                "Alternative Hypothesis: ",
                                c("Not Equal" = "two_sided",
                                  "Greater Than" = "greater",
                                  "Less Than" = "less")),
                   numericInput("normal_statval",
                                "Test Statistic Value: ",
                                min = -Inf, 
                                max = Inf,
                                value = 2)
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'tdist'",
                   numericInput("t_df",
                                "Degrees of Freedom",
                                min = 1,
                                max = 99999,
                                value = 1,
                                step = 1),
                   radioButtons("t_Ha",
                                "Alternative Hypothesis: ",
                                c("Not Equal" = "two_sided",
                                  "Greater Than" = "greater",
                                  "Less Than" = "less")),
                   numericInput("t_statval",
                                "Test Statistic Value: ",
                                min = -Inf, 
                                max = Inf,
                                value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'fdist'",
                   numericInput("f_df1",
                                "Degrees of Freedom (k - 1)",
                                value = 1,
                                min = 1,
                                max = 99999,
                                step = 1),
                   numericInput("f_df2",
                                "Degrees of Freedom (N - k)",
                                value = 1,
                                min = 1,
                                max = 99999,
                                step = 1),
                   numericInput("f_statval",
                                "Test Statistic Value: ",
                                min = 0, 
                                max = Inf,
                                value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'chisqdist'",
                   numericInput("chisq_df",
                                "Degrees of Freedom",
                                min = 1,
                                max = 99999,
                                value = 1,
                                step = 1),
                   numericInput("chisq_statval",
                                "Test Statistic Value: ",
                                min = 0, 
                                max = Inf,
                                value = 1)
                 ),
                 textInput("plot_attr",
                           "Enter Name for Attribution: ",
                           value = "",
                           placeholder = "Enter Attribution Here")
               ),
          
              # Show a plot of the generated distribution
              mainPanel(
                 plotOutput("pvalPlot"),
                 downloadButton("download_pval_plot", "Download Plot")
              )
            )
  
  )
  )
  
))
