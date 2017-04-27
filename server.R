
library(shiny)
library(ggplot2)
library(grid)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ###
  ### Below Code is for Normal Prob Page
  ###
  
  norm_plot_input <- reactive({
    
    validate(
      need(is.numeric(input$norm_prob_mean), "Mean Must be Numeric"),
      need(is.numeric(input$norm_prob_sd), "Standard Deviation Must be Numeric"),
      need(is.numeric(input$left_upper_bound), "Upper Bound Must be Numeric"),
      need(is.numeric(input$right_lower_bound), "Lower Bound Must be Numeric"),
      need(is.numeric(input$mid_lower_bound), "Lower Bound Must be Numeric"),
      need(is.numeric(input$mid_upper_bound), "Upper Bound Must be Numeric"),
      need(is.numeric(input$tails_upper_bound), "Upper Bound Must be Numeric"),
      need(is.numeric(input$tails_lower_bound), "Lower Bound Must be Numeric"),
      need(input$norm_prob_sd > 0, "Standard Deviation Must be Positive")
    )
    
    # create distribution
    dist_func <- function(x) dnorm(x, mean = input$norm_prob_mean, sd = input$norm_prob_sd)
    
    # get distance from mean for reference
    selection <- input$norm_prob_type
    max_mean_dist <- switch(selection,
                         norm_prob_type_left = abs(input$left_upper_bound - input$norm_prob_mean),
                         norm_prob_type_right = abs(input$right_lower_bound - input$norm_prob_mean),
                         norm_prob_type_mid = max(abs(input$mid_lower_bound - input$norm_prob_mean),
                                                  abs(input$mid_upper_bound - input$norm_prob_mean)),
                         norm_prob_type_tails = max(abs(input$tails_lower_bound - input$norm_prob_mean),
                                                    abs(input$tails_upper_bound - input$norm_prob_mean)))
    
    plot_lim <- max(max_mean_dist +  input$norm_prob_sd, 4 * input$norm_prob_sd)
    
    #create base plot
    p <- ggplot(data.frame(x = (input$norm_prob_mean + c(-1,1) * plot_lim)), aes(x)) +
      stat_function(fun = dist_func) + xlim(input$norm_prob_mean - plot_lim,
                                            input$norm_prob_mean + plot_lim)
    
    # create shading by making polygons
    if(selection == "norm_prob_type_left"){
      x <- seq(input$norm_prob_mean - plot_lim, input$left_upper_bound, by = input$norm_prob_sd / 100)
      y <- dnorm(x, mean = input$norm_prob_mean, sd = input$norm_prob_sd)
      dat <- data.frame(X = c(input$norm_prob_mean - plot_lim,
                              x,
                              input$left_upper_bound),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
      
      prob <- round(pnorm(input$left_upper_bound,
                          mean = input$norm_prob_mean,
                          sd = input$norm_prob_sd,
                          lower.tail = T), 4)
      leg_text <- paste("P(X < ", input$left_upper_bound, ") = ", sep = "")
      
      # Text to be sued to label bounds input by user
      gtext <- textGrob(as.character(input$left_upper_bound), y = -0.075, gp = gpar(col = "red",
                                                                      fontsize = 16,
                                                                      fontface = "bold"))
      gline <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      # add text to ggplot object
      p <- p + 
        annotation_custom(gtext,
                          xmin = input$left_upper_bound, xmax = input$left_upper_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline,
                          xmin = input$left_upper_bound, xmax = input$left_upper_bound,
                          ymin = -Inf, ymax = Inf)
      
    } else if (selection == "norm_prob_type_right"){
      x <- seq(input$right_lower_bound, input$norm_prob_mean + plot_lim, by = input$norm_prob_sd / 100)
      y <- dnorm(x, mean = input
                 $norm_prob_mean, sd = input$norm_prob_sd)
      dat <- data.frame(X = c(input$right_lower_bound,
                              x,
                              input$norm_prob_mean + plot_lim),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
      
      prob <- round(pnorm(input$right_lower_bound,
                          mean = input$norm_prob_mean,
                          sd = input$norm_prob_sd,
                          lower.tail = F), 4)
      leg_text <- paste("P(X > ", input$right_lower_bound, ") = ", sep = "")
                        
      # Text to be sued to label bounds input by user
      gtext <- textGrob(as.character(input$right_lower_bound), y = -0.075, gp = gpar(col = "red",
                                                                                    fontsize = 16,
                                                                                    fontface = "bold"))
      gline <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      # add text to ggplot object
      p <- p + 
        annotation_custom(gtext,
                          xmin = input$right_lower_bound, xmax = input$right_lower_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline,
                          xmin = input$right_lower_bound, xmax = input$right_lower_bound,
                          ymin = -Inf, ymax = Inf)
      
    } else if (selection == "norm_prob_type_mid"){
      x <- seq(input$mid_lower_bound, input$mid_upper_bound, by = input$norm_prob_sd / 100)
      y <- dnorm(x, mean = input
                 $norm_prob_mean, sd = input$norm_prob_sd)
      dat <- data.frame(X = c(input$mid_lower_bound,
                              x,
                              input$mid_upper_bound),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
      
      prob <- round(pnorm(input$mid_upper_bound,
                          mean = input$norm_prob_mean,
                          sd = input$norm_prob_sd,
                          lower.tail = T) - 
                      pnorm(input$mid_lower_bound,
                            mean = input$norm_prob_mean,
                            sd = input$norm_prob_sd,
                            lower.tail = T), 4)
      leg_text <- paste("P(", input$mid_lower_bound, " < X < ", input$mid_upper_bound, ") = ", sep = "")
      
      # Text to be sued to label bounds input by user
      gtext_lower <- textGrob(as.character(input$mid_lower_bound), y = -0.075, gp = gpar(col = "red",
                                                                                    fontsize = 16,
                                                                                    fontface = "bold"))
      gline_lower <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      gtext_upper <- textGrob(as.character(input$mid_upper_bound), y = -0.075, gp = gpar(col = "red",
                                                                                         fontsize = 16,
                                                                                         fontface = "bold"))
      gline_upper <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      # add text to ggplot object
      p <- p + 
        annotation_custom(gtext_lower,
                          xmin = input$mid_lower_bound, xmax = input$mid_lower_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline_lower,
                          xmin = input$mid_lower_bound, xmax = input$mid_lower_bound,
                          ymin = -Inf, ymax = Inf) + 
        annotation_custom(gtext_upper,
                          xmin = input$mid_upper_bound, xmax = input$mid_upper_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline_upper,
                          xmin = input$mid_upper_bound, xmax = input$mid_upper_bound,
                          ymin = -Inf, ymax = Inf)       
    } else if (selection == "norm_prob_type_tails"){
      
      # lower tail geom
      x <- seq(input$norm_prob_mean - plot_lim, input$tails_lower_bound, by = input$norm_prob_sd / 100)
      y <- dnorm(x, mean = input
                 $norm_prob_mean, sd = input$norm_prob_sd)
      dat1 <- data.frame(X = c(input$norm_prob_mean - plot_lim,
                              x,
                              input$tails_lower_bound),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      
      # upper tail geom
      x <- seq(input$tails_upper_bound, input$norm_prob_mean + plot_lim, by = input$norm_prob_sd / 100)
      y <- dnorm(x, mean = input
                 $norm_prob_mean, sd = input$norm_prob_sd)
      dat2 <- data.frame(X = c(input$tails_upper_bound,
                               x,
                               input$norm_prob_mean + plot_lim),
                         Y = c(0, y, 0),
                         Z = rep(1, length(x) + 2))
      
      p <- p + geom_polygon(data = dat1, aes(X, Y, fill = as.factor(Z))) + geom_polygon(data = dat2, aes(X, Y, fill = as.factor(Z)))
      
      prob <- round(pnorm(input$tails_lower_bound,
                          mean = input$norm_prob_mean,
                          sd = input$norm_prob_sd,
                          lower.tail = T) + 
                      pnorm(input$tails_upper_bound,
                            mean = input$norm_prob_mean,
                            sd = input$norm_prob_sd,
                            lower.tail = F), 4)
      leg_text <- paste("P(X < ", input$tails_lower_bound, " or X > ", input$tails_upper_bound," ) = ", sep = "")
      
      # Text to be sued to label bounds input by user
      gtext_lower <- textGrob(as.character(input$tails_lower_bound), y = -0.075, gp = gpar(col = "red",
                                                                                         fontsize = 16,
                                                                                         fontface = "bold"))
      gline_lower <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      gtext_upper <- textGrob(as.character(input$tails_upper_bound), y = -0.075, gp = gpar(col = "red",
                                                                                         fontsize = 16,
                                                                                         fontface = "bold"))
      gline_upper <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
      
      # add text to ggplot object
      p <- p + 
        annotation_custom(gtext_lower,
                          xmin = input$tails_lower_bound, xmax = input$tails_lower_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline_lower,
                          xmin = input$tails_lower_bound, xmax = input$tails_lower_bound,
                          ymin = -Inf, ymax = Inf) + 
        annotation_custom(gtext_upper,
                          xmin = input$tails_upper_bound, xmax = input$tails_upper_bound,
                          ymin = -Inf, ymax = Inf) +
        annotation_custom(gline_upper,
                          xmin = input$tails_upper_bound, xmax = input$tails_upper_bound,
                          ymin = -Inf, ymax = Inf)
    }
    
    # Get title
    title <- paste("N(", input$norm_prob_mean,", ",input$norm_prob_sd,")", sep = "")
    
    if (input$prob_attribution != ""){
      title <- trimws(paste(title ,"\n", "by ", input$prob_attribution, sep = ""))
    }
    
    p <- p +
      ggtitle(title) + 
      ylab("Density") +
      scale_fill_manual(values = c("#56B4E9", "#000000", "#E69F00"),
                        name = NULL,
                        labels = paste(leg_text, prob, sep = "")) +
      theme(legend.justification=c(1,1),
            legend.position=c(.95,.95),
            legend.text = element_text(size = 12),
            title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
            plot.title  = element_text(hjust = 0.5)) +
      ggtitle(title) +
      scale_x_continuous(breaks = seq(input$norm_prob_mean - 4 * input$norm_prob_sd,
                                    input$norm_prob_mean + 4 * input$norm_prob_sd,
                                    by = input$norm_prob_sd)) + 
      xlab(ifelse(input$norm_x_axis_label != "", input$norm_x_axis_label, "ENTER X-AXIS LABEL"))
      
    
    g <- ggplotGrob(p)
    g$layout$clip[g$layout$name=="panel"] <- "off"
    g
    
    })
  
  output$normPlot <- renderPlot({
    grid.draw(norm_plot_input())
  })
  
  output$download_prob_plot <- downloadHandler(
    filename = "prob_plot.png",
    content = function(file){
      ggsave(file, plot = grid.draw(norm_plot_input()), device = "png")
    }
  )

  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  ###
  ### Below Code is for P-val Page
  ###
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  ## Create Plot for their p-values based on distribution of test statistic  
  pval_plot_input <- reactive({
    
    validate(
      need(is.numeric(input$normal_statval), "Normal Test Statistic Must be Numeric"),
      need(is.numeric(input$f_statval), "F Test Statistic Must be Numeric"),
      need(is.numeric(input$t_statval), "T Test Statistic Must be Numeric"),
      need(is.numeric(input$chisq_statval), "Chi Squared Test Statistic Must be Numeric"),
      need(input$chisq_statval >= 0, "Chi Squared Test Statistic Must be Non Negative"),
      need(input$f_statval >= 0, "F Test Statistic Must be Non Negative")
    )
    
    # create distribution function based on user choice
    ts_dist <- input$distribution
    
    if (ts_dist == "normal"){
      dist_func <- function(x) dnorm(x, mean = 0, sd = 1)
    } else if (ts_dist == "tdist"){
      dist_func <- function(x) dt(x, df = input$t_df)
    } else if (ts_dist == "fdist"){
      dist_func <- function(x) df(x, df1 = input$f_df1, df2 = input$f_df2)
    } else if (ts_dist == "chisqdist"){
      dist_func <- function(x) dchisq(x, df = input$chisq_df)
    }
    
    # create distribution
    
    # normal distribution
    if (ts_dist == "normal"){
      
      # create plot of distribution
      plot_lim <- max(4 * 1, abs(input$normal_statval - 0) + 2 * 1)
      p <-  ggplot(data.frame(x = (0 + c(-1,1) * 3.5 * 1)), aes(x)) +
        stat_function(fun = dist_func) + xlim(0 - plot_lim,
                                              0 + plot_lim)
      # check for alternate hypothesis
      if (input$normal_Ha == "two_sided"){
        abs_stat <- abs(input$normal_statval)
        x <- seq(abs_stat, abs_stat + 4*1, by = 1 / 100)
        y <- dnorm(x, mean = 0, sd = 1)
        dat1 <- data.frame(X = c(abs_stat, x, abs_stat + 5 * 1),
                           Y = c(0, y, 0), 
                           Z = rep(1, length(x) + 2))
        dat2 <- data.frame(X = -1 * c(abs_stat, x, abs_stat + 5 * 1),
                           Y = c(0, y, 0),
                           Z = rep(1, length(x) + 2))
        p <- p + 
          geom_polygon(data = dat1, aes(X, Y, fill = as.factor(Z)), show.legend = T, alpha = 0.7) + 
          geom_polygon(data = dat2, aes(X, Y, fill = as.factor(Z)), alpha = 0.7)
        
        pval <- round(2*pnorm(-abs_stat, mean = 0, sd = 1), 4)
                                
      } else if (input$normal_Ha == "greater") {
        x <- seq(input$normal_statval, max(0, input$normal_statval) + 5 * 1, by = 1 / 100)
        y <- dnorm(x, mean = 0, sd = 1)
        dat <- data.frame(X = c(input$normal_statval,
                                x,
                                0 + max(0, input$normal_statval) + 5 * 1),
                          Y = c(0, y, 0),
                          Z = rep(1, length(x) + 2))
        p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
        
        pval <- round(pnorm(input$normal_statval, 0, 1, lower.tail = F), 4)
        
      } else if(input$normal_Ha == "less"){
        x <- seq(min(0, input$normal_statval) + -5 * 1, input$normal_statval, by = 1 / 100)
        y <- dnorm(x, mean = 0, sd = 1)
        dat <- data.frame(X = c(0 + min(0, input$normal_statval) + -5 * 1,
                                x,
                                input$normal_statval),
                          Y = c(0, y, 0),
                          Z = rep(1, length(x) + 2))
        p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
        
        pval <- round(pnorm(input$normal_statval, 0, 1, lower.tail = T), 4)
        
      }
      
      plot_title <- "Standard Normal Distribution: N(0,1)"
      x_axis_text <- "Z-Statistic Values"
      stat_val <- input$normal_statval
      
    } else if (ts_dist == "tdist"){ #T distribution
      
      # create base distribution
      plot_lim <- max(4, abs(input$t_statval + 2))
      p <- ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
        stat_function(fun = dist_func) + 
        xlim(-plot_lim, plot_lim)
      
      # check hypothesis
      if (input$t_Ha == "two_sided"){
        abs_stat <- abs(input$t_statval)
        x <- seq(abs_stat, plot_lim, by = .01)
        y <- dt(x, df = input$t_df)
        dat1 <- data.frame(X = c(abs_stat, x, plot_lim),
                           Y = c(0, y, 0),
                           Z = rep(1, length(x) + 2))
        dat2 <- data.frame(X = -1 * c(abs_stat, x, plot_lim),
                           Y = c(0, y, 0),
                           Z = rep(1, length(x) + 2))
        p <- p + 
          geom_polygon(data = dat1, aes(X, Y, fill = as.factor(Z))) + 
          geom_polygon(data = dat2, aes(X, Y, fill = as.factor(Z)))
        
        pval <- round(2 * pt(-abs(input$t_statval), df = input$t_df), 4)
        
      } else if (input$t_Ha == "greater") {
        x <- seq(input$t_statval, plot_lim, by = .01)
        y <- dt(x, df = input$t_df)
        dat <- data.frame(X = c(input$t_statval,
                                x,
                                plot_lim),
                          Y = c(0, y, 0),
                          Z = rep(1, length(x) + 2))
        p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
        
        pval <- round(pt(input$t_statval, df = input$t_df, lower.tail = F), 4)
        
      } else if(input$t_Ha == "less"){
        x <- seq(-plot_lim, input$t_statval, by = 0.01)
        y <- dt(x, df = input$t_df)
        dat <- data.frame(X = c(-plot_lim,
                                x,
                                input$t_statval),
                          Y = c(0, y, 0),
                          Z = rep(1, length(x) + 2))
        p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
        
        pval <- round(pt(input$t_statval, df = input$t_df, lower.tail = T), 4)
        
        stat_val <- input$t_statval
      }
      
      plot_title <- paste("t(", input$t_df, ")", sep = "")
      x_axis_text <- "T-Statistic Values"
      stat_val <- input$t_statval
      
    } else if (ts_dist == "fdist"){
      plot_lim <- max(7, input$f_statval + 2)
      p <- ggplot(data.frame(x = c(0, 10)), aes(x)) + 
        stat_function(fun = dist_func) + 
        xlim(0, plot_lim)
      x <- seq(input$f_statval, plot_lim, by = 0.01)
      y <- df(x, df1 = input$f_df1, df2 = input$f_df2)
      dat <- data.frame(X = c(input$f_statval, x, plot_lim),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      
      pval <- round(pf(input$f_statval, df1 = input$f_df1, df2 = input$f_df2, lower.tail = F), 4)
      
      p <- p + geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
      
      plot_title <- paste("f(", input$f_df1,", ", input$f_df2, ")", sep = "")
      x_axis_text <- "F-Statistic Values"
      stat_val <- input$f_statval
      
    } else if (ts_dist == "chisqdist"){
      plot_lim <- max(7, input$chisq_statval + 2, ((input$chisq_df < 25) + 2)*input$chisq_df)
      p <- ggplot(data.frame(x = c(0, 10)), aes(x)) + 
        stat_function(fun = dist_func) + 
        xlim(0, plot_lim)
      x <- seq(input$chisq_statval, plot_lim, by = 0.01)
      y <- dchisq(x, df = input$chisq_df)
      dat <- data.frame(X = c(input$chisq_statval, x, plot_lim),
                        Y = c(0, y, 0),
                        Z = rep(1, length(x) + 2))
      
      pval <- round(pchisq(input$chisq_statval, df = input$chisq_df, lower.tail = F), 4)
      
      p <- ggplot(data.frame(x = c(0, 10)), aes(x)) + 
        stat_function(fun = dist_func) + 
        xlim(0, plot_lim) + 
        geom_polygon(data = dat, aes(X, Y, fill = as.factor(Z)))
      
      plot_title <- paste("Chi-Squared(", input$chisq_df, ")", sep = "")
      stat_val <- input$chisq_statval
      x_axis_text <- "Chi-Squared-Statistic Values"
    }
    
    if (input$plot_attr != ""){
      final_title <- trimws(paste(plot_title ,"\n", "by ", input$plot_attr, sep = ""))
    } else {
      final_title <- plot_title
    }
    
    gtext <- textGrob(as.character(stat_val), y = -0.075, gp = gpar(col = "red",
                                                                   fontsize = 16,
                                                                   fontface = "bold"))
    gline <- linesGrob(y = c(-0.02, 0.02), gp = gpar(col = "red", lwd = 2))
                      
    
    p <- p + scale_fill_manual(values = c("#56B4E9", "#000000", "#E69F00"),
                               name = NULL,
                               labels = paste("P-Value: \n ", pval)) +
      xlab(x_axis_text) +
      ylab("Density") +
      theme(legend.justification=c(1,1),
            legend.position=c(.95,.95),
            legend.text = element_text(size = 12),
            title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
            plot.title  = element_text(hjust = 0.5)) +
      ggtitle(final_title) +
      annotation_custom(gtext,
                        xmin = stat_val, xmax = stat_val,
                        ymin = -Inf, ymax = Inf) +
      annotation_custom(gline,
                        xmin = stat_val, xmax = stat_val,
                        ymin = -Inf, ymax = Inf)

    g <- ggplotGrob(p)
    g$layout$clip[g$layout$name=="panel"] <- "off"
    g
  })
  
  output$pvalPlot <- renderPlot({
    grid.draw(pval_plot_input()) 
  })
  
  output$download_pval_plot <- downloadHandler(
    filename = "pval_plot.png",
    content = function(file){
      ggsave(file, plot = grid.draw(pval_plot_input()), device = "png")
    }
  )
  
}
)
