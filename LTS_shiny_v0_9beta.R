library(shiny)
library(rhandsontable)
library(tidyr)
library(lme4)
library(car)
library(ggplot2)
library(dplyr)

ui <- shinyUI(fluidPage(
  titlePanel("CRM stability assessment"),
  h5("Estimation of uncertainty associated with stability based on the maximum likelihood method using linear mixed effect models"),
  
  tabsetPanel(
    tabPanel("Data input", 
             sidebarLayout(
               sidebarPanel(
                 helpText(h3("Define your study below and fill in the tables with your data.")),
                 br(),
                 
                 #numericInput(inputId = "time_points", label = "Number of Time Points:", value = 4, min = 1, width = '50%'),
                 
                 #uiOutput("num_run_input"),
                 
                 div(style = "white-space: nowrap;", 
                     numericInput(inputId = "time_points", label = "Number of Time Points:", value = 4, min = 1, width = '30%')),
                 
                 div(style = "white-space: nowrap;", 
                     numericInput(inputId = "no_units", label = "Number of units per time point:", value = 2, min = 1, width = '30%')),
                 
                 div(style = "white-space: nowrap;", 
                     numericInput(inputId = "no_repl", label = "(Maximum) number of replicates:", value = 5, min = 1, width = '30%')),
                 br(),
                 actionButton("Example", "Example dataset", class = "btn-block"),
               width = 2),
               mainPanel(
                 helpText(h3("Table of measured values")),
                 helpText(h5("Fill in the table with time point, unit number, and measurement results data!")),
                 rHandsontableOutput("hot"),
                 
                 br(),
                 
                 helpText(h3("Table of analytical sequence")),
                 helpText(h5("(Optional) Fill in the table with the corresponding analytical sequence values!")),
                 span(textOutput("data_warn1"), style="color:red"),
                 rHandsontableOutput("hot2"),
                 
                 br(),
                 
                 helpText(h3("Table of measurement runs")),
                 helpText(h5("(Optional) Fill in the table with the correspondin measurement run numbers!")),
                 span(textOutput("data_warn2"), style="color:red"),
                 rHandsontableOutput("hot3"),
                 br()
               )
             )
    ),
    tabPanel("Descriptive statistics",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("checkboxGroup"),
                 width = 2),
               mainPanel(
                 fluidRow(
                   column(6, plotOutput("data_graph", width = "auto", height = 480)),
                   column(6, plotOutput("data_boxplot", width = "auto", height = 480))),
                 br(),
                 fluidRow(
                   column(6, h4("Summary table"), tableOutput("summary_table"), align="center"),
                   column(6, plotOutput("hist", width = "auto", height = 480))),
               )
             )
    ),
    tabPanel("Assumption checks",
             helpText(h3("Homoscedasticity check")), 
             verbatimTextOutput("lev_test"),
             br(),
             fluidRow(
               column(6, helpText("Residuals vs fitted values graph"),
               plotOutput("assOutput", width = "auto", height = 480),
               uiOutput("res_vs_fitplot")),  
             ),
             br(),
             helpText(h3("Normality check")),
             verbatimTextOutput("sap_test"),
             br(),
             fluidRow(
               column(6, plotOutput("ass2Output", width = "auto", height = 480),
               uiOutput("QQplots")),
             ),
             br()
             
    ),
    tabPanel("Uncertainty estimation", 
             sidebarLayout(
               sidebarPanel(
                 helpText("Define your study below and fill in the table with your data."),
                 wellPanel(
                   div(style = "white-space: nowrap;", 
                       numericInput(inputId = "target_time_int", label = "Target time interval:", value = 12, min = 0, width = '30%')),
                   br(),
                   radioButtons("Unit", h3("Unit"), c("day(s)", "week(s)", "month(s)", "year(s)"), selected = "year(s)")
                 ),
                 width = 2),
               mainPanel(
                 verbatimTextOutput("ustab"),
                 verbatimTextOutput("ucOutput"),
                 verbatimTextOutput("bias")
               )
             )
    ), 
    tabPanel("Uncertainty associated with heterogeneity", 
             sidebarLayout(
               sidebarPanel(
                 helpText("If replicate measurment of multiple units were used for the stability study then the uncertainty associated with heterogeneity can be also estimated."),
                 helpText(h5("Please, keep in mind: if the number of units is small (5-6 or less) then the estimate has a large uncertainty.")),
                 width = 2),
               mainPanel(
                 verbatimTextOutput("uhom"),
                 verbatimTextOutput("uc_homOutput")
               )
             )
    )
  )
))

server <- function(input, output, session) {
  
  values <- reactiveValues(df = NULL, df2 = NULL)
  
  observe({
    n <- as.integer(input$no_units)
    n2 <- as.integer(input$time_points)
    num_rows <- n * n2
    num_cols <- 2L + as.integer(input$no_repl)
    
    df <- data.frame(matrix(nrow = num_rows, ncol = num_cols))
    colnames(df) <- c("time", "unit_no", paste0("repl", 1L:as.integer(input$no_repl)))
    
    # Convert columns to character to allow text input
    # df[] <- lapply(df, function(x) format(as.numeric(x), scientific = TRUE, digits = 3))
    # df[] <- lapply(df, function (x) {as.numeric(sprintf("%.4g", x))} )
    df[] <- lapply(df, as.numeric)
    print("df first")
    print(str(df))
    
    values$df <- df
    
    # creating data frame for analytical sequence
    df2 <- data.frame(matrix(nrow = num_rows, ncol = num_cols))
    colnames(df2) <- colnames(df)
    
    # Convert columns to character to allow text input
    df2[] <- lapply(df2, as.numeric)
    
    values$df2 <- df2
    
    # creating data frame for measurment run
    df3 <- data.frame(matrix(nrow = num_rows, ncol = num_cols))
    colnames(df3) <- colnames(df)
    
    # Convert columns to character to allow text input
    df3[] <- lapply(df3, as.numeric)
    
    values$df3 <- df3
  })
  
  output$hot <- renderRHandsontable({
    req(input$no_units)
    df <- values$df
    df2 <- values$df2
    df3 <- values$df3
    values$df2$unit_no <- df$unit_no  # Update 'unit_no' in df2 with the values from df
    values$df2$time <- df$time  # Update 'time' in df2 with the values from df
    values$df3$time <- df$time  # Update 'time' in df3 with the values from df
    values$df3$unit_no <- df$unit_no  # Update 'unit_no' in df3 with the values from df
    #df_display <- lapply(df, function(x) format(as.numeric(x), scientific = TRUE, digits = 3))
    #df_display <- as.data.frame(df_display)
    #print(df_display)
    if (!is.null(df)) {
      rhandsontable(df, stretchH = "all", rowHeaders = NULL) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = TRUE,
          readOnly = FALSE  # Set readOnly to TRUE for "unit_no" column
        ) %>%
        hot_col(c("time", "unit_no"), format = "0") #%>%
      #hot_col(c(paste0("repl", 1:as.integer(input$no_repl))), format = "0.00e+0") 
    }
  })
  
  output$hot2 <- renderRHandsontable({
    req(input$no_units)
    df2 <- values$df2
    if (!is.null(df2)) {
      rhandsontable(df2, stretchH = "all", rowHeaders = NULL) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = TRUE
        ) %>%
        hot_col(c("time", "unit_no"), readOnly = TRUE, format = "0") %>%
        hot_col(c("time", "unit_no", paste0("repl", 1L:as.integer(input$no_repl))), format = "0") 
    }
  })
  
  output$hot3 <- renderRHandsontable({
    req(input$no_units)
    df3 <- values$df3
    if (!is.null(df3)) {
      rhandsontable(df3, stretchH = "all", rowHeaders = NULL) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = TRUE
        ) %>%
        hot_col(c("time", "unit_no"), readOnly = TRUE, format = "0") %>%
        hot_col(c("time", "unit_no", paste0("repl", 1:as.integer(input$no_repl))), format = "0") 
    }
  })
  
  observeEvent(input$hot, {
    #req(input$no_units)
    df <- hot_to_r(input$hot)
    values$df <- df
    print("df updated")
    print(str(df))
  })
  
  observeEvent(input$hot2, {
    #req(input$no_units)
    df2 <- hot_to_r(input$hot2)
    values$df2 <- df2
    print("df2 updated")
    print(str(df2))
  })
  
  observeEvent(input$hot3, {
    #req(input$no_units)
    df3 <- hot_to_r(input$hot3)
    values$df3 <- df3
    print("df3 updated")
    print(str(df3))
  })
  
  observeEvent(input$Example, {
    df <- read.csv("https://raw.githubusercontent.com/kamitoth/LTS/ca711a99857744ba86d1be176519020d989ed81c/df_example1.csv")
    df2 <- read.csv("https://raw.githubusercontent.com/kamitoth/LTS/ca711a99857744ba86d1be176519020d989ed81c/df_example2.csv")
    df3 <- read.csv("https://raw.githubusercontent.com/kamitoth/LTS/ca711a99857744ba86d1be176519020d989ed81c/df_example3.csv")
    values$df <- df
    values$df2 <- df2
    values$df3 <- df3
  })
  
  observe({
    df <- values$df
    df2 <- values$df2
    df3 <- values$df3
    if (!is.null(df)) {
      df_long <- gather(df, rep, res, paste0("repl", 1L:as.integer(input$no_repl)))
      if (!all(is.na(df2[,-(1L:2L)])) & identical(colSums(!is.na(df)), colSums(!is.na(df2)))) {
        df2_long <- gather(df2, rep, anal, paste0("repl", 1L:as.integer(input$no_repl)))
        values$df2_long <- df2_long
        if (identical(which(is.na(df_long$res)), which(is.na(df2_long$anal)))) {
          df_long <- cbind(df_long, anal=df2_long$anal)
        }
      }
      if (!all(is.na(df3[,-(1L:2L)])) & identical(colSums(!is.na(df)), colSums(!is.na(df3)))) {
        df3_long <- gather(df3, rep, run, paste0("repl", 1L:as.integer(input$no_repl)))
        values$df3_long <- df3_long
        if (identical(which(is.na(df_long$res)), which(is.na(df3_long$run)))) {
          df_long <- cbind(df_long, run=df3_long$run)
        } 
      }
      
      values$df_long <- df_long
      #values$df_long$res <- as.numeric(values$df_long$res)
      
      # Check unique levels of time
      unique_levels <- unique(df_long$time)
      print("df_long created")
      print(str(df_long))
      
      # Check unique levels of unit_no
      unique_unit_no <- unique(df_long$unit_no)
      print("unique_unit_no")
      print(length(unique_unit_no))
      
      # Check unique levels of run
      if (!is.null(df_long$run)) {
        unique_run <- unique(df_long$run)
      }
      
      if (!any(is.na(df_long$unit_no)) & !any(is.na(df_long$time))
          # To avoid freezing if minimum number of data for LME not provided
          & sum(!is.na(df_long$res)) > length(unique_levels)*as.integer(input$no_units)) {
        if (length(unique_levels) < 2L) {
          values$uc <- "Error: Not enough unique levels in 'time' for regression analysis."
          # same unit tested under repeatability conditions without analytical trend check
        }
        # If only a single unit was tested and only the measurement data is provided
        else if (length(unique_unit_no) < 2L & !("anal" %in% colnames(df_long)) & !("run" %in% colnames(df_long))) {
          uc <- lm(res ~ time, df_long)
          # same unit tested under repeatability conditions with analytical trend check
        } 
        # If only a single unit was tested and measurement data and analytical sequence are provided
        else if (length(unique_unit_no) < 2L & "anal" %in% colnames(df_long) & !("run" %in% colnames(df_long))) {
          #check linear regression slope for significance
          anal_reg <- lm(res ~ anal, df_long)
          sig_slope <- round(coef(summary(anal_reg))[ 'anal', "Pr(>|t|)"], digits=2)
          #if significant: correct dataset?
          if (sig_slope < 0.05) {
            # correct data for analytical trend
            df_long$res2 <- df_long$res - df_long$anal * summary(lm(res ~ anal, df_long))$coefficients['anal', 'Estimate']
            uc <- lm(res2 ~ time, df_long) # change df_long!!!! + unrealistic scenario
          } else {
            uc <- lm(res ~ time, df_long)
          }
        } 
        # If only a single unit was tested and measurement and run data are provided
        else if (length(unique_unit_no) < 2L & !("anal" %in% colnames(df_long)) & "run" %in% colnames(df_long)) {
          # LME with run as random effect even though it might have few levels
          # Should it be lm vs LME based on the number of run levels???
          uc <- lmer(res ~ time + (1|run), df_long)
        } 
        # If only a single unit was tested and measurement and run data and analytical sequence are all provided
        else if (length(unique_unit_no) < 2 & "anal" %in% colnames(df_long) & "run" %in% colnames(df_long)) {
            # Estimate significance level of the LME slope
            sig_slope <- anova(lmerTest::lmer(res ~ time + anal + (1|run), df_long, REML=F))['anal','Pr(>F)'] # to obtain approximate p-values
            if (sig_slope < 0.05){
              uc <- lmer(res ~ time + anal + (1|run), df_long, REML=F)
              # Otherwise use simpler model
            } else {
              uc <- lmer(res ~ time + (1|run), df_long, REML=F)
            }
        } 
        else if (length(unique_unit_no) > 1L & !("anal" %in% colnames(df_long)) & !("run" %in% colnames(df_long))) {
          # Perform ML fit
          uc <- lmer(res ~ time + (1|unit_no), df_long, REML=F)
        } 
        else if (length(unique_unit_no) > 1L & !("anal" %in% colnames(df_long)) & ("run" %in% colnames(df_long))) {
          # Use nested model if design is fully nested
          M <- table(df_long$run, df_long$unit_no)
          if (all(colSums(M > 0L) == 1L)) {
            uc <- lmer(res ~ time + (1|run/unit_no), df_long, REML=F)
          } else {
            # Use crossed model otherwise
            uc <- lmer(res ~ time + (1|run) + (1|unit_no), df_long, REML=F)
          }
          # 'Run' is used as random effect regardless of the number of levels
          # Otherwise it could mess up the fixed effect and SE estimations
        }
        else if (length(unique_unit_no) > 1L & ("anal" %in% colnames(df_long)) & !("run" %in% colnames(df_long))) {
          # Estimate significance level of the LME slope
          sig_slope <- anova(lmerTest::lmer(res ~ time + anal + (1|unit_no), df_long, REML=F))['anal','Pr(>F)'] # to obtain approximate p-values
          # If significant, add analytical sequence to the model
          if (sig_slope < 0.05){
            uc <- lmer(res ~ time + anal + (1|unit_no), df_long, REML=F)
          }
          # Otherwise use the simpler model
          else {
            uc <- lmer(res ~ time + (1|unit_no), df_long, REML=F)
          }
          # For both 'anal' and 'run' present...
        }
        else if (length(unique_unit_no) > 1L & ("anal" %in% colnames(df_long)) 
                 & ("run" %in% colnames(df_long))) {
          # Use nested model if design is fully nested
          M <- table(df_long$run, df_long$unit_no)
          if (all(colSums(M > 0L) == 1L)) {
            # Estimate significance level of the LME slope
            sig_slope <- anova(lmerTest::lmer(res ~ time + anal + (1|run/unit_no), df_long, REML=F))['anal','Pr(>F)'] # to obtain approximate p-values
            
            # If significant, add analytical sequence to the model
            if (sig_slope < 0.05){
              uc <- lmer(res ~ time + anal + (1|run/unit_no), df_long, REML=F)
              # Otherwise use simpler model
            } else {
              uc <- lmer(res ~ time + (1|run/unit_no), df_long, REML=F)
            }
          } else {
            # Use crossed model if design is not fully nested...
            sig_slope2 <- anova(lmerTest::lmer(res ~ time + anal + (1|run) + (1|unit_no), df_long, REML=F))['anal','Pr(>F)'] # to obtain approximate p-values
            
            # With 'anal' in the model if analytical trend is significant...
            if (sig_slope2 < 0.05) {
              uc <- lmer(res ~ time + anal + (1|run) + (1|unit_no), df_long, REML=F)
              # or without it if it is not significant
            } else{
              uc <- lmer(res ~ time + (1|run) + (1|unit_no), df_long, REML=F)
            }
          }
        }
        
        # Saphiro-Wilk test for checking normality
        # BUG is still there!
        #tryCatch( { sap <- shapiro.test(residuals(uc))$p.value }
        #          , warning = function(w) { sap <- "Hey, a warning" })
        
        # BUG is still there!
        if (!is.null(uc)){
          sap <- try(shapiro.test(residuals(uc))$p.value)
          if (inherits(sap, "try-error")) sap <- "error"
        }

        # Extract standard error of the LME slope and calculate (approximate) p-values
        if (class(uc) == "lmerMod") {
          se_lme <- round(coef(summary(uc))[ , "Std. Error"][[2L]], digits=2)
          sig_lme <- anova(lmerTest::lmer(formula(uc), df_long, REML=F))['time','Pr(>F)'] # to obtain approximate p-values
        } else {
          se_lme <- summary(uc)$coefficients['time', 'Std. Error']
          sig_lme <- coef(summary(lm(formula(uc), data=df_long)))[ 'time', "Pr(>|t|)"]
        }
        # Perform the 'standard' linear regression (OLS) (just for comparison)
        lm_mod <- lm(res ~ time, df_long)
        # Extract standard error of the 'standard' linear fit slope (just for comparison)
        se_lm <- summary(lm_mod)$coefficients['time', 'Std. Error']
        # 'standard' linear regression (OLS) on the unit means (again, just for comparison)
        df_long_mod <- df_long %>%
          group_by(unit_no) %>%
          summarise(res=mean(res), time=mean(time))
        lm_mod2 <- lm(res ~ time, df_long_mod)
        # Extract standard error (again, just for comparison)
        se_lm2 <- summary(lm_mod2)$coefficients['time', 'Std. Error']
        
        #Estimation of uncertainty associated with heterogeneity based on the stability data
        if (!is.null(uc) && inherits(uc, "lmerMod") && length(unique_unit_no) > 1L){
          uc_hom <- update(uc, REML=T)
          uc_hom_VC <- as.data.frame(VarCorr(uc_hom))
          between_unit_sd <- uc_hom_VC[grepl("unit_no", uc_hom_VC$grp),'sdcor']
          busd_rel <- between_unit_sd/mean(df_long$res)
          within_unit_sd <- uc_hom_VC[which(uc_hom_VC$grp == 'Residual'),'sdcor']
          wsd_rel <- within_unit_sd/mean(df_long$res)
        } 
        
        values$uc <- uc
        values$sap <- sap
        values$se_lme <- se_lme
        values$se_lm <- se_lm
        values$se_lm2 <- se_lm2
        values$sig_lme <- sig_lme
        values$uc_hom <- uc_hom
        values$between_unit_sd <- between_unit_sd
        values$within_unit_sd <- within_unit_sd
        values$busd_rel <- busd_rel
        values$wsd_rel <- wsd_rel
      } else {
        values$uc <- "Insufficient data in measurement table."
      }
    }
  })
 
  output$data_graph <- renderPlot({
    df_long <- values$df_long
    df_long <- na.omit(df_long)
    if (!is.null(df_long) && length(unique(df_long$unit_no)) < 2L) {
      # If a 'single object' was tested 
      fit <- ggplot(!is.na(df_long), aes(x=time, y=res))+
        geom_point(position=position_dodge2(0.5), size=3)+
        geom_smooth(method='lm', se = FALSE, color="black")
    } else if (!is.null(df_long) && length(unique(df_long$unit_no)) > 1L){
      # Show unit numbers with different colors if multiple units were tested
      fit <- ggplot(df_long, aes(x=time, y=res, color=as.factor(unit_no)))+
        geom_point(position=position_dodge2(0.5), size=3)+
        geom_smooth(method='lm', se = FALSE, color="black")+
        labs(colour = "unit number")
    }
    fit <- fit + labs(title = "Graph of the dataset", y="result")
    fit <- fit + theme(panel.border = element_rect(linetype = "solid", fill = NA),
                       plot.title = element_text(size = 22),
                       axis.title = element_text(size = 18),
                       axis.text = element_text(size = 15),
                       legend.title = element_text(size = 18),
                       legend.text = element_text(size = 15))
    if (!is.null(input$sel) && any(input$sel == "run")){
      fit <- fit + facet_wrap(~run)
    }
    print(fit)
  })
  
  output$data_boxplot <- renderPlot({
    df_long <- values$df_long
    df_long <- na.omit(df_long)
    if (!is.null(df_long) && length(unique(df_long$unit_no)) < 2L) {
      # If a 'single object' was tested 
      fit2 <- ggplot(df_long, aes(x=as.factor(time), y=res))+
        geom_boxplot(position=position_dodge2(0.5), outlier.size = 3)
    } else if (!is.null(df_long) && length(unique(df_long$unit_no)) > 1L){
      # Show unit numbers with different colors if multiple units were tested
      fit2 <- ggplot(df_long, aes(x=as.factor(time), y=res, color=as.factor(unit_no)))+
        geom_boxplot(position=position_dodge2(0.5), outlier.size = 3)+
        labs(colour = "unit number", x = "time")
    } else {
      print("Error")
    }
    fit2 <- fit2 + labs(title = "Boxplot representation of the dataset", y="result")
    fit2 <- fit2 + theme(panel.border = element_rect(linetype = "solid", fill = NA),
                       plot.title = element_text(size = 22),
                       axis.title = element_text(size = 18),
                       axis.text = element_text(size = 15),
                       legend.title = element_text(size = 18),
                       legend.text = element_text(size = 15))
    if (!is.null(input$sel) && any(input$sel == "run")){
      fit2 <- fit2 + facet_wrap(~run)
    }
    print(fit2)
  })
  
  # Update the checkboxGroupInput choices based on the data frame
 # output$checkboxGroup <- renderUI({
 #   df_long <- values$df_long
 #   if (!is.null(df_long)) {
 #     selected_columns <- colnames(df_long[,!colnames(df_long) %in% c("res", "rep", "anal", "time")])
 #     }
 #   print("selected columns")
 #   print(selected_columns)
 #   print("colnames")
 #   print(colnames(df_long))
 #   checkboxGroupInput("sel", "Group by", choices = selected_columns)
 # })
  
  output$checkboxGroup <- renderUI({
    df_long <- values$df_long
    selected_columns <- NULL  # Initialize selected_columns outside the if block
    
    if (!is.null(df_long)) {
      selected_columns <- setdiff(colnames(df_long),c("res", "time", "rep", "anal"))
    }

    checkboxGroupInput("sel", "Group by", choices = selected_columns)
  })
  
  # Could be added: number of replicates, runs, units...
  output$summary_table <- renderTable({
    df_long <- values$df_long
    if (!is.null(df_long)) {
      sum <- if (!is.null(input$sel)) {
        na.omit(df_long) %>%
          group_by(across(all_of(input$sel))) %>%
          summarise(
            mean = sprintf("%.2e", mean(res)),
            sd = sprintf("%.2e", sd(res)),
            median = sprintf("%.2e", median(res)),
            n = n()
            ) 
      } else {
        na.omit(df_long) %>%
            summarise(
              mean = sprintf("%.2e", mean(res)),
              sd = sprintf("%.2e", sd(res)),
              median = sprintf("%.2e", median(res)),
              n = n()
            ) 
      } 
    }
    if (!is.null(sum$unit_no)) {sum$unit_no <- as.character(sum$unit_no)}
    if (!is.null(sum$run)) {sum$run <- as.character(sum$run)}
    sum
  })
  
  output$hist <- renderPlot({
    df_long <- values$df_long
    df_long <- na.omit(df_long)
    if (!is.null(df_long)) {
      # If a 'single object' was tested 
      fit3 <- ggplot(df_long, aes(x=res))+
        geom_histogram(aes(y = ..density..), colour = 1, fill = "white")+
        geom_density()
    } 
    fit3 <- fit3 + labs(title = "Histogram and kernel density of the dataset")
    fit3 <- fit3 + theme(panel.border = element_rect(linetype = "solid", fill = NA),
                         plot.title = element_text(size = 22),
                         axis.title = element_text(size = 18),
                         axis.text = element_text(size = 15),
                         legend.title = element_text(size = 18),
                         legend.text = element_text(size = 15))
    if (!is.null(input$sel) & length(input$sel) == 1){
        if (input$sel == "run") {
          fit3 <- fit3 + facet_wrap(~run)
        } else if (input$sel == "unit_no") {
          fit3 <- fit3 + facet_wrap(~unit_no)
        }
    } else if (!is.null(input$sel)){
      fit3 <- fit3 + facet_wrap(~ run + unit_no)
    }
    fit3 <- fit3 + labs(x ="result")
    print(fit3)
  })
   
  output$ucOutput <- renderPrint({
    uc <- values$uc
    #conv_status <- values$conv_status
    #df_long <- values$df_long
    if (!is.null(uc) & class(uc) == "lmerMod") {
      print(summary(uc))
      
      # Check singular fit warnings and print the warning message if present
      if (isSingular(uc)==TRUE){
        # Check convergence status and print the warning message if not converged
        conv_status <- summary(uc)$optinfo$conv$lme4$messages
        if (!is.null(conv_status)) {
          a <- warning("Warning!", conv_status)
        }
        b <- paste("The parameters are on the boundary of the feasible parameter space:", sep ="\n",
              "variances of one or more linear combinations of effects are (close to) zero")
        paste(a,b, sep ="\n")
      }
    } 
    # If only one 'unit' was used for the entire study summarise linear regression
    else if (!is.null(uc) & class(uc) == "lm"){
      print(summary(uc))
    }
    # Print the error message
    else{
      cat(warning(uc))
    }
  })
  
  output$data_warn1 <- renderText({
    df2 <- values$df2
    df <- values$df
    if (!all(is.na(df2[,-(1L:2L)])) & !identical(colSums(!is.na(df)), colSums(!is.na(df2)))) {
      paste("Number of data in analytical sequence table and measurement table do not match.",
            "Analytical sequence will be ignored!", sep="\n")
    } else if (!all(is.na(df2[,-(1:2)])) & identical(colSums(!is.na(df)), colSums(!is.na(df2))) &
               !identical(which(is.na(df)), which(is.na(df2)))) {
      paste("Position of data in analytical sequence table and measurement table do not match.",
            "Analytical sequence will be ignored!", sep="\n")
    }
  })
  
  output$data_warn2 <- renderText({
    df3 <- values$df3
    df <- values$df
    if (!all(is.na(df3[,-(1L:2L)])) & !identical(colSums(!is.na(df)), colSums(!is.na(df3)))) {
      paste("Number of data in run table and measurement table do not match.",
            ("Run data will be ignored!"), sep="\n")
    } else if (!all(is.na(df3[,-(1L:2L)])) & identical(colSums(!is.na(df)), colSums(!is.na(df3))) &
              !identical(which(is.na(df)), which(is.na(df3)))) {
      paste("Position of data in run table and measurement table do not match.",
            ("Run data will be ignored!"), sep="\n")
    }
  })
  
  output$assOutput <- renderPlot({
    uc <- values$uc
    if (!is.null(uc) & class(uc) == "lmerMod") {
      # If uc is lmerMod, show residuals vs fitted values graph
      par(mfrow = c(2, 1)); plot(uc)
    } else{
      # If uc is lm, show both residuals and standardized residuals vs fitted values graph
      par(mfrow = c(2, 1)); plot(uc, 1); plot(uc, 3)
    }
  })
  
  output$ass2Output <- renderPlot({
    uc <- values$uc
    if (!is.null(uc) & class(uc) == "lmerMod") {
      qqnorm(resid(uc)); qqline(resid(uc))
    } else {
      plot(uc, 2)
    }
  })
  
  output$lev_test <- renderText({
    lev <- values$lev
    if (!is.null(lev)) {
      paste(paste("Levene's test p-value is", sep = " ", signif(lev, digits = 2)),
            if (lev > 0.05){
              "H0 (p>0.05): population variances are approximately equal (homoscedasticity) at the 95% CL"
            } else {
              "H0 (p<0.05): population variances are NOT equal (heterocedasticity) at the 95% CL"
            }, sep="\n")
    }
  })
  
  output$sap_test <- renderText({
    sap <- values$sap
    if (!is.null(sap)) {
      paste(paste("Shapiro-Wilk's normality test p-value is", sep = " ", signif(sap, digits = 2)),
            if (sap > 0.05){
              "H0 (p>0.05): residuals are approximately normally distributed at the 95% CL"
            } else {
              "H0 (p<0.05): residuals are NOT normally distributed at the 95% CL"
            }, sep="\n")
    }
  })
  
  url <- a("How to interpret QQ plots?", href="https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot?noredirect=1&lq=1", target = "_blank")
  output$QQplots <- renderUI({
    url
  })
  
  url2 <- a("How to interpret residual vs fitted values plots?", href="https://stats.stackexchange.com/questions/76226/interpreting-the-residuals-vs-fitted-values-plot-for-verifying-the-assumptions", target = "_blank")
  output$res_vs_fitplot <- renderUI({
    url2
  })

  
  output$ustab <- renderText({
    uc <- values$uc
    se_lme <- values$se_lme
    sig_lme <- values$sig_lme
    print(sig_lme)
    if (!is.null(se_lme) && inherits(uc, "lmerMod")) {
      if (sig_lme < 0.05){
        paste(paste("Standard error of the slope based on the ML estimation of LME:", sep = " ", signif(se_lme, digits = 3)),
              paste("Uncertainty associated with stability is not provided because there is a significant trend in the stability study! See ISO Guide 35:2017 for guidance!"), 
              sep="\n")
      } else {
        paste(paste("Standard error of the slope based on the ML estimation of LME:", sep = " ", signif(se_lme, digits = 3)),
              paste("The uncertainty associacetd with stability for", sep = " ", input$target_time_int, 
                    input$Unit, "=",
                    signif(input$target_time_int*se_lme, digits = 3)), sep="\n")
      }
    } else if (!is.null(se_lme) && inherits(uc, "lm")){
      if (sig_lme < 0.05){
        paste(paste("Standard error of the slope of the linear regression:", sep = " ", signif(se_lme, digits = 3)),
              paste("Uncertainty associated with stability is not provided because there is a significant trend in the stability study! See ISO Guide 35:2017 for guidance!"), 
              sep="\n")
      } else {
        paste(paste("Standard error of the slope of the linear regression:", sep = " ", signif(se_lme, digits = 3)),
              paste("The uncertainty associacetd with stability for", sep = " ", input$target_time_int, 
                    input$Unit, "=",
                    signif(input$target_time_int*se_lme, digits = 3)), sep="\n")
      }
    }
  })
  
  output$bias <- renderText({
    se_lme <- values$se_lme
    se_lm <- as.numeric(values$se_lm)
    se_lm2 <- as.numeric(values$se_lm2)
    val <- (se_lm-se_lme)/se_lme*100L
    val2 <- (se_lm2-se_lme)/se_lme*100L
    if (!is.null(se_lm) & !is.null(se_lme)) {
      paste(paste("If between unit variances would be ignored (linear regression model)", sep="\n", "the uncertainty estimate would be biased by"), 
            paste(sep = " ", signif(val, digits = 3), "% (all data) or", signif(val2, digits = 3), "% (unit means)"))
    }
  })
  
  output$uhom <- renderText({
    between_unit_sd <- values$between_unit_sd
    within_unit_sd <- values$within_unit_sd
    busd_rel <- values$busd_rel
    wsd_rel <- values$wsd_rel
    if (!is.null(between_unit_sd) & !is.null(within_unit_sd)) {
      paste(paste(paste("The between unit standard deviation (standard uncertainty associated with heterogeneity) estimate is", 
            signif(between_unit_sd, digits = 3), sep=" "),".", sep=""),
      paste("If the mean accurately represents the central tendency of the dataset, then", signif(busd_rel*100, digits = 3), "% is the relative between unit SD estimate."),
      paste(paste("At the same time, the within unit standard deviation (standard uncertainty associated with method repeatability) estimate is", 
            signif(within_unit_sd, digits = 3), sep=" "),".", sep=""),
      paste("If the mean accurately represents the central tendency of the dataset, then", signif(wsd_rel*100, digits = 3), "% is the relative within unit SD estimate."),
      sep="\n")
    }
  })
  
  output$uc_homOutput <- renderPrint({
    uc_hom <- values$uc_hom
    if (!is.null(uc_hom)) {
      print(summary(uc_hom))
      
      # Check singular fit warnings and print the warning message if present
      if (isSingular(uc_hom)==TRUE){
        # Check convergence status and print the warning message if not converged
        conv_status <- summary(uc_hom)$optinfo$conv$lme4$messages
        if (!is.null(conv_status)) {
          a <- warning("Warning!", conv_status)
        }
        b <- paste("The parameters are on the boundary of the feasible parameter space:", sep ="\n",
                   "variances of one or more linear combinations of effects are (close to) zero")
        paste(a,b, sep ="\n")
      }
    } 
  })
}

shinyApp(ui = ui, server = server)