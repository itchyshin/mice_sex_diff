

library(shiny)
library(shinythemes)

meta_results <- read.csv("./trait_results.csv")
procedure_results <- read.csv("./procedure_results.csv")
GroupingTerm_results <- read.csv("./GroupingTerm_results.csv")


ui <- fluidPage(mainPanel(theme = shinythemes::themeSelector()),
    # Application title
    titlePanel("Percentage Differences in Mean and Variances between the Sexes"),
    
    sidebarPanel(
      strong("Introduction"),
        p(" This Shiny app allows users to interface with the traits, procedures and major trait types present in the mouse database, and using the meta-analytic results from analyses across strains and centers, calculate the percentage difference between males and females for the given cateogory. This can be quite useful in understanding how different traits cary across the sexes and in conducting power analyses when designing experiments with a given trait to ensure that differences between sexes can be detected with a given experimental design"), 
        strong("How to use:"), 
      p("Using the app is simple. Just find your trait, procedure or major trait group of interest in the dropdown menu and the table will be updated with meta-analytic results. You can read the -  Interpretation of Output - for an example trait below the table."), 
        strong("Interpretation of output:"), 
        p(" To interpret the above table output users need to look at the percentage increase and the direction. For example, for the trait - pre-pulse inhibition -  the mean percentage tells us that males are 0.52% larger than females as the direction tells us that males are greater than females (Males >). In contrast, when we look at the variance in this trait, we see that females have 0.91% larger variance than males (Females >)")
    ),
    
    # Trait inputs
   mainPanel(
        selectInput("trait", "Trait of Interest", unique(meta_results$parameter_group), multiple = FALSE),
    
      # Output Table
      tableOutput("table1")
    ), 
   
   mainPanel(
     selectInput("Procedure", "Procedure of Interest", unique(procedure_results$X), multiple = FALSE),
     
     # Output Table
     tableOutput("table2")
   ),
   
   mainPanel(
     selectInput("TraitGroup", "Trait Group of Interest", unique(GroupingTerm_results$X), multiple = FALSE),
     
     # Output Table
     tableOutput("table3")
   ),
  
    mainPanel(
        tags$img(src = "PCWmice1.jpg", width = 220, height =120 )
       ) 
    )    

server <- function(input, output){
    
    output$table1 <- renderTable({	
            
        # remember log(males/females)
            sub_trait_data <- meta_results[meta_results$parameter_group %in% as.character(input$trait),]
            sub_trait_data_effects <- sub_trait_data[, c("lnRR", "lnVR", "lnCVR")]
        
        # odds ratio: 1 = males = females; <1 = males < females; >1 = males > females
            odds_ratio <- as.numeric(exp(sub_trait_data_effects))
        
        # Calculate percent diff
            perc_difference <- ifelse(odds_ratio < 1, (1-odds_ratio)*100, (odds_ratio-1)*100)
            direct <- ifelse(odds_ratio < 1, "Females >", "Males >") 
            
        # Table output
            table <- rbind(paste0(round(perc_difference, digits = 2), " %"), direct)
            table <- cbind(c("Percentage Increase", "Direction"), table)
            colnames(table) <- c("Name", "Mean", "Variance", "Coefficient of Variance")
            table
    })
    
    output$table2 <- renderTable({	
      
      # remember log(males/females)
      sub_trait_data <- procedure_results[procedure_results$X %in% as.character(input$Procedure),]
      sub_trait_data_effects <- sub_trait_data[, c("lnRR", "lnVR", "lnCVR")]
      
      # odds ratio: 1 = males = females; <1 = males < females; >1 = males > females
      odds_ratio <- as.numeric(exp(sub_trait_data_effects))
      
      # Calculate percent diff
      perc_difference <- ifelse(odds_ratio < 1, (1-odds_ratio)*100, (odds_ratio-1)*100)
      direct <- ifelse(odds_ratio < 1, "Females >", "Males >") 
      
      # Table output
      table <- rbind(paste0(round(perc_difference, digits = 2), " %"), direct)
      table <- cbind(c("Percentage Increase", "Direction"), table)
      colnames(table) <- c("Name", "Mean", "Variance", "Coefficient of Variance")
      table
    })
    
    output$table3 <- renderTable({	
      
      # remember log(males/females)
      sub_trait_data <- GroupingTerm_results[GroupingTerm_results$X %in% as.character(input$TraitGroup),]
      sub_trait_data_effects <- sub_trait_data[, c("lnRR", "lnVR", "lnCVR")]
      
      # odds ratio: 1 = males = females; <1 = males < females; >1 = males > females
      odds_ratio <- as.numeric(exp(sub_trait_data_effects))
      
      # Calculate percent diff
      perc_difference <- ifelse(odds_ratio < 1, (1-odds_ratio)*100, (odds_ratio-1)*100)
      direct <- ifelse(odds_ratio < 1, "Females >", "Males >") 
      
      # Table output
      table <- rbind(paste0(round(perc_difference, digits = 2), " %"), direct)
      table <- cbind(c("Percentage Increase", "Direction"), table)
      colnames(table) <- c("Name", "Mean", "Variance", "Coefficient of Variance")
      table
    })
    
}

shinyApp(ui, server)

