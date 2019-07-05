
library(shiny)

meta_results <- read.csv("trait_meta_results.csv")


ui <- fluidPage(mainPanel(shinythemes::themeSelector()),
    # Application title
    titlePanel("Trait Specific Percentage Differences in Mean and Variances between the Sexes"),
    
    sidebarPanel(
      strong("Introduction"),
        p(" This Shiny app allows users to interface with the traits present in the mouse database, and using the meta-analytic results from analyses across strains and centers, calculate the percentage difference between males and females for the given trait. This can be quite useful in understanding how different traits cary across the sexes and in conducting power analyses when designing experiments with a given trait to ensure that differences between sexes can be detected with a given experimental design"), 
        strong("How to use:"), 
      p("Using the app is simple. Just find your trait of interest in the dropdown menu and the table will be updated with meta-analytic results. You can read the -  Interpretation of Output - for an example trait below the table."), 
        strong("Interpretation of output:"), 
        p(" To interpret the above table output users need to look at the percentage increase and the direction. For example, for the trait -  % pre-pulse inhibition - global - the mean percentage tells us that males are 0.73% larger than females as the direction tells us that males are greater than females (Males >). In contrast, when we look at the variance in this trait, we see that females have 1.11% larger variance than males (Females >)")
    ),
    
    # Trait inputs
   mainPanel(
        selectInput("trait", "Trait of Interest", unique(meta_results$trait), multiple = FALSE),
    
      # Output Table
      tableOutput("table")
    ), 
  
    mainPanel(
        tags$img(src = "008169.png", width = 580, height =340)
       ) 
    )    

server <- function(input, output){
    
    output$table <- renderTable({	
            
        # remember log(males/females)
            sub_trait_data <- meta_results[meta_results$trait %in% as.character(input$trait),]
            sub_trait_data_effects <- sub_trait_data[, c("lnRR", "lnVR", "lnCVR")]
        
        # odds ratio: 1 = males = females; <1 = males < females; >1 = males > females
            odds_ratio <- as.numeric(exp(sub_trait_data_effects))
        
        # Calculate percent diff
            perc_difference <- ifelse(odds_ratio < 1, (1-odds_ratio)*100, (odds_ratio-1)*100)
            direct <- ifelse(odds_ratio < 1, "Males >", "Females >") 
            
        # Table output
            table <- rbind(paste0(round(perc_difference, digits = 2), " %"), direct)
            table <- cbind(c("Percentage Increase", "Direction"), table)
            colnames(table) <- c("Name", "Mean", "Variance", "Coefficient of Variance")
            table
    })
    
}

shinyApp(ui, server)

