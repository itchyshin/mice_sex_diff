
calculate_population_stats <- function(mydata, min_individuals = 5) {
  mydata %>% 
    group_by(population, strain_name, production_center, sex) %>% 
    summarise(
      trait = parameter_name[1],
      x_bar = mean(data_point),
      x_sd = sd(data_point),
      n_ind = n()
    ) %>% 
    ungroup() %>%
    filter(n_ind > min_individuals) %>% 
    # Check both sexes present & filter those missing
    group_by(population) %>% 
    mutate(
      n_sex = n_distinct(sex)
    ) %>% 
    ungroup() %>%
    filter(n_sex ==2) %>% 
    select(-n_sex) %>%
    arrange(production_center, strain_name, population, sex)
}


create_meta_analysis_effect_sizes <- function(mydata) {
  i <- seq(1, nrow(mydata), by = 2)
  input <- data.frame(
    n1i = mydata$n_ind[i],
    n2i = mydata$n_ind[i + 1],
    x1i = mydata$x_bar[i],
    x2i = mydata$x_bar[i + 1],
    sd1i = mydata$x_sd[i],
    sd2i = mydata$x_sd[i + 1]
  )
  
  mydata[i,] %>% 
  select(strain_name, production_center) %>%
    mutate(
      effect_size_CVR = Calc.lnCVR(CMean = input$x1i, CSD = input$sd1i, CN = input$n1i, EMean = input$x2i, ESD = input$sd2i, EN = input$n2i),
      sample_variance_CVR = Calc.var.lnCVR(CMean = input$x1i, CSD = input$sd1i, CN = input$n1i, EMean = input$x2i, ESD = input$sd2i, EN = input$n2i),
      effect_size_VR = Calc.lnVR(CSD = input$sd1i, CN = input$n1i, ESD = input$sd2i, EN = input$n2i),
      sample_variance_VR = Calc.var.lnVR(CN = input$n1i, EN = input$n2i),
      effect_size_RR = Calc.lnRR(CMean = input$x1i, CSD = input$sd1i, CN = input$n1i, EMean = input$x2i, ESD = input$sd2i, EN = input$n2i),
      sample_variance_RR = Calc.var.lnRR(CMean = input$x1i, CSD = input$sd1i, CN = input$n1i, EMean = input$x2i, ESD = input$sd2i, EN = input$n2i),
      err = as.factor(seq_len(n()))
    )

}