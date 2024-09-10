
panelInOut  <- function(number_panelsPerPeriod, num_inOut, period){
  
  panels <- matrix(NA, nrow = number_panelsPerPeriod + num_inOut * (period - 1), ncol = period)
  
  panels[,1] <- c(1:number_panelsPerPeriod, rep(NA_integer_, nrow(panels) - number_panelsPerPeriod))
  
  nextvalue <- function(x, sig = 1){
    x[length(x)] + sig
  }
  
  for(j in 2:period){
    panels[(num_inOut*(j-1) + 1):(num_inOut*(j-1) + number_panelsPerPeriod), j] <- 
      c(
                     panels[,j-1][!is.na(panels[,j-1])][-(1:num_inOut)],
                     nextvalue(panels[,j-1][!is.na(panels[,j-1])][-(1:num_inOut)]):
                       nextvalue(panels[,j-1][!is.na(panels[,j-1])][-(1:num_inOut)], 
                                 num_inOut)
                    )
  }
  panels
}



# 
# esquemaAdaptado <- panelInOut(number_panelsPerPeriod = 15, num_inOut = 3, period = 60)
# unique(esquemaAdaptado %>% as.vector())
# unique(esquemaAdaptado %>% as.vector()) %>% length() # 192 panels
# 
# 
# 
prueba2 <- panelInOut(15, 3, 60)
unique(prueba2 %>% as.vector()) %>% length()
# writexl::write_xlsx(prueba2 %>% as.data.frame(), "prueba2.xlsx")


prueba3 <- panelInOut(10,1, 60)
unique(prueba3 %>% as.vector()) %>% length()


prueba3 <- panelInOut(2505,50, 60)
unique(prueba3 %>% as.vector()) %>% length()
