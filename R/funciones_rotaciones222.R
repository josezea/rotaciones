library(dplyr)

######################### Función para generar esquemas rotativos 2-2-2  #####################
# Function to find the number of common elements between two rows
utils_common_elements_between_rows <- function(row1, row2) {
  length(intersect(row1, row2))
}

# Function to create a matrix of common elements counts
utils_create_common_elements_matrix <- function(matrix_data) {
  matrix_data <- as.matrix(matrix_data)
  n <- nrow(matrix_data)
  common_elements_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      common_elements_matrix[i, j] <- utils_common_elements_between_rows(matrix_data[i, ], 
                                                                          matrix_data[j, ])
    }
  }
  return(common_elements_matrix)
}

# Create the common elements matrix



utils_generatePanel_222 <- function(vector_numVisita1raLetraEntrada, 
                         vector_numVisita2daLetraEntrada,
                         letra_inicia_bloque, num_trimestres){
  # Para agregar letras a los valores numéricos
  letra_inicial <- which(LETTERS == toupper(letra_inicia_bloque))
  vctr_letras_iniciales <- LETTERS[letra_inicial:(letra_inicial + 3)]
  
  
  # Inicializar la matriz
  
  matriz_resultado <- matrix(NA, nrow = num_trimestres, 
                             ncol = length(vector_numVisita1raLetraEntrada))
  
  # Inicialización de primera fila
  matriz_resultado[1,] <- c(1, 1, 1, 1)
  
  
  # Inicialización de matriz con respecto a primera fila
  # En esta parte se establecen los valores iniciales al máximo posible
  
  matriz_resultado[2:6, which(vector_numVisita1raLetraEntrada == 1)] <- c(1, NA, NA, 1, 1)
  
  
  matriz_resultado[2:6,which(vector_numVisita1raLetraEntrada == 2)] <- c(NA, NA, 1, 1, NA)
  
  
  
  matriz_resultado[2:6,which(vector_numVisita1raLetraEntrada == 3)] <- c(1, NA, NA, NA, NA)
  
  
  
  matriz_resultado[2:6,which(vector_numVisita1raLetraEntrada == 4)] <- c(NA, NA, NA, NA, NA)
  
  
  
  # Inicialización segunda fila
  for(j in 1:4){
    if(is.na(matriz_resultado[2,j])){
      matriz_resultado[2,j] = 2
    } 
  }
  
  
  # Inicialización tercera fila
  for(j in 1:4){
    if(matriz_resultado[1,j] == matriz_resultado[2,j] ){
      matriz_resultado[3,j] = matriz_resultado[2,j] + 1
    }
    
    if(matriz_resultado[1,j] != matriz_resultado[2,j] ){
      matriz_resultado[3,j] = matriz_resultado[2,j] 
    }
    
  }
  
  
  # Inicialización cuarta fila 
  for(j in 1:4){
    if(!is.na(matriz_resultado[4,j])){
      matriz_resultado[4,j] = matriz_resultado[4,j]
    }
    
    if(is.na(matriz_resultado[4,j]) &
       matriz_resultado[2,j] != matriz_resultado[3,j] ){
      matriz_resultado[4,j] = matriz_resultado[3,j] 
    }
    
    if(is.na(matriz_resultado[4,j]) &
       matriz_resultado[2,j] == matriz_resultado[3,j] &
       vector_numVisita1raLetraEntrada[j] %in% 1:2){
      matriz_resultado[4,j] = matriz_resultado[1,j] 
    }
    
    
    if(is.na(matriz_resultado[4,j]) &
       matriz_resultado[2,j] == matriz_resultado[3,j] &
       vector_numVisita1raLetraEntrada[j] %in% 3:4){
      matriz_resultado[4,j] = matriz_resultado[3,j] + 1 
    }
  }
  
  
  
  
  # Inicialización quinta fila
  for(j in 1:4){
    if(!is.na(matriz_resultado[5,j])){
      matriz_resultado[5,j] = matriz_resultado[5,j]
    }
    
    if(is.na(matriz_resultado[5,j]) &
       matriz_resultado[3,j] != matriz_resultado[4,j] ){
      matriz_resultado[5,j] = matriz_resultado[4,j] 
    }
    
    if(is.na(matriz_resultado[5,j]) &
       matriz_resultado[3,j] == matriz_resultado[4,j] &
       vector_numVisita1raLetraEntrada[j] %in% 1:2){
      matriz_resultado[5,j] = matriz_resultado[2,j] 
    }
    
    
    if(is.na(matriz_resultado[5,j]) &
       matriz_resultado[3,j] == matriz_resultado[4,j] &
       vector_numVisita1raLetraEntrada[j] %in% 3:4){
      matriz_resultado[5,j] = matriz_resultado[4,j] + 1 
    }
  }
  
  
  # La logica depende de donde arranca las segunda letras del 6 al 8
  
  # Inicialización sexta fila
  for(j in 1:4){
    if(!is.na(matriz_resultado[6,j])){
      matriz_resultado[6,j] = matriz_resultado[6,j]
    }
    
    if(is.na(matriz_resultado[6,j]) &
       matriz_resultado[4,j] != matriz_resultado[5,j] ){
      matriz_resultado[6,j] = matriz_resultado[5,j] 
    }
    
    if(is.na(matriz_resultado[6,j]) &
       matriz_resultado[4,j] == matriz_resultado[5,j] &
       vector_numVisita2daLetraEntrada[j] == 1){
      matriz_resultado[6,j] = matriz_resultado[3,j] 
    }
    
    
    if(is.na(matriz_resultado[6,j]) &
       matriz_resultado[4,j] == matriz_resultado[5,j] &
       vector_numVisita2daLetraEntrada[j] == 3){
      matriz_resultado[6,j] = matriz_resultado[5,j] + 1 
    }
  }
  
  
  
  
  # Inicialización septima fila
  for(j in 1:4){
    if(!is.na(matriz_resultado[7,j])){
      matriz_resultado[7,j] = matriz_resultado[7,j]
    }
    
    if(is.na(matriz_resultado[7,j]) &
       matriz_resultado[5,j] != matriz_resultado[6,j] ){
      matriz_resultado[7,j] = matriz_resultado[6,j] 
    }
    
    if(is.na(matriz_resultado[7,j]) &
       matriz_resultado[5,j] == matriz_resultado[6,j] &
       vector_numVisita2daLetraEntrada[j] == 1){
      matriz_resultado[7,j] = matriz_resultado[4,j] 
    }
    
    
    if(is.na(matriz_resultado[7,j]) &
       matriz_resultado[5,j] == matriz_resultado[6,j] &
       vector_numVisita2daLetraEntrada[j] == 3){
      matriz_resultado[7,j] = matriz_resultado[6,j] + 1 
    }
  }
  
  
  
  # De la 8 en adelante
  
  
  for(i in 8:num_trimestres){
    for(j in 1:4){
      if(!is.na(matriz_resultado[i,j])){
        matriz_resultado[i,j] = matriz_resultado[i,j]
      }
      
      if(is.na(matriz_resultado[i,j]) &
         matriz_resultado[i-2,j] != matriz_resultado[i-1,j] ){
        matriz_resultado[i,j] = matriz_resultado[i-1,j] 
      }
      
      if(is.na(matriz_resultado[i,j]) &
         matriz_resultado[i-2,j] == matriz_resultado[i-1,j] &
         matriz_resultado[i-3,j] == matriz_resultado[i-7,j]){
        matriz_resultado[i,j] = matriz_resultado[i-1,j] + 1 
      }
      
      
      if(is.na(matriz_resultado[i,j]) &
         matriz_resultado[i-2,j] == matriz_resultado[i-1,j] &
         matriz_resultado[i-3,j] != matriz_resultado[i-7,j]){
        matriz_resultado[i,j] = matriz_resultado[i-3,j]  
      }
    }
  }
  
  
  
  matriz_resultado2 <- matriz_resultado
  for(i in 1:4) { # Bloque de 4
    matriz_resultado2[,i] <- paste0(vctr_letras_iniciales[i], 
                                    matriz_resultado[,i]) 
    
  }
  
  matriz_resultado2
}

# Función principal

utils_flexiblePanels222 <- function(inicia_primeraVisita = 1:4, 
                      inicia_segundaVisita = c(1, 1, 3, 3), numTrimestres){
  
  # Control
  if(!identical(sort(as.integer(unique(inicia_primeraVisita))), 1:4)) {
    stop("El vector vctr_inicial_numVisita debe ser cualquier permutación del vector 1, 2, 3, 4, 5 en cualquier orden, por ejemplo: 3, 5, 1, 3, 2")
  }
  
  bloque1 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                          vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                          letra_inicia_bloque = "A", num_trimestres = numTrimestres)
  
  
  bloque2 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                          vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                          letra_inicia_bloque = "E", num_trimestres = numTrimestres)
  
  bloque3 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                          vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                          letra_inicia_bloque = "I", num_trimestres = numTrimestres)
  
  Bloque <- cbind(bloque1, bloque2, bloque3) %>% as.data.frame()
  row.names(Bloque) <- paste0(1:numTrimestres)
  colnames(Bloque) <- c(rep("Mes1", 4), rep("Mes2", 4), rep("Mes3", 4))  
  
  num_paneles <- ncol(Bloque)
  
  # Trimestres contiguos
  common_elements_matrix <- utils_create_common_elements_matrix(Bloque)
  
  contiguo <- NA
  for(i in 1:(nrow(common_elements_matrix) - 1)){
    contiguo[i] <- common_elements_matrix[i,i+1] 
    names(contiguo)[i] <- paste0(i, "-", i+1)
  }
  
  prop_contiguo <- contiguo / num_paneles
  indica_contiguo <- 100 * sum(prop_contiguo == 0.5) / length(contiguo)
  
  
  
  contiguoCada2 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 2)){
    contiguoCada2[i] <- common_elements_matrix[i,i+2] # typo cada4
    names(contiguoCada2)[i] <- paste0(i, "-", i + 2)
    
  }
  
  prop_contiguoCada2 <- contiguoCada2 / num_paneles
  indica_contiguoCada2 <- 100 * sum(prop_contiguoCada2 == 0) / length(prop_contiguoCada2)
  
  
  
  contiguoCada3 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 3)){
    contiguoCada3[i] <- common_elements_matrix[i,i+3] # typo cada4
    names(contiguoCada3)[i] <- paste0(i, "-", i + 3)
  }
  
  prop_contiguoCada3 <- contiguoCada3 / num_paneles
  indica_contiguoCada3 <- 100 * sum(prop_contiguoCada3 == 0.25) / length(contiguoCada3)
  
  
  
  
  contiguoCada4 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 4)){
    contiguoCada4[i] <- common_elements_matrix[i,i+4] # typo cada4
    names(contiguoCada4)[i] <- paste0(i, "-", i + 4)
  }
  
  prop_contiguoCada4 <- contiguoCada4 / num_paneles
  indica_contiguoCada4 <- 100 * sum(prop_contiguoCada4 == 0.5) / length(contiguoCada4)
  
  
  Resultado <- list(Bloque, common_elements_matrix, 
                    contiguo, prop_contiguo, indica_contiguo,
                    contiguoCada2, prop_contiguoCada2, indica_contiguoCada2,
                    contiguoCada3, prop_contiguoCada3, indica_contiguoCada3,
                    contiguoCada4, prop_contiguoCada4, indica_contiguoCada4,
                    (indica_contiguo +  indica_contiguoCada2 + 
                       indica_contiguoCada3 + indica_contiguoCada4) / 4)
  
  
   
  names(Resultado) <- c("Panel", "filas_elementosComunes", 
                        "FilascontiguaComunes", "prop_FilascontiguaComunes", "test_FilascontiguaComunes",
                        "FilascontiguaComunesCada2", "prop_FilascontiguaComunesCada2", "test_FilascontiguaComunesCada2",
                        "FilascontiguaComunesCada3", "prop_FilascontiguaComunesCada3", "test_FilascontiguaComunesCada3",
                        "FilascontiguaComunesCada4", "prop_FilascontiguaComunesCada4", "test_FilascontiguaComunesCada4",
                        "testGlobal")
  
  Resultado
}



f_admissibleScenarios222 <- function(inicia_primeraVisita = 1:4, numTrimestres = 40){
  grid_segundavisita <- expand.grid(c(1,3), c(1,3), c(1,3), c(1,3))
  lista_resultados_posibles <- vector(mode = "list", nrow(grid_segundavisita))
  for(i in 1:nrow(grid_segundavisita)){
    lista_resultados_posibles[[i]] <- utils_flexiblePanels222(inicia_primeraVisita = inicia_primeraVisita,
                                                         inicia_segundaVisita =  grid_segundavisita[i,],
                                                         numTrimestres = numTrimestres)
    
  }
  
  vctr_testGlabales <- rep(NA_real_, nrow(grid_segundavisita))
  for(i in 1:nrow(grid_segundavisita)){
    vctr_testGlabales[i] <- lista_resultados_posibles[[i]]$testGlobal
  }
  df_escenariosPlausibles <- grid_segundavisita[which(vctr_testGlabales == 100),]
  df_escenariosPlausibles
}


f_panel <- function(inicia_primeraVisita = 1:4, numTrimestres,
                      inicia_segundaVisita = NULL){
  
  # Control
  df_plausibles <- f_admissibleScenarios222(inicia_primeraVisita, numTrimestres)
  
    if(!is.null(inicia_segundaVisita)){
  df_escenarioFlexible <- utils_flexiblePanels222(inicia_primeraVisita, inicia_segundaVisita, 
                                              numTrimestres)
  }
  
  if(!identical(sort(as.integer(unique(inicia_primeraVisita))), 1:4)) stop("El vector vctr_inicial_numVisita debe ser cualquier permutación del vector 1, 2, 3, 4, 5 en cualquier orden, por ejemplo: 3, 5, 1, 3, 2")
  
  if(is.null(inicia_segundaVisita)) {
    inicia_segundaVisita <- df_plausibles[1,]
    df_escenarioFlexible <- utils_flexiblePanels222(inicia_primeraVisita, inicia_segundaVisita, 
                                                    numTrimestres)
    }
  
  
  bloque1 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                    vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                    letra_inicia_bloque = "A", num_trimestres = numTrimestres)
  
  
  bloque2 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                     vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                     letra_inicia_bloque = "E", num_trimestres = numTrimestres)
  
  bloque3 <- utils_generatePanel_222(vector_numVisita1raLetraEntrada = inicia_primeraVisita,
                     vector_numVisita2daLetraEntrada = inicia_segundaVisita,
                     letra_inicia_bloque = "I", num_trimestres = numTrimestres)
  
  Bloque <- cbind(bloque1, bloque2, bloque3) %>% as.data.frame()
  row.names(Bloque) <- paste0(1:numTrimestres)
  colnames(Bloque) <- c(rep("Mes1", 4), rep("Mes2", 4), rep("Mes3", 4))  
    
  num_paneles <- ncol(Bloque)
  
  # Trimestres contiguos
  common_elements_matrix <- utils_create_common_elements_matrix(Bloque)
  
  contiguo <- NA
  for(i in 1:(nrow(common_elements_matrix) - 1)){
    contiguo[i] <- common_elements_matrix[i,i+1] 
    names(contiguo)[i] <- paste0(i, "-", i+1)
  }
  
  prop_contiguo <- contiguo / num_paneles
  indica_contiguo <- 100 * sum(prop_contiguo == 0.5) / length(contiguo)
  
  
  
  contiguoCada2 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 2)){
    contiguoCada2[i] <- common_elements_matrix[i,i+2] # typo cada4
    names(contiguoCada2)[i] <- paste0(i, "-", i + 2)
    
  }
  
  prop_contiguoCada2 <- contiguoCada2 / num_paneles
  indica_contiguoCada2 <- 100 * sum(prop_contiguoCada2 == 0) / length(prop_contiguoCada2)
  
  
  
  contiguoCada3 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 3)){
    contiguoCada3[i] <- common_elements_matrix[i,i+3] # typo cada4
    names(contiguoCada3)[i] <- paste0(i, "-", i + 3)
  }
  
  prop_contiguoCada3 <- contiguoCada3 / num_paneles
  indica_contiguoCada3 <- 100 * sum(prop_contiguoCada3 == 0.25) / length(contiguoCada3)
  
  
  
  
  contiguoCada4 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 4)){
    contiguoCada4[i] <- common_elements_matrix[i,i+4] # typo cada4
    names(contiguoCada4)[i] <- paste0(i, "-", i + 4)
  }
  
  prop_contiguoCada4 <- contiguoCada4 / num_paneles
  indica_contiguoCada4 <- 100 * sum(prop_contiguoCada4 == 0.5) / length(contiguoCada4)
  
  
  Resultado <- list(Bloque, common_elements_matrix, 
                      contiguo, prop_contiguo, indica_contiguo,
                      contiguoCada2, prop_contiguoCada2, indica_contiguoCada2,
                      contiguoCada3, prop_contiguoCada3, indica_contiguoCada3,
                      contiguoCada4, prop_contiguoCada4, indica_contiguoCada4,
                    (indica_contiguo +  indica_contiguoCada2 + 
                      indica_contiguoCada3 + indica_contiguoCada4) / 4,
                    inicia_segundaVisita)
  
 test_global <- (indica_contiguo +  indica_contiguoCada2 + 
                   indica_contiguoCada3 + indica_contiguoCada4) / 4
if(test_global != 100) {
stop(paste0("En la segunda visita (inicia_segundaVisita) el valor debe ser NULL o alguno de los 
            siguientes valores:",  paste(apply(df_plausibles, 1, paste, collapse = ","), 
                             collapse = "; ")))
} 
 
 

   names(Resultado) <- c("Panel", "filas_elementosComunes", 
                        "FilascontiguaComunes", "prop_FilascontiguaComunes", "test_FilascontiguaComunes",
                        "FilascontiguaComunesCada2", "prop_FilascontiguaComunesCada2", "test_FilascontiguaComunesCada2",
                        "FilascontiguaComunesCada3", "prop_FilascontiguaComunesCada3", "test_FilascontiguaComunesCada3",
                        "FilascontiguaComunesCada4", "prop_FilascontiguaComunesCada4", "test_FilascontiguaComunesCada4",
                        "testGlobal", "inicia_segundaVisita")
  
  Resultado
}


######################### Fin Función para generar esquemas rotativos 2-2-2  #####################



# Escenario que usa Ecuador

# escenario_Ecuador <- f_panel(inicia_primeraVisita = c(1,3,2,4),
#                                inicia_segundaVisita = c(1,3,1,3),
#                                numTrimestres = 40)
# 
# # Escenario erroneo
# 
# escenario_ErroneoEcuador <- f_panel(inicia_primeraVisita = c(1,3,2,4), 
#                                inicia_segundaVisita = c(1,1,1,1),
#                                numTrimestres = 40)
# 
# f_admissibleScenarios222(c(1,3,2,4), 40)
# 
# # Si se omité el primer argumento colocar un default en la segunda visita
# escenario_EcuadorSinSegundaVistia <- f_panel(inicia_primeraVisita = c(1,3,2,4),
#                                       numTrimestres = 40)
# 
# 
# 
# # Escenario que usa Ecuador
# 
# escenario_Ecuador <- f_panel(inicia_primeraVisita = c(1,3,2,4),
#                                inicia_segundaVisita = c(1,3,1,3),
#                                numTrimestres = 40)
# 
# # Escenario erroneo
# 
# escenario_ErroneoEcuador <- f_panel(inicia_primeraVisita = c(1,3,2,4), 
#                                       inicia_segundaVisita = c(1,1,1,1),
#                                       numTrimestres = 40)
# 
# f_admissibleScenarios222(c(1,3,2,4), 40)
# 
# 
# 
# # Si se omité el primer argumento colocar un default en la segunda visita
# escenario_EcuadorSinSegundaVistia <- f_panel(inicia_primeraVisita = c(1,3,2,4),
#                                                numTrimestres = 40)
# 
