
f_block500 <- function(num_visitas, trimestres, letra_bloque){
  bloque_inicial <- rep(1, 5 - num_visitas + 1)
  
  if(length(bloque_inicial) == 4) bloque_inicial <- c(bloque_inicial, 2)
  if(length(bloque_inicial) == 3) bloque_inicial <- c(bloque_inicial, 2, 2)
  if(length(bloque_inicial) == 2) bloque_inicial <- c(bloque_inicial, 2, 2, 2)
  if(length(bloque_inicial) == 1) bloque_inicial <- c(bloque_inicial, 2, 2, 2, 2)
  
  vctr_bloque <- rep(NA_integer_, trimestres - length(bloque_inicial))  
  vctr_bloque[1:5] <- bloque_inicial
  

  for(i in 6:trimestres){
    if((vctr_bloque[i-1] == vctr_bloque[i-2]) &
       (vctr_bloque[i-2] == vctr_bloque[i-3]) &
       (vctr_bloque[i-3] == vctr_bloque[i-4]) & 
       (vctr_bloque[i-4] == vctr_bloque[i-5])){
      vctr_bloque[i] <- vctr_bloque[i-1] + 1  
    }
    
    
    
    if((vctr_bloque[i-1] == (vctr_bloque[i-2] + 1)) &
       ((vctr_bloque[i-2] + 1) == (vctr_bloque[i-3] + 1)) &
       ((vctr_bloque[i-3] + 1) ==  (vctr_bloque[i-4] + 1)) &
       ((vctr_bloque[i-4] + 1) == (vctr_bloque[i-5] + 1))){
      vctr_bloque[i] <- vctr_bloque[i-1]   
    }
    
    if((vctr_bloque[i-1] == vctr_bloque[i-2] ) &
       (vctr_bloque[i-2] == (vctr_bloque[i-3] + 1)) &
       ((vctr_bloque[i-3] + 1) ==  (vctr_bloque[i-4] + 1)) &
       ((vctr_bloque[i-4] + 1) == (vctr_bloque[i-5] + 1))){
      vctr_bloque[i] <- vctr_bloque[i-1]   
    }
    
    
    if((vctr_bloque[i-1] == vctr_bloque[i-2] ) &
       (vctr_bloque[i-2] == vctr_bloque[i-3]) &
       (vctr_bloque[i-3] ==  (vctr_bloque[i-4] + 1)) &
       ((vctr_bloque[i-4] + 1) == (vctr_bloque[i-5] + 1))){
      vctr_bloque[i] <- vctr_bloque[i-1]   
    }
    
    
    if((vctr_bloque[i-1] == vctr_bloque[i-2] ) &
       (vctr_bloque[i-2] == vctr_bloque[i-3]) &
       (vctr_bloque[i-3] ==  vctr_bloque[i-4]) &
       (vctr_bloque[i-4]== (vctr_bloque[i-5] + 1))){
      vctr_bloque[i] <- vctr_bloque[i-1]   
    }
    
    
    
  }
  vctr_bloque <- paste0(letra_bloque, vctr_bloque)
  vctr_bloque
}


f_panel500 <- function(vctr_inicial_numVisita, trimestres,
                            letra_bloques = c("A", "B", "C", "D", "E")){
  if(trimestres < 6) stop("Cómo mínimo debe considerar 6 trimestres")
  if(!identical(sort(as.integer(unique(vctr_inicial_numVisita))), 1:5)) stop("El vector vctr_inicial_numVisita debe ser cualquier permutación del vector 1, 2, 3, 4, 5 en cualquier orden, por ejemplo: 3, 5, 1, 3, 2")
  if(length(unique(letra_bloques)) != 5 | !is.character(letra_bloques)) stop ("Debe especificar 5 letras diferentes")
  resultado_esquema <- cbind(
    f_block500(vctr_inicial_numVisita[1], trimestres, letra_bloques[1]), 
    f_block500(vctr_inicial_numVisita[2], trimestres, letra_bloques[2]),
    f_block500(vctr_inicial_numVisita[3], trimestres, letra_bloques[3]),
    f_block500(vctr_inicial_numVisita[4], trimestres, letra_bloques[4]),
    f_block500(vctr_inicial_numVisita[5], trimestres, letra_bloques[5]))
  resultado_esquema
  }
  
# 
# f_panel500(vctr_inicial_numVisita = c(3, 2, 4, 1, 5), trimestre = 40,
#                 letra_bloques = c("A", "B", "C", "D", "E"))
