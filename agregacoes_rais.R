agrega_escolaridade <- function(escolaridade_rais){
  analfabeto <- c("ANALFABETO")
  ef_incompleto <- c("ATE 5.A INC",  "5.A CO FUND",  "6. A 9. FUND")
  ef_completo <- c("FUND COMPL", "MEDIO INCOMP")
  em_completo <- c("MEDIO COMPL", "SUP. INCOMP")
  sup_completo <- c("SUP. COMP", "MESTRADO", "DOUTORADO")
  
  if (escolaridade_rais %in% analfabeto){
    escolaridade_thais <- "ANALFABETO"
  }
  else if (escolaridade_rais %in% ef_incompleto){
    escolaridade_thais <- "ENSINO FUNDAMENTAL INCOMPLETO"
  }
  else if (escolaridade_rais %in% ef_completo){
    escolaridade_thais <- "ENSINO FUNDAMENTAL COMPLETO"
  }
  else if (escolaridade_rais %in% em_completo){
    escolaridade_thais <- "ENSINO MÉDIO COMPLETO"
  }
  else if (escolaridade_rais %in% sup_completo){
    escolaridade_thais <- "ENSINO SUPERIOR COMPLETO"
  }
  else {
    stop("Invalido: Escolaridade")
  }
  
  return(escolaridade_thais)
}  

agrega_cod_escolaridade <- function(escolaridade_rais){
  analfabeto <- c("1")
  ef_incompleto <- c("2",  "3",  "4")
  ef_completo <- c("5", "6")
  em_completo <- c("7", "8")
  sup_completo <- c("9", "10", "11")
  
  if (escolaridade_rais %in% analfabeto){
    escolaridade_thais <- "1"
  }
  else if (escolaridade_rais %in% ef_incompleto){
    escolaridade_thais <- "2"
  }
  else if (escolaridade_rais %in% ef_completo){
    escolaridade_thais <- "3"
  }
  else if (escolaridade_rais %in% em_completo){
    escolaridade_thais <- "4"
  }
  else if (escolaridade_rais %in% sup_completo){
    escolaridade_thais <- "5"
  }
  else {
    stop("Invalido: Escolaridade")
  }
  
  return(escolaridade_thais)
}  

agrega_raca <- function(raca_rais){

  pretos_pardos <- c("Preta", "Parda")
  
  if (raca_rais %in% pretos_pardos){
    return ("Pretos e Pardos")
  }
  return (raca_rais)
}  

agrega_cod_raca <- function(raca_rais){
  if (raca_rais == 1 || raca_rais == 2){
    raca = raca_rais
  }
  if (raca_rais == 4 || raca_rais == 8){
    raca = "3"
  }
  if (raca_rais == 6){
    raca = "4"
  }
  if (raca_rais == 9){
    raca = "5"
  }
  if (raca_rais == -1){
    raca = "-1"
  }
  return(raca)
  
}


