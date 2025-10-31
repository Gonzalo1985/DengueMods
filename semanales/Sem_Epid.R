Sem_Epid <- function(anio = 1990:2024, semana = 1:52){
  
  anios <- as.numeric(substr(as.character(anio), 3, 4))
  anios_dig <- formatC(anios, width = 2, flag = 0)
  sems_dig <- formatC(semana, width = 2, flag = 0)
  
  Semanas <- c()
  
  for (x in anios_dig) {
    SemEpid <- paste0(sems_dig, "/", x)
    Semanas <- c(Semanas, SemEpid)
  }
  return(Semanas)
}

