library('googledrive')
library('stringr')
library('readxl')

# Coenxiòn con la cuenta de Google Drive
drive_auth()


read.data.epidemio <- function(wave = "25-26", week = week)
{
  folder.data <- drive_get(
    paste0("Repositorio/Epidemiologia/Casos Temp ", wave)
    )
  
  files.data <- drive_ls(folder.data)
  data.position <- which(str_ends(files.data$name, paste0(week, ".xlsx")))
  
  print("La carpeta cuenta con estos archivos")
  print(files.data)
  
  drive_download(as_id(files.data$id[data.position]), 
                 path = files.data$name[data.position], 
                 overwrite = TRUE)
  
  data <- read_excel(files.data$name[data.position])
  
  return(data)
  
}


