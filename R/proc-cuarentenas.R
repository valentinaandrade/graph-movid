# Code 2: Cuarentenas

# Cargar libreria
pacman::p_load(googlesheets4, casen)

### Cargar datos cuarentenas
data.cuarentena <- read_sheet("https://docs.google.com/spreadsheets/d/1m-UOSySxvqRJp8LjYO26282VWD6L5FU5gm5o8Mx7Uy4/edit?usp=sharing",
                              sheet = "Hoja 1", range = cell_rows(c(2, NA)))


data.cuarentena$comuna <- zoo::na.locf(data.cuarentena$...2) # Rellenar NA de comuna
data.cuarentena <- data.cuarentena %>% slice(.,1:(which(data.cuarentena == "Cordones sanitarios", arr.ind=TRUE)[1]-2))  # Eliminar data redundante
data.cuarentena$...3 <- data.cuarentena$...2 <- data.cuarentena$...1 <- NULL

# Limpiar base colapsando por comuna y asignando 1 a las comunas con al menos una parte de ella en cuarentena en cada día
data.cuarentena$comuna <- sub(pattern=" urbano",replacement = "", x=data.cuarentena$comuna)  # Eliminar "urbano" de algunas comunas
data.cuarentena <- data.cuarentena %>% group_by(comuna) %>% mutate_each(funs(as.numeric)) %>% 
  summarise_all(funs(sum)) %>%  ungroup() %>% 
  mutate_at(vars(-comuna),funs(ifelse(.>0,1,0)))

# Pasar a formato long y arreglar fechas
cuarentena_long <- tidyr:::gather(data.cuarentena, dia, cuarentena,colnames(data.cuarentena)[2]:tail(colnames(data.cuarentena),n=1), factor_key=F)
cuarentena_long <- cuarentena_long %>% separate(dia, sep ="-", c("day","month"))  %>% 
  mutate(year=2020,
         month=tolower(month),
         month=ifelse(month=="mar.",3,
                      ifelse(month=="abr.",4,
                             ifelse(month=="may.",5,
                                    ifelse(month=="jun.",6,
                                           ifelse(month=="jul.",7,
                                                  ifelse(month=="ago.",8,
                                                         ifelse(month=="sept.",9,NA))))))),
         date=lubridate::make_date(year, month, day),
         semana=lubridate::week(date))

cuarentena_long <- cuarentena_long %>% dplyr::select(-day,-month,-year)

# Generar base por semana + generar variable lag de 1 y 2 semanas para explorar efectos con delay
cuarentena_longwk <- cuarentena_long  %>% group_by(comuna)  %>% arrange(comuna,date) %>% 
  dplyr::select(-date) %>% group_by(comuna, semana) %>% 
  summarise_all(funs(mean), na.rm = TRUE) %>%                                         
  mutate(cuarentenawk = cuarentena,
         cuarentenawk_lag1 = lag(cuarentena, n=1),
         cuarentenawk_lag2 = lag(cuarentena, n=2),
         cuarentenawk = tidyr::replace_na(ifelse(cuarentenawk>=0.5,1,0),0), # Dicotomizar cuarentena semanal
         cuarentenawk_lag1 = tidyr::replace_na(ifelse(cuarentenawk_lag1>=0.5,1,0),0), # Dicotomizar cuarentena semanal y reemplazar NA
         cuarentenawk_lag2 = tidyr::replace_na(ifelse(cuarentenawk_lag2>=0.5,1,0),0))  %>%   # Dicotomizar cuarentena semanal reemplazar NA
  dplyr::select(-cuarentena)

### Merge
cuarentenas <- left_join(cuarentena_long, cuarentena_longwk, by=c("comuna", "semana"))

# Homogeneizar variables para el merge
#data$fecha <- as.Date(data$fecha_obs)
cuarentenas$fecha <- as.Date(cuarentenas$date)
#cuarentenas$comuna <- tolower(stringi::stri_trans_general(cuarentenas$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula
#data$comuna <- tolower(stringi::stri_trans_general(data$u2_comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula

# Base final
#data <- left_join(data, cuarentenas, by=c("comuna", "fecha", "semana"))
data <- cuarentenas 
# Asignar 0 a las comunas sin cuarentenas en las fechas y semanas correspondientes
data$cuarentena <- tidyr::replace_na(data$cuarentena,0)
data$cuarentenawk <- tidyr::replace_na(data$cuarentenawk,0)
data$cuarentenawk_lag1 <- tidyr::replace_na(data$cuarentenawk_lag1,0)
data$cuarentenawk_lag2 <- tidyr::replace_na(data$cuarentenawk_lag2,0)

#data <- na.omit(data)
x<- merge(data, casen::codigos_subdere, by.x = "comuna", by.y = "nombre_comuna", all.x = T)

# exportar ----------------------------------------------------------------
saveRDS(data, "output/cuarentenas.rds")
table(data$semana)

