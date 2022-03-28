#' @title Genración de datos de CO2 en l'eliana
#'
#' @description Genración de datos de CO2 en l'eliana
#'
#' @param id_dispositivo
#'
#' @return json
#'
#' @examples  sim_co2_vaqao(id_dispositivo)
#'
#' @import httr
#' dplyr
#' jsonlite
#' lubridate
#'
#' @export


sim_co2_vaqao <- function(id_dispositivo){

  id_dispositivo <- as.character(id_dispositivo)

  # -----------------------------------------------------------------------------
  # PETICIÓN TOKEN THB
  # -----------------------------------------------------------------------------
  cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
  post <- httr::POST(url = "http://212.227.148.73:31956/api/auth/login",
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)


  # -----------------------------------------------------------------------------
  # PETICIÓN DATOS SENSOR
  # -----------------------------------------------------------------------------

  #id_dispositivo <- "003fd2d0-ae67-11ec-9ff7-abd6deb577d1"
  keys <- URLencode(c("CO2,TVOC,Temperatura,Humedad"))
  fecha_inicial <- Sys.time()
  hour(fecha_inicial) <- hour(fecha_inicial) -5
  fecha_inicial <- format(as.numeric(as.POSIXct(fecha_inicial))*1000,scientific = F)
  fecha_final <- Sys.time()
  fecha_final <- format(as.numeric(as.POSIXct(fecha_final))*1000,scientific = F)

  url_thb_fechas <- paste("http://212.227.148.73:31956/api/plugins/telemetry/DEVICE/",id_dispositivo,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_inicial,"&endTs=",fecha_final,sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))


  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df <- as.data.frame(df)

  # Gestión df sin datos
  fecha_final <- Sys.time()
  minute(fecha_final) <- minute(fecha_final) + 30
  fecha_final <- format(as.numeric(as.POSIXct(fecha_final))*1000,scientific = F)
  if(nrow(df) == 0){  # Sin datos
    # Simulación datos
    CO2 <- round(runif(1, min=550, max=650),0)
    Temperatura <- round(runif(1, min=18, max=22),2)
    Humedad <- round(runif(1, min=28, max=36),0)
    TVOC <- round(runif(1, min=200, max=240),0)
    df <- data.frame(CO2,Temperatura, Humedad, TVOC,stringsAsFactors = FALSE)
  }else{  # Si hay datos
    df <- df[,c(1,2,4,6,8)]
    colnames(df) <- c("ts","CO2","TVOC","Temperatura","Humedad")
    df$CO2 <- as.numeric(df$CO2)
    df$TVOC <- as.numeric(df$TVOC)
    df$Temperatura <- as.numeric(df$Temperatura)
    df$Humedad <- as.numeric(df$Humedad)
    # Simulación datos
    # Suma o resta
    if(round(runif(1, min=0, max=1)) == 1){  # Suma
      df$CO2 <- df$CO2 + round(runif(1, min=1, max=20),0)
      df$Temperatura <- df$Temperatura + round(runif(1, min=0.1, max=0.3),2)
      df$Humedad <- df$Humedad + round(runif(1, min=1, max=3),0)
      df$TVOC <- df$TVOC + round(runif(1, min=1, max=12),0)
    }else{  # Resta
      df$CO2 <- df$CO2 - round(runif(1, min=1, max=20),0)
      df$Temperatura <- df$Temperatura - round(runif(1, min=0.1, max=0.3),2)
      df$Humedad <- df$Humedad - round(runif(1, min=1, max=3),0)
      df$TVOC <- df$TVOC - round(runif(1, min=1, max=12),0)
    }
  }
  df$ts <- fecha_final

  # Envío a plataforma
  json_envio_plataforma <- paste('{"CO2":', df$CO2,',',
                                 '"Temperatura":',df$Temperatura, ',',
                                 '"Humedad":',df$Humedad, ',',
                                 '"TVOC":',df$TVOC, '}',
                                 sep = "")

  # GET token del dispositivo
  url <- paste("http://212.227.148.73:31956/api/device/",id_dispositivo,"/credentials",sep = "")
  get_token <- httr::GET(url = url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  token <- jsonlite::fromJSON(rawToChar(get_token$content))
  token <- token$credentialsId

  url <- paste("http://212.227.148.73:31956/api/v1/",token,"/telemetry",sep = "")

  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                     body = json_envio_plataforma,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  return(1)
}


