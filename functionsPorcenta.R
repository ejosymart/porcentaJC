DownloadPorcenta <- function (directorio, dirUrl, inicio, fin, ...) {
    tiempo <- seq(as.Date(inicio), as.Date(fin), by = "day")
    tiempo2 <- strftime(tiempo, format = "%d%m%Y")
    for (i in 1:length(tiempo2)) {
        tempUrl <- paste0(dirUrl, tiempo2[i], ".xlsx")
        tempUrl <- ifelse(url.exists(tempUrl), tempUrl, paste0(dirUrl, tiempo2[i], "_.xlsx"))
        try(download.file(url = tempUrl, 
                          destfile = paste0(directorio, "imarpe_rpelag_porfinal", tiempo2[i], ".xlsx"), method = "internal", 
                          mode = "wb"))
    }
}

DownloadPorcentaJC_Industrial <- function(directorio, dirUrl, inicio, fin, ...){
  tiempo  <- seq(as.Date(inicio), as.Date(fin), by = "day")
  tiempo2 <- strftime(tiempo, format = "%d%m%Y")
  for (i in 1:length(tiempo2)){
    tempUrl <- paste0(dirUrl, tiempo2[i], ".xlsx")
    tempUrl <- ifelse(url.exists(tempUrl), tempUrl, paste0(dirUrl, tiempo2[i], ".xlsx"))
    try(download.file(url = tempUrl, 
                      destfile = paste0(directorio, "imarpe_repo_jure_", tiempo2[i], ".xlsx"), method = "internal", 
                      mode = "wb"))
  }
}



ReadPorcenta <- function (directorio, 
                          inicio, 
                          fin, ...){
    out.porcenta = list()
    list.porcenta = NULL
    tiempo <- seq(as.Date(inicio), as.Date(fin), by = "day")
    tiempo2 <- strftime(tiempo, format = "%d%m%Y")
    desembarque <- NULL
    n.embarcaciones <- NULL
    e.muestreadas <- NULL
    p.juveniles <- NULL
    moda <- NULL
    for (i in 1:length(tiempo2)) {
      file_name <- paste(directorio, paste("imarpe_rpelag_porfinal", 
            tiempo2[i], ".xlsx", sep = ""), sep = "")
        wb <- NULL
        wb <- tryCatch({
            loadWorkbook(file_name)
        }, error = function(e) {
            message("Not Found", file_name)
            return(wb)
        })
        if (!is.null(wb)) {
            desemb <- readWorksheet(wb, sheet = "reporte", startRow = 11, 
                startCol = 3, endRow = 12, endCol = 40)
            n.embar <- readWorksheet(wb, sheet = "reporte", startRow = 12, 
                startCol = 3, endRow = 13, endCol = 40)
            e.muest <- readWorksheet(wb, sheet = "reporte", startRow = 13, 
                startCol = 3, endRow = 14, endCol = 40)
            p.juv <- readWorksheet(wb, sheet = "reporte", startRow = 14, 
                startCol = 3, endRow = 15, endCol = 40)
            mod <- readWorksheet(wb, sheet = "reporte", startRow = 15, 
                startCol = 3, endRow = 16, endCol = 40)
        }
        else {
            desemb <- rep(NA, 38)
            n.embar <- rep(NA, 38)
            e.muest <- rep(NA, 38)
            p.juv <- rep(NA, 38)
            mod <- rep(NA, 38)
        }
        porcenta = print(tiempo2[i])
        list.porcenta = rbind(list.porcenta, porcenta)
        desemb1 <- t(matrix(as.numeric(desemb)))
        desembarque <- rbind(desembarque, desemb1)
        n.embar1 <- t(matrix(as.numeric(n.embar)))
        n.embarcaciones <- rbind(n.embarcaciones, n.embar1)
        e.muest1 <- t(matrix(as.numeric(e.muest)))
        e.muestreadas <- rbind(e.muestreadas, e.muest1)
        p.juv1 <- t(matrix(as.numeric(p.juv)))
        p.juveniles <- rbind(p.juveniles, p.juv1)
        mod1 <- t(matrix(as.numeric(mod)))
        moda <- rbind(moda, mod1)
    }
    puertos_porcentas <- c("Paita", "Paita", "Parachique", "Parachique", 
        "Chicama", "Chicama", "Chimbote", "Chimbote", "Samanco", 
        "Samanco", "Casma", "Casma", "Huarmey", "Huarmey", "Supe", 
        "Supe", "Vegueta", "Vegueta", "Huacho", "Huacho", "Chancay", 
        "Chancay", "Callao", "Callao", "T.Mora", "T.Mora", "Pisco", 
        "Pisco", "Atico", "Atico", "Planchada", "Planchada", 
        "Quilca", "Quilca", "Mollendo", "Mollendo", "Ilo", "Ilo")
    tipo <- c(rep(c("Ind", "Ind Mad"), length(puertos_porcentas)/2))
    puerto <- c("tiempo", as.character(tiempo))
    desembarque[is.na(desembarque)] <- 0
    n.embarcaciones[is.na(n.embarcaciones)] <- 0
    e.muestreadas[is.na(e.muestreadas)] <- 0
    p.juveniles[is.na(p.juveniles)] <- 0
    moda[is.na(moda)] <- 0
    desembarque <- data.frame(desembarque)
    n.embarcaciones <- data.frame(n.embarcaciones)
    e.muestreadas <- data.frame(e.muestreadas)
    p.juveniles <- data.frame(p.juveniles)
    moda <- data.frame(moda)
    desembarque <- data.frame(rbind(tipo, desembarque))
    names(desembarque) <- puertos_porcentas
    n.embarcaciones <- data.frame(rbind(tipo, n.embarcaciones))
    names(n.embarcaciones) <- puertos_porcentas
    e.muestreadas <- data.frame(rbind(tipo, e.muestreadas))
    names(e.muestreadas) <- puertos_porcentas
    p.juveniles <- data.frame(rbind(tipo, p.juveniles))
    names(p.juveniles) <- puertos_porcentas
    moda <- data.frame(rbind(tipo, moda))
    names(moda) <- puertos_porcentas
    desembarque <- cbind(puerto, desembarque)
    n.embarcaciones <- cbind(puerto, n.embarcaciones)
    e.muestreadas <- cbind(puerto, e.muestreadas)
    p.juveniles <- cbind(puerto, p.juveniles)
    moda <- cbind(puerto, moda)
    out.porcenta$desembarque = desembarque
    out.porcenta$n.embarcaciones = n.embarcaciones
    out.porcenta$e.muestreadas = e.muestreadas
    out.porcenta$p.juveniles = p.juveniles
    out.porcenta$moda = moda
    return(out.porcenta)
}


ReadPorcentaJC_Industrial <- function (directorio, 
                                       inicio, 
                                       fin, ...){
  
    out.porcenta                    <- list()
    out.porcenta$Jurel              <- list()
    out.porcenta$Caballa            <- list()
    out.porcenta$Incidental_Jurel   <- list()
    out.porcenta$Incidental_Caballa <- list() 
    
    list.porcenta           <- NULL
    tiempo                  <- seq(as.Date(inicio), as.Date(fin), by = "day")
    tiempo2                 <- strftime(tiempo, format = "%d%m%Y")
    
    # Jurel
    desembarque_Jurel       <- NULL
    n.embarcaciones_Jurel   <- NULL
    e.muestreadas_Jurel     <- NULL
    p.juveniles_Jurel       <- NULL
    moda_Jurel              <- NULL
    
    #Caballa
    desembarque_Caballa     <- NULL
    n.embarcaciones_Caballa <- NULL
    e.muestreadas_Caballa   <- NULL
    p.juveniles_Caballa     <- NULL
    moda_Caballa            <- NULL
    
    #Otras especies
    OtrosSp                 <- NULL
    Xwarnings               <- NULL
    
    #Incidental Jurel
    desembarque_incidental_Jurel     <- NULL 
    n.embarcaciones_incidental_Jurel <- NULL
    e.muestreadas_incidental_Jurel   <- NULL
    
    #Incidental Caballa
    desembarque_incidental_Caballa     <- NULL 
    n.embarcaciones_incidental_Caballa <- NULL
    e.muestreadas_incidental_Caballa   <- NULL
    
    
    #Bucle para crear porcenta
    for (i in 1:length(tiempo2)) {
        file_name <- paste(directorio, paste("imarpe_repo_jure_", tiempo2[i], ".xlsx", sep = ""), sep = "")
        wb <- NULL
        wb <- tryCatch({
            loadWorkbook(file_name)
        }, error = function(e) {
            message("Not Found", file_name)
            return(wb)
        })
        if (!is.null(wb)){
            #Jurel
            desemb_Jurel  <- readWorksheet(wb, sheet = "reporte", startRow = 11, startCol = 3, endRow = 12, endCol = 18)
            n.embar_Jurel <- readWorksheet(wb, sheet = "reporte", startRow = 12, startCol = 3, endRow = 13, endCol = 18)
            e.muest_Jurel <- readWorksheet(wb, sheet = "reporte", startRow = 13, startCol = 3, endRow = 14, endCol = 18)
            p.juv_Jurel   <- readWorksheet(wb, sheet = "reporte", startRow = 14, startCol = 3, endRow = 15, endCol = 18)
            mod_Jurel     <- readWorksheet(wb, sheet = "reporte", startRow = 15, startCol = 3, endRow = 16, endCol = 18)
            
            #Caballa
            desemb_Caballa  <- readWorksheet(wb, sheet = "reporte", startRow = 17, startCol = 3, endRow = 18, endCol = 18)
            n.embar_Caballa <- readWorksheet(wb, sheet = "reporte", startRow = 18, startCol = 3, endRow = 19, endCol = 18)
            e.muest_Caballa <- readWorksheet(wb, sheet = "reporte", startRow = 19, startCol = 3, endRow = 20, endCol = 18)
            p.juv_Caballa   <- readWorksheet(wb, sheet = "reporte", startRow = 20, startCol = 3, endRow = 21, endCol = 18)
            mod_Caballa     <- readWorksheet(wb, sheet = "reporte", startRow = 21, startCol = 3, endRow = 22, endCol = 18)
            
            #Incidental Jurel
            desemb_incidental_Jurel  <- readWorksheet(wb, sheet = "reporte", startRow = 33, startCol = 3, endRow = 34, endCol = 18)
            n.embar_incidental_Jurel <- readWorksheet(wb, sheet = "reporte", startRow = 34, startCol = 3, endRow = 35, endCol = 18)
            e.muest_incidental_Jurel <- readWorksheet(wb, sheet = "reporte", startRow = 35, startCol = 3, endRow = 36, endCol = 18)
            
            #Incidental Caballa
            desemb_incidental_Caballa  <- readWorksheet(wb, sheet = "reporte", startRow = 37, startCol = 3, endRow = 38, endCol = 18)
            n.embar_incidental_Caballa <- readWorksheet(wb, sheet = "reporte", startRow = 38, startCol = 3, endRow = 39, endCol = 18)
            e.muest_incidental_Caballa <- readWorksheet(wb, sheet = "reporte", startRow = 39, startCol = 3, endRow = 40, endCol = 18)
            
            #Otros
            OTROS   <- readWorksheet(wb, sheet = "reporte", startRow = 23, startCol = 2, endRow = 30, endCol = 19)
            OTROS   <- OTROS[, -ncol(OTROS)]
            if (dim(OTROS)[2] < 17) {
                OTROS <- cbind(OTROS, matrix(NA, ncol = 17 - dim(OTROS)[2], nrow = nrow(OTROS)))
            }
        }
        else {
            #Jurel
            desemb_Jurel     <- rep(NA, 17)
            n.embar_Jurel    <- rep(NA, 17)
            e.muest_Jurel    <- rep(NA, 17)
            p.juv_Jurel      <- rep(NA, 17)
            mod_Jurel        <- rep(NA, 17)
            
            #Caballa
            desemb_Caballa   <- rep(NA, 17)
            n.embar_Caballa  <- rep(NA, 17)
            e.muest_Caballa  <- rep(NA, 17)
            p.juv_Caballa    <- rep(NA, 17)
            mod_Caballa      <- rep(NA, 17)
            
            #Incidental Jurel
            desemb_incidental_Jurel  <- rep(NA, 17)
            n.embar_incidental_Jurel <- rep(NA, 17)
            e.muest_incidental_Jurel <- rep(NA, 17)
            
            #Incidental Caballa
            desemb_incidental_Caballa  <- rep(NA, 17)
            n.embar_incidental_Caballa <- rep(NA, 17)
            e.muest_incidental_Caballa <- rep(NA, 17)
            
            #Otros
            OTROS      <- data.frame(matrix(NA, ncol = 17, nrow = 7))
            OTROS[, 1] <- "NONE"
        }
        
        Xwarnings             <- rbind(Xwarnings, tiempo2[i])
        porcenta              <- print(tiempo2[i])
        
        # Jurel
        desemb1_Jurel         <- t(matrix(as.numeric(desemb_Jurel)))
        desembarque_Jurel     <- rbind(desembarque_Jurel, desemb1_Jurel)
        
        n.embar1_Jurel        <- t(matrix(as.numeric(n.embar_Jurel)))
        n.embarcaciones_Jurel <- rbind(n.embarcaciones_Jurel, n.embar1_Jurel)
        
        e.muest1_Jurel        <- t(matrix(as.numeric(e.muest_Jurel)))
        e.muestreadas_Jurel   <- rbind(e.muestreadas_Jurel, e.muest1_Jurel)
        
        p.juv1_Jurel          <- t(matrix(as.numeric(p.juv_Jurel)))
        p.juveniles_Jurel     <- rbind(p.juveniles_Jurel, p.juv1_Jurel)
        
        mod1_Jurel            <- t(matrix(as.numeric(mod_Jurel)))
        moda_Jurel            <- rbind(moda_Jurel, mod1_Jurel)
        
        # Caballa
        desemb1_Caballa         <- t(matrix(as.numeric(desemb_Caballa)))
        desembarque_Caballa     <- rbind(desembarque_Caballa, desemb1_Caballa)
        
        n.embar1_Caballa        <- t(matrix(as.numeric(n.embar_Caballa)))
        n.embarcaciones_Caballa <- rbind(n.embarcaciones_Caballa, n.embar1_Caballa)
        
        e.muest1_Caballa        <- t(matrix(as.numeric(e.muest_Caballa)))
        e.muestreadas_Caballa   <- rbind(e.muestreadas_Caballa, e.muest1_Caballa)
        
        p.juv1_Caballa          <- t(matrix(as.numeric(p.juv_Caballa)))
        p.juveniles_Caballa     <- rbind(p.juveniles_Caballa, p.juv1_Caballa)
        
        mod1_Caballa            <- t(matrix(as.numeric(mod_Caballa)))
        moda_Caballa            <- rbind(moda_Caballa, mod1_Caballa)
        
        
        #Incidental Jurel
        desemb1_incidental_Jurel         <- t(matrix(as.numeric(desemb_incidental_Jurel)))
        desembarque_incidental_Jurel     <- rbind(desembarque_incidental_Jurel, desemb1_incidental_Jurel)
        
        n.embar1_incidental_Jurel        <- t(matrix(as.numeric(n.embar_incidental_Jurel)))
        n.embarcaciones_incidental_Jurel <- rbind(n.embarcaciones_incidental_Jurel, n.embar1_incidental_Jurel)
        
        e.muest1_incidental_Jurel        <- t(matrix(as.numeric(e.muest_incidental_Jurel)))
        e.muestreadas_incidental_Jurel   <- rbind(e.muestreadas_incidental_Jurel, e.muest1_incidental_Jurel)
        
        
        #Incidental Caballa
        desemb1_incidental_Caballa         <- t(matrix(as.numeric(desemb_incidental_Caballa)))
        desembarque_incidental_Caballa     <- rbind(desembarque_incidental_Caballa, desemb1_incidental_Caballa)
        
        n.embar1_incidental_Caballa        <- t(matrix(as.numeric(n.embar_incidental_Caballa)))
        n.embarcaciones_incidental_Caballa <- rbind(n.embarcaciones_incidental_Caballa, n.embar1_incidental_Caballa)
        
        e.muest1_incidental_Caballa        <- t(matrix(as.numeric(e.muest_incidental_Caballa)))
        e.muestreadas_incidental_Caballa   <- rbind(e.muestreadas_incidental_Caballa, e.muest1_incidental_Caballa)
        
        
        #Otros
        OtrosSp                 <- rbind(OtrosSp, OTROS)
    }
    
    # NameSp            <- unique(OtrosSp$Especies)
    
    puertos_porcentas <- c("Paita", "Parachique", "Chicama", 
                           "Chimbote", "Samanco", "Supe", 
                           "Vegueta", "Huacho", "Chancay", 
                           "Callao", "T.Mora", "Pisco", 
                           "Atico", "Planchada", "Mollendo", "Ilo")

    Fecha            <- as.character(tiempo)
    
    #Jurel
    desembarque_Jurel[is.na(desembarque_Jurel)]         <- 0
    n.embarcaciones_Jurel[is.na(n.embarcaciones_Jurel)] <- 0
    e.muestreadas_Jurel[is.na(e.muestreadas_Jurel)]     <- 0
    p.juveniles_Jurel[is.na(p.juveniles_Jurel)]         <- 0
    moda_Jurel[is.na(moda_Jurel)]                       <- 0
    
    #Caballa
    desembarque_Caballa[is.na(desembarque_Caballa)]         <- 0
    n.embarcaciones_Caballa[is.na(n.embarcaciones_Caballa)] <- 0
    e.muestreadas_Caballa[is.na(e.muestreadas_Caballa)]     <- 0
    p.juveniles_Caballa[is.na(p.juveniles_Caballa)]         <- 0
    moda_Caballa[is.na(moda_Caballa)]                       <- 0
    
    #Incidental Jurel
    desembarque_incidental_Jurel[is.na(desembarque_incidental_Jurel)]         <- 0
    n.embarcaciones_incidental_Jurel[is.na(n.embarcaciones_incidental_Jurel)] <- 0
    e.muestreadas_incidental_Jurel[is.na(e.muestreadas_incidental_Jurel)]     <- 0
    
    #Incidental Caballa
    desembarque_incidental_Caballa[is.na(desembarque_incidental_Caballa)]         <- 0
    n.embarcaciones_incidental_Caballa[is.na(n.embarcaciones_incidental_Caballa)] <- 0
    e.muestreadas_incidental_Caballa[is.na(e.muestreadas_incidental_Caballa)]     <- 0
    
    #Otros
    OtrosSp[is.na(OtrosSp)] <- 0
    
    #Jurel
    desembarque_Jurel       <- data.frame(desembarque_Jurel)
    n.embarcaciones_Jurel   <- data.frame(n.embarcaciones_Jurel)
    e.muestreadas_Jurel     <- data.frame(e.muestreadas_Jurel)
    p.juveniles_Jurel       <- data.frame(p.juveniles_Jurel)
    moda_Jurel              <- data.frame(moda_Jurel)
    
    #Caballa
    desembarque_Caballa     <- data.frame(desembarque_Caballa)
    n.embarcaciones_Caballa <- data.frame(n.embarcaciones_Caballa)
    e.muestreadas_Caballa   <- data.frame(e.muestreadas_Caballa)
    p.juveniles_Caballa     <- data.frame(p.juveniles_Caballa)
    moda_Caballa            <- data.frame(moda_Caballa)
    
    # Incidental_Jurel
    desembarque_incidental_Jurel     <- data.frame(desembarque_incidental_Jurel)
    n.embarcaciones_incidental_Jurel <- data.frame(n.embarcaciones_incidental_Jurel)
    e.muestreadas_incidental_Jurel   <- data.frame(e.muestreadas_incidental_Jurel)
    
    # Incidental Caballa
    desembarque_incidental_Caballa     <- data.frame(desembarque_incidental_Caballa)
    n.embarcaciones_incidental_Caballa <- data.frame(n.embarcaciones_incidental_Caballa)
    e.muestreadas_incidental_Caballa   <- data.frame(e.muestreadas_incidental_Caballa)
    
    #Otros
    OtrosSp                 <- data.frame(OtrosSp)
    
    #Jurel
    names(desembarque_Jurel)     <- puertos_porcentas
    names(n.embarcaciones_Jurel) <- puertos_porcentas
    names(e.muestreadas_Jurel)   <- puertos_porcentas
    names(p.juveniles_Jurel)     <- puertos_porcentas
    names(moda_Jurel)            <- puertos_porcentas
    
    #Caballa
    names(desembarque_Caballa)     <- puertos_porcentas
    names(n.embarcaciones_Caballa) <- puertos_porcentas
    names(e.muestreadas_Caballa)   <- puertos_porcentas
    names(p.juveniles_Caballa)     <- puertos_porcentas
    names(moda_Caballa)            <- puertos_porcentas
    
    #Incidental Jurel
    names(desembarque_incidental_Jurel)     <- puertos_porcentas
    names(n.embarcaciones_incidental_Jurel) <- puertos_porcentas
    names(e.muestreadas_incidental_Jurel)   <- puertos_porcentas
    
    #Incidental Caballa
    names(desembarque_incidental_Caballa)     <- puertos_porcentas
    names(n.embarcaciones_incidental_Caballa) <- puertos_porcentas
    names(e.muestreadas_incidental_Caballa)   <- puertos_porcentas
    
    #Otros
    names(OtrosSp)         <- c("Especies", puertos_porcentas)
    
    
    #Jurel
    desembarque_Jurel      <- cbind(Fecha, desembarque_Jurel)
    n.embarcaciones_Jurel  <- cbind(Fecha, n.embarcaciones_Jurel)
    e.muestreadas_Jurel    <- cbind(Fecha, e.muestreadas_Jurel)
    p.juveniles_Jurel      <- cbind(Fecha, p.juveniles_Jurel)
    moda_Jurel             <- cbind(Fecha, moda_Jurel)
    
    #Caballa
    desembarque_Caballa            <- cbind(Fecha, desembarque_Caballa)
    n.embarcaciones_Caballa        <- cbind(Fecha, n.embarcaciones_Caballa)
    e.muestreadas_Caballa          <- cbind(Fecha, e.muestreadas_Caballa)
    p.juveniles_Caballa            <- cbind(Fecha, p.juveniles_Caballa)
    moda_Caballa                   <- cbind(Fecha, moda_Caballa)
    
    #Incidental Jurel
    desembarque_incidental_Jurel      <- cbind(Fecha, desembarque_incidental_Jurel)
    n.embarcaciones_incidental_Jurel  <- cbind(Fecha, n.embarcaciones_incidental_Jurel)
    e.muestreadas_incidental_Jurel    <- cbind(Fecha, e.muestreadas_incidental_Jurel)
    
    #Incidental Caballa
    desembarque_incidental_Caballa      <- cbind(Fecha, desembarque_incidental_Caballa)
    n.embarcaciones_incidental_Caballa  <- cbind(Fecha, n.embarcaciones_incidental_Caballa)
    e.muestreadas_incidental_Caballa    <- cbind(Fecha, e.muestreadas_incidental_Caballa)
    
    #Otros
    OtrosSp           <- cbind(sort(rep(Fecha, times = 7)), OtrosSp)
    names(OtrosSp)[1] <- "Fecha"
    
    Xwarnings <- data.frame(Fechas = Xwarnings)
    
    #Salida
    out.porcenta$Jurel$desembarque_Jurel       <- data.frame(desembarque_Jurel, row.names = 1)
    out.porcenta$Jurel$n.embarcaciones_Jurel   <- data.frame(n.embarcaciones_Jurel, row.names = 1)
    out.porcenta$Jurel$e.muestreadas_Jurel     <- data.frame(e.muestreadas_Jurel, row.names = 1)
    out.porcenta$Jurel$p.juveniles_Jurel       <- data.frame(p.juveniles_Jurel, row.names = 1)
    out.porcenta$Jurel$moda_Jurel              <- data.frame(moda_Jurel, row.names = 1)
    
    out.porcenta$Caballa$desembarque_Caballa     <- data.frame(desembarque_Caballa, row.names = 1)
    out.porcenta$Caballa$n.embarcaciones_Caballa <- data.frame(n.embarcaciones_Caballa, row.names = 1)
    out.porcenta$Caballa$e.muestreadas_Caballa   <- data.frame(e.muestreadas_Caballa, row.names = 1)
    out.porcenta$Caballa$p.juveniles_Caballa     <- data.frame(p.juveniles_Caballa, row.names = 1)
    out.porcenta$Caballa$moda_Caballa            <- data.frame(moda_Caballa, row.names = 1)
    
    out.porcenta$Incidental_Jurel$desembarque_incidental_Jurel     <- data.frame(desembarque_incidental_Jurel, row.names = 1)
    out.porcenta$Incidental_Jurel$n.embarcaciones_incidental_Jurel <- data.frame(n.embarcaciones_incidental_Jurel, row.names = 1)
    out.porcenta$Incidental_Jurel$e.muestreadas_incidental_Jurel   <- data.frame(e.muestreadas_incidental_Jurel, row.names = 1) 
      
    out.porcenta$Incidental_Caballa$desembarque_incidental_Caballa     <- data.frame(desembarque_incidental_Caballa, row.names = 1)
    out.porcenta$Incidental_Caballa$n.embarcaciones_incidental_Caballa <- data.frame(n.embarcaciones_incidental_Caballa, row.names = 1) 
    out.porcenta$Incidental_Caballa$e.muestreadas_incidental_Caballa   <- data.frame(e.muestreadas_incidental_Caballa, row.names = 1)
    
    out.porcenta$OtrosSp  <- OtrosSp
    out.porcenta$warnings <- Xwarnings
    
    out.porcenta          <- suppressWarnings(out.porcenta)
    
    return(out.porcenta)
}


downloadDailyFishing <- function (directory = NULL, 
                                  startDate, 
                                  endDate, 
                                  specie,
                                  saveFile = TRUE) {
  
    if (is.null(directory) || !dir.exists(directory)) {
        directory = tempdir()
        dir.create(path = directory, showWarnings = FALSE)
    }
    
    dailyFishing <- switch(specie,
                           anchoveta = getDailyFishing_Anchoveta(directory = directory,
                                                                 urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal", 
                                                                 startDate = startDate, 
                                                                 endDate = endDate),
                           jurel = getDailyFishing_Jurel(directory = directory,
                                                         urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_repo_jure_",
                                                         startDate = startDate,
                                                         endDate = endDate),
                           caballa = getDailyFishing_Caballa(directory = directory, 
                                                             urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_repo_jure_", 
                                                             startDate = startDate, 
                                                             endDate = endDate))
  
    unlink(x = list.files(path = directory, pattern = ".xlsx", 
                          full.names = TRUE), force = TRUE, recursive = TRUE)
    
    if (isTRUE(saveFile)) {
        write.csv(x = dailyFishing, 
                  file = paste0("dailyFishing", "_", specie, "_", gsub("-", "", startDate), "_", gsub("-", "", endDate), ".csv"), 
                  row.names = FALSE)
    }
    return(dailyFishing)
}


getDailyFishing_Anchoveta <- function(directory,
                                      urlFishingMonitoring, 
                                      startDate, 
                                      endDate){
  
  DownloadPorcenta(directorio = directory, 
                   dirUrl = urlFishingMonitoring, 
                   inicio = startDate, 
                   fin = endDate)
  
  outputData <- ReadPorcenta(directorio = directory, 
                             inicio = startDate, 
                             fin = endDate)
  
    dataLanding           <- outputData$desembarque
    dataEffort            <- outputData$n.embarcaciones
    dataLanding$puerto    <- as.character(dataLanding$puerto)
    dataLanding           <- rbind(colnames(dataLanding), dataLanding)
    colnames(dataLanding) <- ""
    
    vectorDates <- seq(from = as.Date(startDate, format = "%Y-%m-%d"), 
                       to = as.Date(endDate, format = "%Y-%m-%d"), 
                       by = "day")
    
    years  <- as.numeric(format(vectorDates, "%Y"))
    months <- as.numeric(format(vectorDates, "%m"))
    days   <- as.numeric(format(vectorDates, "%d"))
    vectorPorts  <- (dim(dataLanding)[2] - 1)
    dailyFishing <- data.frame(anho = rep(years, each = vectorPorts), 
                               mes = rep(months, each = vectorPorts), 
                               dia = rep(days, each = vectorPorts), 
                               especie = rep("anchoveta", vectorPorts), 
                               tipo_flota = rep(c("industrial", "industrial_madera"), times = vectorPorts/2 * length(vectorDates)), 
                               puerto = rep(as.character(dataLanding[1, 2:(dim(dataLanding)[2])]), times = length(vectorDates)), 
                               captura = as.numeric(as.vector(t(dataLanding[c(3:dim(dataLanding)[1]), c(2:dim(dataLanding)[2])]))), 
                               embarcaciones = as.numeric(as.vector(t(dataEffort[c(2:dim(dataEffort)[1]), c(2:dim(dataEffort)[2])]))))
  
    return(dailyFishing)
}


getDailyFishing_Jurel <- function(directory,
                                  urlFishingMonitoring,
                                  startDate, 
                                  endDate){
  
   DownloadPorcentaJC_Industrial(directorio = directory, 
                                 dirUrl = urlFishingMonitoring, 
                                 inicio = startDate, 
                                 fin = endDate)
    
    outputData <- ReadPorcentaJC_Industrial(directorio = directory, 
                                            inicio = startDate, 
                                            fin = endDate)
    
    dataLanding           <- outputData$Jurel$desembarque_Jurel
    dataEffort            <- outputData$Jurel$n.embarcaciones_Jurel
    dataLanding           <- rbind(colnames(dataLanding), dataLanding)
    colnames(dataLanding) <- ""
    
    vectorDates <- seq(from = as.Date(startDate, format = "%Y-%m-%d"), 
                       to = as.Date(endDate, format = "%Y-%m-%d"), 
                       by = "day")
    years  <- as.numeric(format(vectorDates, "%Y"))
    months <- as.numeric(format(vectorDates, "%m"))
    days   <- as.numeric(format(vectorDates, "%d"))
    vectorPorts  <- dim(dataLanding)[2]
    dailyFishing <- data.frame(anho = rep(years, each = vectorPorts), 
                               mes = rep(months, each = vectorPorts), 
                               dia = rep(days, each = vectorPorts), 
                               especie = rep("jurel", vectorPorts), 
                               tipo_flota = rep("industrial", 
                                                times = vectorPorts * length(vectorDates)), 
                               puerto = rep(as.character(dataLanding[1, 1:(dim(dataLanding)[2])]), 
                                            times = length(vectorDates)), 
                               captura = as.numeric(as.vector(t(dataLanding[c(2:dim(dataLanding)[1]), 
                                                                            c(1:dim(dataLanding)[2])]))), 
                               embarcaciones = as.numeric(as.vector(t(dataEffort[c(1:dim(dataEffort)[1]), 
                                                                                 c(1:dim(dataEffort)[2])]))))
    
  return(dailyFishing)
}



getDailyFishing_Caballa <- function(directory,
                                    urlFishingMonitoring,
                                    startDate, 
                                    endDate){
  
   DownloadPorcentaJC_Industrial(directorio = directory, 
                                 dirUrl = urlFishingMonitoring, 
                                 inicio = startDate, 
                                 fin = endDate)
    
    outputData <- ReadPorcentaJC_Industrial(directorio = directory, 
                                            inicio = startDate, 
                                            fin = endDate)
    
    dataLanding           <- outputData$Caballa$desembarque_Caballa
    dataEffort            <- outputData$Caballa$n.embarcaciones_Caballa
    dataLanding           <- rbind(colnames(dataLanding), dataLanding)
    colnames(dataLanding) <- ""
    
    vectorDates <- seq(from = as.Date(startDate, format = "%Y-%m-%d"), 
                      to = as.Date(endDate, format = "%Y-%m-%d"), 
                      by = "day")
    years  <- as.numeric(format(vectorDates, "%Y"))
    months <- as.numeric(format(vectorDates, "%m"))
    days   <- as.numeric(format(vectorDates, "%d"))
    vectorPorts  <- dim(dataLanding)[2]
    dailyFishing <- data.frame(anho = rep(years, each = vectorPorts), 
                               mes = rep(months, each = vectorPorts), 
                               dia = rep(days, each = vectorPorts), 
                               especie = rep("caballa", vectorPorts), 
                               tipo_flota = rep("industrial", 
                                                times = vectorPorts * length(vectorDates)), 
                               puerto = rep(as.character(dataLanding[1, 1:(dim(dataLanding)[2])]), 
                                            times = length(vectorDates)), 
                               captura = as.numeric(as.vector(t(dataLanding[c(2:dim(dataLanding)[1]), 
                                                                            c(1:dim(dataLanding)[2])]))), 
                               embarcaciones = as.numeric(as.vector(t(dataEffort[c(1:dim(dataEffort)[1]), 
                                                                                 c(1:dim(dataEffort)[2])]))))
    
  return(dailyFishing)
}