cve_test <- unique(resultado$CVEGEO)[1:20]

bm_results <- microbenchmark(
  bucle_for = {
    conteo_clues_por_ageb_test <- conteo_clues_por_ageb[0, ] 
    
    for(cve_unica in cve_test){
      counter=counter+1
      if(counter%%100==0) print(cve_unica)
      #Tiempo de ejecución esperado: 3 horas
      #cve_unica=unique(resultado$CVEGEO)[63]#'130460178'
      puntos_de_ageb=resultado |> dplyr::filter(CVEGEO==cve_unica) 
      matriz_de_costos=gdistance::costDistance(T.GC,fromCoords = 
                                                 matrix(unlist(puntos_de_ageb$geometry ),nrow = nrow(puntos_de_ageb),ncol = 2,byrow = T)
                                               ,toCoords = 
                                                 matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T)
      )##Lamentablemente nuestro modelo de accesibilidad regresa Infinitos en las zonas donde la fricción es máxima (pendiente cercana a 90), por ejemplo montañas
      particion_N1_costos=matriz_de_costos[,1:(num_clues_por_tipo$conteo[1])] |> estandarizar_matriz_costos()
      particion_N2_costos=matriz_de_costos[,(1+(num_clues_por_tipo$conteo[1])):(num_clues_por_tipo$conteo[1]+(num_clues_por_tipo$conteo[2]))] |> estandarizar_matriz_costos()
      particion_N3_costos=matriz_de_costos[,(num_clues_por_tipo$conteo[1]+(num_clues_por_tipo$conteo[2])+1):nrow(clues_solicitados)] |> estandarizar_matriz_costos()
      
      num_clues_N1=c(10,20,40,60,NA) |> lapply(\(x){
        ifelse(x |> is.na(),{
          means_tiempo_clues=colMeans(particion_N1_costos,na.rm = T)
          return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
          ,rowSums(particion_N1_costos<x) |> mean(na.rm=T) |> round(0))
        
      }) |> unlist()
      num_clues_N2=c(10,20,40,60,NA) |> lapply(\(x){
        ifelse(x |> is.na(),{
          means_tiempo_clues=colMeans(particion_N2_costos,na.rm = T)
          return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
          ,rowSums(particion_N2_costos<x) |> mean(na.rm=T) |> round(0))
        
      }) |> unlist()
      num_clues_N3=c(10,20,40,60,NA) |> lapply(\(x){
        ifelse(x |> is.na(),{
          means_tiempo_clues=colMeans(particion_N3_costos,na.rm = T)
          return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
          ,rowSums(particion_N3_costos<x) |> mean(na.rm=T) |> round(0))
        
      }) |> unlist()
      
      conteo_clues_por_ageb[nrow(conteo_clues_por_ageb)+1,]=c(
        cve_unica,num_clues_N1,
        num_clues_N2,
        num_clues_N3
      )
    }
  },
  times = 2 # Solo 2 veces porque es muy tardado
)

print(bm_results)