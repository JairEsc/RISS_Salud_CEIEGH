###
#Datos generales: 
#Número de personas / porcentaje con al menos un clues n1 a menos de 10 minutos
#Número de personas / porcentaje con al menos un clues n2 a menos de 10 minutos
#Número de personas / porcentaje con al menos un clues n1 a menos de 60 minutos
#Número de personas / porcentaje con al menos un clues n2 a menos de 60 minutos

#Localidades donde no se cumpla, visualizables y filtrables/ descargables
#Municipios donde no se cumpla, visualizables y filtrables/ descargables

#Por municipio, tiempo promedio de accesibilidad a clues n1 y n2. Escalado por poblacion

#Por localidades, tiempo promedio de accesibilidad a clues n1 y n2. Escalado por poblacion. Filtrable y descargable

#Por clues, promedio de personas a menos de 10 y 60 minutos, filtrable y descargable

#agebs con al menos k clues a menos de 10 minutos
demograficos_scince |> dplyr::filter(as.numeric(CLUES_N1_10)<1) |> nrow()
#agebs con al menos k clues a menos de 60 minutos
demograficos_scince |> dplyr::filter(as.numeric(CLUES_N1_60)<1) |> nrow()
#agebs con al menos 1 clues N2 a menos de 10 minutos
demograficos_scince |> dplyr::filter(as.numeric(tiempo_promedio_CLUES_N2)>10) |> nrow()
#agebs con al menos 1 clues N2 a menos de 10 minutos
demograficos_scince |> dplyr::filter(as.numeric(tiempo_promedio_CLUES_N2)>60) |> nrow()

