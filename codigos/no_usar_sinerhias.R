muestra_clues_con_sinerhias=clues_en_operacion |> dplyr::select(CLUES,NOMBRE.DE.LA.UNIDAD) |> dplyr::collect() |> 
  dplyr::sample_n(size = 10)

"inputs/Catalogo_SINERHIAS_2025 F.xlsx" |> openxlsx::read.xlsx(sheet = 2)->sinerhias

sinerhias$X3[sinerhias$X3!='TITULO' &sinerhias$X3!='NombreVar' & !is.na(sinerhias$X3)]


muestra_clues_con_sinerhias[,sinerhias$X3[sinerhias$X3!='TITULO' &sinerhias$X3!='NombreVar' & !is.na(sinerhias$X3)][1:5]
]=NA

muestra_clues_con_sinerhias$areacoextbas=sample(c(TRUE,FALSE,NA),size = 10,replace = T)
muestra_clues_con_sinerhias$consgeneral=sample(1:10,size = 10,replace = T)
muestra_clues_con_sinerhias$consmedfam=sample(1:10,size = 10,replace = T)
muestra_clues_con_sinerhias$medipreven=sample(c(TRUE,FALSE,NA),size = 10,replace = T)
muestra_clues_con_sinerhias$salasesp2_1_2=sample(1:10,size = 10,replace = T)



muestra_clues_con_sinerhias|> write.csv("inputs/muestra_clues_con_sinerhias.csv",row.names = F)
