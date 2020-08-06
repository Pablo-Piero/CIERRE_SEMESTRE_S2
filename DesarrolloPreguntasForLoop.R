listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))

L1<-listaDocumentos
medidasProteccion = c("mp")

for(i in 1:length(medidasProteccion)) {
  index = sapply(L1, function(x1)match(medidasProteccion[i],x1,nomatch = 0) > 0)
  a = which(index)
  k = 0 ; l = 0 ; g = 0
  for(j in a){
    cantidadPorMp=length(L1[[j]])-1
    if(cantidadPorMp==1){k=k+1}
    else if(cantidadPorMp==2){l=l+1} 
    else{g=g+1}
  }
  print(paste("hay",k,"causas con 1 niño"))
  print(paste("hay",l,"causas con 2 niño"))
  print(paste("hay",g,"causas con 3 niño"))
}

#2
# Los oficios están compuestos por el código al cual pertenecen, 
# construya una función que almacene los códigos y las temáticas 
# a las que están asociadas.

lista<-c("av01","av02","av03","av04","av07","av08")
for(k in 1:length(lista)){
  d<-c(" ")
  for(i in 1:length(lista[k])){
    b<-lista[k]
    index = sapply(L1, function(x1)match(lista[k][i],x1,nomatch = 0) > 0)
    a = which(index)
    for(j in 1:length(a)){
      d<-cat(" ",L1[[a[j]]][[3]]," ") }
    cat(lista[k],d,"\n")
  }}

# 3
#Construya una función que actúe de juez y retorne aprobada o reprobada 
#para los diferentes oficios, y entregue la cantidad que hay de cada una.
# aprobado si tiene 3 cositos, reprobado si no

lista<-c("av01","av02","av03","av04","av07","av08")
aprobados=0
for(k in 1:length(lista)){
  d<-c(" ")
  for(i in 1:length(lista[k])){
    b<-lista[k]
    index = sapply(L1, function(x1)match(lista[k][i],x1,nomatch = 0) > 0)
    a = which(index)
    if (length(a)>2){aprobados=aprobados+1}
  }
} 
print(paste("Llegaron", length(lista) ,"oficios de los cuales:", aprobados ,"son aprobados y ",length(lista)-aprobados ,"reprobados"))

