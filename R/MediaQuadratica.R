#Funzione per il calcolo della media quadratica

MediaQuadratica<-function(valori,frequenze = FALSE){
  if(is.numeric(valori) & is.numeric(frequenze)){
    if(length(valori)==length(frequenze)){
      SommaQuadratica <- 0
      for(k in 1:length(valori)){
        SommaQuadratica = (valori[k]^2)*frequenze[k] + SommaQuadratica
      }
      media <- sqrt(SommaQuadratica/sum(frequenze))
      print(signif(media,4))
    }else{
      print("Errore! I parametri della funzione 'MediaQuadratica' devono avere la stessa lunghezza...")
    }
  }else if(is.numeric(valori)&isFALSE(frequenze)){
    SommaQuadratica <- 0
    for(k in 1:length(valori)){
      SommaQuadratica <- valori[k]^2 + SommaQuadratica
    }
    media <- sqrt(SommaQuadratica/length(valori))
    print(signif(media,4))
  }else
    {
    print("Errore! I parametri della funzione 'MediaQuadratica devono essere di tipo 'numeric'...")
  }
}




