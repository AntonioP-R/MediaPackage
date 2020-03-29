#Funzione per calcolare la media geometrica 

MediaGeometrica<-function(valori,frequenze = FALSE){
  if(is.numeric(valori)&is.numeric(frequenze)){
    if(length(valori)==length(frequenze)){
      mediaGeometrica <- 0
      for(k in 1:length(valori)){
        mediaGeometrica <- ((frequenze[k] * log(valori[k],exp(1)))/sum(frequenze)) + mediaGeometrica
      }
      print(signif(exp(mediaGeometrica),4))
    }else{
      print("Errore! I parametri della funzione 'MediaGeometrica' devono avere la stessa lunghezza...")
    }
  }else if(is.numeric(valori)&isFALSE(frequenze)){
      media <- prod(valori)^(1/length(valori))
      print(signif(media,4))
  }else{
    print("Errore! I parametri della funzione 'MediaGeometrica' devono essere di tipo 'numeric'...")
  }
}

    

    



