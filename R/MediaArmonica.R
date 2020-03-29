#Funzione per il calcolo della media armonica 

MediaArmonica<-function(valori, frequenze = FALSE){
  if(is.numeric(valori)&is.numeric(frequenze)){
    if(length(valori)==length(frequenze)){
      mediaarmonica <-0
      for(k in 1:length(valori)){
        mediaarmonica <- (frequenze[k]/valori[k]) + mediaarmonica
      }
      print(signif(sum(frequenze)/mediaarmonica,4))
    }else{
      print("Errore! I parametri della funzione 'MediaArmonica' devono avere la stessa lunghezza...")
    }
  }else if(is.numeric(valori)&isFALSE(frequenze)){
      SommaReciproci <- 0
      for(k in 1:length(valori)){
        SommaReciproci <- (1/valori[k]) + SommaReciproci
      }
      mediaarmonica <- length(valori)/SommaReciproci
      print(signif(mediaarmonica,4))
  }else{
   print("Errore! I parametri della funzione 'MediaArmonica' devono essere di tipo 'numeric'...")
 }
}


