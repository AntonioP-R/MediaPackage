#Funzione per il calcolo della media aritmetica 

MediaAritmetica<-function(valori,frequenze = FALSE){
  if(is.numeric(valori) & is.numeric(frequenze)){
    if(length(valori)==length(frequenze)){
      media <- 0
      for(k in 1:length(valori)){
        media = valori[k]*frequenze[k] + media
      }
      print(signif(media/sum(frequenze),4))
    }else{
      print("Errore! I parametri della funzione 'MediaAritmetica' devono avere la stessa lunghezza...")
    }
  }else if(is.numeric(valori)&isFALSE(frequenze)){
    print(signif(sum(valori)/length(valori),4))
  }else{
   print("Errore! I parametri della funzione 'MediaAritmetica' devono essere di tipo 'numeric'...")
  }
}




