\name{MediaQuadratica}
\alias{MediaQuadratica}
\encoding{UTF-8}
\title{Media Quadratica}

\description{Data una successione di valori, o una distribuzione di frequenza, la funzione calcola la media quadratica.}

\usage{MediaQuadratica(valori, frequenze = FALSE)}

\arguments{
  \item{valori}{Vettore di tipo "numeric". Rappresenta il vettore di valori per i quali vogliamo calcolare la media quadratica.}
  
  \item{frequenze}{Vettore di tipo "numeric". Rappresenta le frequenze, ossia i "pesi" dei valori descritti nel parametro precedente. È un parametro opzionale perché possiamo avere anche una successione di valori senza le relative frequenze.}
}

\details{La media quadratica si ottiene estraendo la radice quadrata della media aritmetica dei quadrati dei valori.}

\value{Il valore di ritorno è di tipo 'numeric' e rappresenta la media quadratica.}

\section{Warning}{La funzione può restituire un errore in due occasioni, la prima, quando i parametri (valori e frequenze) hanno dimensioni diverse, e la seconda, invece, quando i parametri NON sono di tipo "numeric".}

\author{AntonioP-R}

\note{Per qualsiasi informazione aggiuntiva, o malfunzionamento della libreria, si prega di contattare l'autore tramite email al seguente indirizzo: \email{antoniop.r0033@gmail.com}}

\examples{
#Senza "pesi"
x <-c(1,2,3)
media<-MediaQuadratica(x,frequenze = FALSE)
#Con "pesi"
f<-c(2,3,7)
media<-MediaQuadratica(x,f)
}
