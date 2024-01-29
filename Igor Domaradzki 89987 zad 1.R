# Igor Domaradzki 89987

# Zadanie 1

pathQ <- function(map, startPoint, endRegion) {
  if (map[startPoint[[1]], startPoint[[2]]] == FALSE) {
    return("Punkt startowy znajduje się na ściance labiryntu. Wybierz inny punkt.")
  }
  
  l_rows <- nrow(map)
  l_cols <- ncol(map)
  
  odwiedzone <- matrix(FALSE, l_rows, l_cols)
  odwiedzone[startPoint[[1]], startPoint[[2]]] <- TRUE
  
  currentPoint <- startPoint
  kierunki <- c(-1, 0, 1, 0, -1)
  do_sprawdzenia <- currentPoint
  kolejka <- list(currentPoint)
  while (length(kolejka) > 0) {
    currentPoint <- kolejka[[1]]
    for (i in 1:4){
      do_sprawdzenia[[1]] <- currentPoint[[1]] + kierunki[i]
      do_sprawdzenia[[2]] <- currentPoint[[2]] + kierunki[i+1]
      
      if(do_sprawdzenia[[1]] > 0 & do_sprawdzenia[[1]] <= l_rows && do_sprawdzenia[[2]] > 0 &&
         do_sprawdzenia[[2]] <= l_cols && odwiedzone[do_sprawdzenia[[1]], do_sprawdzenia[[2]]] == FALSE &&
         map[do_sprawdzenia[[1]], do_sprawdzenia[[2]]] == TRUE) {
        odwiedzone[do_sprawdzenia[[1]], do_sprawdzenia[[2]]] <- TRUE
        if (as.numeric(do_sprawdzenia[[1]]) %in% as.numeric(unlist(endRegion[1])) &&
            as.numeric(do_sprawdzenia[[2]] %in% as.numeric(unlist(endRegion[2])))) {
          return(TRUE)
        }
        kolejka <- append(kolejka, list(do_sprawdzenia))
        
      }
    }
    kolejka <-kolejka[-1]
    
  }
  return(FALSE)
}

# Przyklady
pathQ(matrix(c(T,T,T,T,T,
               T,F,T,T,T,
               T,F,T,F,F,
               T,F,T,T,T,
               T,F,T,T,T), nrow = 5, byrow = T ), startPoint = list(x=1, y=1), endRegion = list(x=4:5, y=4:5))

pathQ(matrix(c(T,T,T,F,T,
               T,F,T,F,T,
               T,F,T,F,F,
               T,F,T,F,T,
               T,F,T,T,T), nrow = 5, byrow = T ), startPoint = list(x=1, y=5), endRegion = list(x=4:5, y=4:5))

pathQ(matrix(T, 100, 100), startPoint = list(x=1, y=5), endRegion = list(x=100, y=100))


pathQ(matrix(c(T,T,T,T,T,
               T,F,F,F,T,
               T,F,T,F,T,
               T,F,F,F,T,
               T,T,T,T,T), nrow = 5, byrow = T ), list(x=1, y=1), list(x=3, y=3))
