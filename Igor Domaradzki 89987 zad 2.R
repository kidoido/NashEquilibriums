# Igor Domaradzki 89987

# Zadanie 2
getAllPureStrategyNE <- function(game){
  
  wymiary <- dim(game[[1]])
  lista_wym <- list()
  for (wymiar in wymiary){
    wek <- list(1:wymiar)
    lista_wym <- c(lista_wym, wek)
  }
  strategie <- do.call(expand.grid, lista_wym)
  names(strategie) <- names(game)
  
  for (nr_gracza in 1:length(game)){
    strategie_nasze <- list()
    macierz_z_wyplatami <- cbind(strategie, game[[nr_gracza]][as.matrix(strategie)])
    strategie_przeciwnikow <- unique(strategie[,-nr_gracza])
    macierz_z_wyplatami_bez_gracza <- macierz_z_wyplatami[,-nr_gracza]
    
    if (length(game) > 2){
      for (unique in 1:nrow(strategie_przeciwnikow)){
        warunek <- strategie_przeciwnikow[unique,]
        wszystkie_str <- c()
        for (ii in 1:nrow(macierz_z_wyplatami_bez_gracza)){
          analizowany_row <- macierz_z_wyplatami_bez_gracza[ii,]
          for(ij in 1:length(warunek)){
            if(as.numeric(analizowany_row[,ij]) == as.numeric(warunek[ij])){
              czy_str <- TRUE
            } else {
              czy_str <- FALSE
              break
            }
          }
          if (czy_str == TRUE){
            wszystkie_str <- rbind(wszystkie_str,macierz_z_wyplatami[ii,])
            
          }
        }
        
        najlepsza_str <- wszystkie_str[wszystkie_str[, length(game)+1] == max(wszystkie_str[, length(game)+1]), ]
        najlepsza_str <- najlepsza_str[,c(1:length(game))]
      
        if (nrow(najlepsza_str) != 1) {
          for (i in 1:nrow(najlepsza_str)) {
            strategie_nasze <- c(strategie_nasze, list(najlepsza_str[i,]))
          }
        } else {
          strategie_nasze <- c(strategie_nasze, list(najlepsza_str))
        }
      }
    } else {
      for (unique in strategie_przeciwnikow){
        wszystkie_str <- macierz_z_wyplatami[macierz_z_wyplatami[,-nr_gracza][, -ncol(macierz_z_wyplatami[,-nr_gracza])] == unique,]
        najlepsza_str <- wszystkie_str[wszystkie_str[, length(game)+1] == max(wszystkie_str[, length(game)+1]), ]
        najlepsza_str <- najlepsza_str[,c(1:length(game))]
        if (nrow(najlepsza_str) != 1) {
          for (i in 1:length(najlepsza_str)) {
            strategie_nasze <- c(strategie_nasze, list(najlepsza_str[i,]))
          }
        } else {
          strategie_nasze <- c(strategie_nasze, list(najlepsza_str))
        }
      }
    }
    
    if(nr_gracza == 1) {
      rn <- strategie_nasze
    } else {
      rn <- intersect(rn, strategie_nasze)
    }
    
  }
  if (length(rn) == 0){
    print("Gra nie ma równowagi Nasha w strategiah czystych")
  } else {
    for (el_listy in rn){
      names(el_listy) <- NULL
    }
    print(rn, row.names=F, col.names = F)
  }
  
}


# Przykłady
### Przykład 1

game <- list(
  "player 1" = array(c(1, 0, 0, 1), dim = c(2, 2)),
  "player 2" = array(c(1, 0, 0, 1), dim = c(2, 2))
)

getAllPureStrategyNE(game)

### Przykład 2

game <- list(
  "player1" = array(c(1, 0, 0, 0, 0, 1, 0, 1), dim = c(2, 2, 2)),
  "player2" = array(c(1, 0, 0, 0, 0, 1, 0, 1), dim = c(2, 2, 2)),
  "player3" = array(c(1, 0, 0, 0, 0, 1, 0, 1), dim = c(2, 2, 2))
)

getAllPureStrategyNE(game)

### Przykład 3 (dylemat więźnia)

game <- list(
  "player1" = array(c(5, 10, 1, 2), dim = c(2, 2)),
  "player2" = array(c(5, 1, 10, 2), dim = c(2, 2))
)

getAllPureStrategyNE(game)
