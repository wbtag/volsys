#' @export
rozdel_kvotou <- function(str, V, S, kvota="hare", zbytky="zbytky") {
  if (kvota == "hare") {
    odp <- 0
  } else if (kvota == "hagenbach-bischoff") {
    S <- S+1
    odp <- 1
  } else if (kvota == "imperiali") {
    S <- S+2
    odp <- 2
  } else if (kvota == "imperiali2") {
    S <- S+3
    odp <- 3
  } else {
    stop("Neznámá kvóta")
  }
  Q <- sum(V)/S
  kresla <- V/Q
  names(kresla) <- str
  zbyva <- (sum(kresla)-odp)-sum(floor(kresla))
  if (zbyva > 0) {
    if (zbytky == "zbytky") {
      zbytky <- (kresla-floor(kresla))*Q
      kresla <- floor(kresla)
      while (zbyva > 0) {
        kresla[which.max(zbytky)] <- kresla[which.max(zbytky)] + 1
        zbytky[which.max(zbytky)] <- 0
        zbyva <- zbyva-1
      }
    } else if (zbytky == "prumery") {
      kresla <- floor(kresla)
      prum <- V/(kresla+1)
      while (zbyva > 0) {
        kresla[which.max(prum)] <- kresla[which.max(prum)] + 1
        prum[which.max(prum)] <- 0
        zbyva <- zbyva-1
      }
    } else if (zbytky == "presun") {
      kresla <- floor(kresla)
      print(paste(zbyva,ifelse(zbyva == 1, "mandát", ifelse(zbyva > 1 & zbyva < 5, "mandáty", "mandátů")),
                  "se rozdělí v následujícím skrutiniu.",sep = " "))
    }
    print(kresla)
  } else {
    print(floor(kresla))
  }
}