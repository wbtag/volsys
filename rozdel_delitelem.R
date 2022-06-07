dhondt <- function(str, V, S) {
  dis <- 0
  V0 <- V
  kresla <- numeric(length(str))
  names(kresla) <- str
  while (S > dis) {
    kresla[which.max(V)] <- kresla[which.max(V)] + 1
    V[which.max(V)] <- V0[which.max(V)]/(1+kresla[which.max(V)])
    dis <- dis + 1
  }
  return(kresla)
}

dansky <- function(str,V,S) {
  dis <- 0
  k <- numeric(length(str)) + 1
  del = 2
  V0 = V
  kresla <- numeric(length(str))
  names(kresla) <- str
  while (S > dis) {
    kresla[which.max(V)] <- kresla[which.max(V)] + 1
    k[which.max(V)] <- k[which.max(V)] + 2
    V[which.max(V)] <- V0[which.max(V)]/(del+k[which.max(V)]-2+kresla[which.max(V)])
    dis <- dis + 1
  }
  return(kresla)
}

sainte_lague <- function(str,V,S) {
  dis <- 0
  k <- numeric(length(str)) + 1
  del = 1
  V0 = V
  kresla <- numeric(length(str))
  names(kresla) <- str
  while (S > dis) {
    kresla[which.max(V)] <- kresla[which.max(V)] + 1
    k[which.max(V)] <- k[which.max(V)] + 1
    V[which.max(V)] <- V0[which.max(V)]/(del+k[which.max(V)]-1+kresla[which.max(V)])
    dis <- dis + 1
  }
  return(kresla)
}

sainte_lague2 <- function(str,V,S) {
  dis <- 0
  k <- numeric(length(str)) + 1
  del = 1
  V0 = V
  V = V/1.4
  kresla <- numeric(length(str))
  names(kresla) <- str
  while (S > dis) {
    kresla[which.max(V)] <- kresla[which.max(V)] + 1
    k[which.max(V)] <- k[which.max(V)] + 1
    V[which.max(V)] <- V0[which.max(V)]/(del+k[which.max(V)]-1+kresla[which.max(V)])
    dis <- dis + 1
  }
  return(kresla)
}

imperiali_d <- function(str,V,S) {
  dis <- 0
  del = 2
  V0 = V
  V = V/2
  kresla <- numeric(length(str))
  names(kresla) <- str
  while (S > dis) {
    kresla[which.max(V)] <- kresla[which.max(V)] + 1
    V[which.max(V)] <- V0[which.max(V)]/(del+kresla[which.max(V)])
    dis <- dis + 1
  }
  return(kresla)
}

#' @export
rozdel_delitelem <- function(str, V, S, delitel="dhondt", klauzule=0.05) {
  names(V) <- str
  Vs <- sum(V)
  V <- V[V > Vs*klauzule]
  str <- str[which(V>Vs*klauzule)]
  if (delitel == "dhondt") {
    dhondt(str,V,S)
  } else if (delitel == "imperiali") {
    imperiali_d(str,V,S)
  } else if (delitel == "sainte-lague") {
    sainte_lague(str,V,S)
  } else if (delitel == "sainte-lague2") {
    sainte_lague2(str,V,S)
  } else if (delitel == "dansky") {
    dansky(str,V,S)
  } else {
    stop("Neznámý dělitel/tento dělitel nelze použít")
  }
}

strany <- c("A","B","C","D","E")
hlasy <- c(485,290,140,100,75)


rozdel_delitelem(strany, hlasy, 6, delitel = "dhondt")
rozdel_delitelem(strany, hlasy, 6, delitel = "sainte-lague")
rozdel_delitelem(strany, hlasy, 6, delitel = "sainte-lague2")
rozdel_delitelem(strany, hlasy, 6, delitel = "dansky")
rozdel_delitelem(strany, hlasy, 6, delitel = "imperiali")