BuyResource <- function(world, buyer.id, resource){
  # check if there are any lots in market and if company has money, else do nothing
  if(nrow(world$market) == 0 | world$companies$money[buyer.id] > 0){
    world$resource.information$demand[which(LETTERS == resource)] <- world$resource.information$demand[which(LETTERS == resource)] + 1
    return(world)
  }
  
  # relevant offers
  relevant.offers <- world$market[grepl(resource, world$market$resource),]
  
  # check if resource available in market, else increase demand but do nothing else
  if(nrow(relevant.offers) == 0){
    world$resource.information$demand[which(LETTERS == resource)] <- world$resource.information$demand[which(LETTERS == resource)] + 1
    return(world)
  }
  
  # order by price and find seller
  seller <- relevant.offers[order(relevant.offers$price)[1],]
  # output
   #message(resource, "@",round(seller$price, 2)," buyer:",buyer.id, ",seller:",seller$seller.id)
  # deduct money from company account
  world$companies$money[buyer.id] <- world$companies$money[buyer.id] - (seller$price * 1 - world$government$transaction.tax)
  # add money to seller. seller.id==0 means government
  if(seller$seller.id == 0){
    world$government$state.funds <- world$government$state.funds + seller$price
  } else {
    world$companies$money[seller$seller.id] <- world$companies$money[seller$seller.id] + (seller$price * 1 - world$government$transaction.tax)
    world$government$state.funds <- world$government$state.funds + seller$price * world$government$transaction.tax
  }
  # add resource to buyer inventory
  world$companies$inventory[buyer.id] <- paste0(world$companies$inventory[buyer.id], seller$resource, collapse = "")
  message(paste0(world$companies$inventory[buyer.id], seller$resource, collapse = ""))
  # remove lot from market and remove demand
  original.lot <- which(world$market$lot.id == seller$lot.id)
  world$market <- world$market[-original.lot, ]
  world$resource.information$demand[which(LETTERS == resource)] <- world$resource.information$demand[which(LETTERS == resource)] - 1
  return(world)
}
SellResource <- function(world, seller.id, resource){
  resource <- strsplit(resource, "")[[1]]
  world$market <- rbind(
    world$market,
    data.frame(
      lot.id = max(world$market$lot.id, 0, na.rm = T) + 1,
      seller.id = seller.id,
      resource = resource,
      price = world$resource.information$price[match(resource, LETTERS)],
      age = 1,
      stringsAsFactors = F
    )
  )
  return(world)
}
DeterminePrices <- function(world){
  # determine supply and demand
  for(i in 1:26){world$resource.information$supply[i] <- sum(world$companies$type == LETTERS[i])}
  world$resource.information$demand <- round(world$resource.information$demand * .5)
  world$resource.information$demand[world$resource.information$demand<0] <- 0
  # determine price
  world$resource.information$price <- round((1 + world$resource.information$demand) / (1 + world$resource.information$supply), 2)
  world$resource.information$price[is.infinite(world$resource.information$price)] <- 0
  return(world)
}
GetPrices <- function(world, r, get.min = F, inf.if.unavailable = F){
  r.num <- match(r, LETTERS)
  price <- rep(NA, length(r))
  
  # on market
  for(i in 1:length(r)){
    price[i] <- mean(world$market$price[world$market$resource == r[i]], na.rm = T)
  }
  
  # if only marketable resources sought, return inf if none available
  if(inf.if.unavailable){
    price[is.nan(price)] <- 1e6
    return(price)
  }
  # in database
  for(i in which(is.nan(price))){
    price[i] <- world$resource.information$price[which(LETTERS == r[i])]
  }
  # fall back
  price[is.infinite(price) | is.na(price) | is.nan(price) | is.nan(price)] <- 0
  
  if(get.min){
    price <- min(price)
  }
  
  return(price)
}
GetInputs <- function(world, r){
  LETTERS[igraph::adjacent_vertices(world$resource.graph, which(LETTERS == r))[[1]]]
}
GetNumericInputs <- function(world, r){
  as.numeric(igraph::adjacent_vertices(world$resource.graph, which(LETTERS == r))[[1]])
}
IncreaseLotAges <- function(world){
  world$market$age <- world$market$age + 1
  return(world)
}