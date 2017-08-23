ConsumeResource <- function(world, resource){
  # check if there are any lots in market, else do nothing
  if(nrow(world$market) == 0){return(world)}
  
  # relevant offers
  relevant.offers <- world$market[grepl(resource, world$market$resource),]
  
  # check if resource available in market, else do nothing
  if(nrow(relevant.offers) == 0){
    world$resource.information$demand[which(LETTERS == resource)] <- world$resource.information$demand[which(LETTERS == resource)] + 1
    return(world)
  }
  
  # order by price and find seller
  seller <- relevant.offers[order(relevant.offers$price)[1],]
  # output
  message(resource, "@",round(seller$price, 2)," consumed seller was",seller$seller.id)
  # deduct money from population account
  world$population$money <- world$population$money - seller$price
  # add money to seller
  world$companies$money[seller$seller.id] <- world$companies$money[seller$seller.id] + seller$price
  # grow population
  world$population$size <- world$population$size + runif(1)
  # remove lot from market and decrease demand
  original.lot <- which(world$market$lot.id == seller$lot.id)
  world$market <- world$market[-original.lot, ]
  world$resource.information$demand[which(LETTERS == resource)] <- world$resource.information$demand[which(LETTERS == resource)] - 1
  return(world)
}
ConsumeAllResources <- function(world){
  # note employment
  if(world$population$mean.employed == 0){
    world$population$mean.employed <- world$population$employed / world$population$size
  } else {
    world$population$mean.employed <- 1/2 * (world$population$mean.employed + world$population$employed / world$population$size) 
  }
  # workers go home
  world$population$employed <- 0
  # each person wants 1/10 of a resource
  consumable <- world$resource.information$name[which(world$resource.information$type == 0)]
  to.consume <- sample(consumable, 0.1 * world$population$size, T)
  for(r in to.consume){
    world <- ConsumeResource(world, r)
  }
  return(world)
}