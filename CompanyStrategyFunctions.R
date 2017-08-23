RunCompany <- function(world, company.id){
  # store my resource
  my.r <- world$companies$type[company.id]
  my.r.num <- which(LETTERS == my.r)
  
  # step 1: calculate the expected profits for these actions:
  #    a) gather a resource in degree^2 steps and then have it sold
  e.profit.gather <- GetPrices(world, my.r) - degree(world$resource.graph)[my.r.num]
  #    b) buy an adjacent resource to make my resource, then sell it
  e.profit.trade <- GetPrices(world, my.r) - GetPrices(world, GetInputs(world, my.r), T, T)
  
  # step 2: gather if that had the highest profit. Else: buy an input and convert
  if(e.profit.gather > e.profit.trade & world$resource.information$type[my.r.num] > 0){
    # if we do not have enough money, loan more if we expect to profit from it.
    if(world$companies$money[company.id] < degree(world$resource.graph)[my.r.num]){
      world <- IncreaseLoan(world, company.id, degree(world$resource.graph)[my.r.num])
    }
    world <- ProduceResource(world, company.id)
  } else if(e.profit.trade >= 0) {
    my.inputs <- GetInputs(world, my.r)
    cheapest.input <- my.inputs[which.min(GetPrices(world, my.inputs))]
    if(world$companies$money[company.id] < GetPrices(world, cheapest.input)){
      world <- IncreaseLoan(world, company.id, GetPrices(world, cheapest.input))
    }
    world <- BuyResource(world, company.id, cheapest.input)
    world$companies$last.action[company.id] <- paste0("b",cheapest.input)
    if(grepl(paste0(GetInputs(world, my.r),collapse = "|"), world$companies$inventory[company.id])){
      # message(company.id," makes ",my.r)
      world$companies$inventory[company.id] <- my.r
      world$companies$last.action[company.id] <- "m"
    }
  }
  
  # step 3: sell a resource if it is in our inventory
  if(world$companies$inventory[company.id] != ""){
    world <- SellResource(world, company.id, world$companies$type[company.id])
    world$companies$inventory[company.id] <- ""
  }
  
  #print(c(my.r,e.profit.gather, e.profit.trade, (e.profit.gather > e.profit.gather & world$resource.information$type[my.r.num] > 0), world$companies$last.action[company.id]))
  
  return(world)
}