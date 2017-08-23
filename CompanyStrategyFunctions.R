RunCompany <- function(world, company.id){
  # store my resource
  my.r <- world$companies$type[company.id]
  my.r.num <- which(LETTERS == my.r)
  my.inputs <- GetInputs(world, my.r)
  
  # step 1: calculate the expected profits for these actions:
  #    a) gather a resource in degree^2 steps and then have it sold
  #    b) buy an adjacent resource to make my resource, then sell it
  e.profits <- ExpectedProfits(world, my.r.num)
  
  # step 2: gather if that had the highest profit. Else: buy an input and convert
  if(e.profits$profit.produce > e.profits$profit.make & world$resource.information$type[my.r.num] > 0){
    world <- ProduceResource(world, company.id)
  } else if(e.profits$profit.make > 0) {
    cheapest.input <- my.inputs[which.min(GetPrices(world, my.inputs))]
    world <- BuyResource(world, company.id, cheapest.input)
    world$companies$last.action[company.id] <- paste0("b",cheapest.input)
    if(grepl(cheapest.input, world$companies$inventory[company.id])){
      browser()
      # message(company.id," makes ",my.r)
      world$companies$inventory[company.id] <- my.r
      world$companies$last.action[company.id] <- "m"
    }
  } else {
    
  }
  
  # step 3: always sell a resource if it is in our inventory
  if(world$companies$inventory[company.id] != ""){
    world <- SellResource(world, company.id, world$companies$type[company.id])
    world$companies$inventory[company.id] <- ""
  }
  
  # if we end up with negative money, increase loan
  if(world$companies$money[company.id] < 0){
    world <- IncreaseLoan(world, company.id, abs(world$companies$money[company.id]))
  }
  
  return(world)
}
ExpectedProfits <- function(world, r.num){
  profits <- data.frame(
    name=LETTERS[r.num],
    cost.to.produce=degree(world$resource.graph)[r.num],
    cost.of.inputs=0,
    price=GetPrices(world, LETTERS[r.num]),
    profit.produce=0,
    profit.make=0
  )
  for(i in 1:length(r.num)){
    inputs <- GetInputs(world, LETTERS[i])
    if(length(inputs) > 0){
      profits$cost.of.inputs[i] <- GetPrices(world, inputs, T, T)
    } else {
      profits$cost.of.inputs[i] <- NA
    }
    
  }
  profits$profit.produce <- profits$price - profits$cost.to.produce
  profits$profit.make <- profits$price - profits$cost.of.inputs
  return(profits)
}