# source functions
source("MarketFunctions.R")
source("CompanyFunctions.R")
source("SimulationFunctions.R")
source("CompanyStrategyFunctions.R")
source("GovernmentFunctions.R")
source("PopulationFunctions.R")

n.iter <- 1200
price.history <- matrix(0, 26, n.iter)

# init world
source("InitWorld.R")
# run world
world <- CreateCompany(world, 26 * 4)
for(time in 1:n.iter){
  message("===== ",time," =====")
  world <- DeterminePrices(world)
  for(i in 1:nrow(world$companies)){world <- RunCompany(world, i)}
  world <- CollectInterest(world)
  world <- ConsumeAllResources(world)
  world <- MoveResourceQueues(world)
  world <- IncreaseLotAges(world)
  price.history[, time] <- world$resource.information$price
  par(mfcol=c(1,2))
  image(price.history)
  plot(world$companies$money - world$companies$loan.remaining.amount,col="white")
  text(x=1:nrow(world$companies), y=world$companies$money - world$companies$loan.remaining.amount, world$companies$type)
  par(mfcol=c(1,1))
}
