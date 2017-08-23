RunWorld <- function(world, days){
  # companies are active. They produce, make, and place buy/sell orders.
  for(i in 1:nrow(world$companies)){world <- RunCompany(world, i)}
  # price for each resource is calculated
  world <- DeteterminePrices(world)
  # consumers are active. They place orders for sink-goods.
  # government is active. Interests are paid. Companies are bankrupted. New companies are created.
  # clean-up phase. Resources are moved up the queue. 
  world <- MoveResourceQueues(world)
}