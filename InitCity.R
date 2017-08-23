InitCity

size <- 100

city <- list(
  people = data.frame(age=sample(1:100,size,T),influence=runif(size),type=sample(LETTERS,size,T)),
  network = sample_smallworld(1, size, 2, 0.1)
)
