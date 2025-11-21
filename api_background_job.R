library(pins)
library(vetiver)
library(plumber)

model_board <- board_folder(path = 'data/final/', versioned = TRUE)

v <- model_board |> 
  vetiver_pin_read(name = 'final_model_large')

pr() |> 
  vetiver_api(v) |> 
  pr_run(port = 8088)
