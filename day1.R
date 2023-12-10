# ===== DAY 1 =====
# https://adventofcode.com/2023/day/1

# retrieve first and last digit in each line
# form a two-digit number ("calibration value")
# sum up all calibration values

library(here)
library(stringr)
library(dplyr)

# ===== functions =====

get_first_digit = function(string_list){
  (string_list
   |> str_extract_all(pattern = "\\d{1}")
   |> lapply(\(x) x[1])
   |> unlist()
  )
}

reverse_string = function(string_list){
  (string_list
   |> lapply(
     \(x) (strsplit(x, split = "")
           |> unlist()
           |> rev()
           |> paste(collapse = ""))
     )
  )
}

get_answer = function(doc){
  first_digit = doc |> get_first_digit()
  last_digit = doc |> reverse_string() |> get_first_digit()

  (paste0(first_digit, last_digit) # concatenate digits
    |> as.numeric()
    |> sum()
  )
}

print_answer = function(ans){
  print(paste("Answer:", ans))
}

# ===== PART 1 =====

# ===== test with sample input =====

input = readLines(here("input", "day1-sample.txt"))
input |> get_answer() |> print_answer()

# ===== real input =====
input = readLines(here("input", "day1.txt"))
input |> get_answer() |> print_answer()
