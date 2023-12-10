# ===== DAY 1 =====
# https://adventofcode.com/2023/day/1

# retrieve first and last digit in each line
# form a two-digit number ("calibration value")
# sum up all calibration values

library(here)
library(stringr)
library(dplyr)
library(english)

# ===== functions =====

digit_list = 1:9 # digits corresponding to english words that we need to search for (from puzzle statement)

english_to_digit = function(word, digits = digit_list){
  # let 'words' that are already digits pass through (no conversion)
  if(!is.na(suppressWarnings(as.numeric(word)))) return(word)

  # check whether word will have a match in digit list
  word_list = as.english(digits)
  if(!(word %in% word_list)) stop(paste(word, "does not have match in digit list of", paste(digit_list, collapse = ",")))

  # perform conversion
  lookup = as.character(digit_list) # always return a string with the digit
  # (to match early return output above)
  names(lookup) = word_list
  lookup[[word]]
}

convert_digits = function(string_list){
  (string_list
   |> lapply(\(x) english_to_digit(x))
   |> unlist()
  )
}

get_digit = function(
    string_list,
    pattern,
    index # string with expression specifying index of matches vector, x,
    # to extract, for each element of the string_list
){
  (string_list
   |> str_extract_all(pattern = pattern)
   |> lapply(\(x) x[eval(parse(text = index))])
   |> unlist()
  )
}

get_first_digit = function(string_list, pattern){
  get_digit(string_list = string_list, pattern = pattern, index = "1")
}

get_last_digit = function(string_list, pattern){
  get_digit(string_list = string_list, pattern = pattern, index = "length(x)")
}

get_answer = function(doc, pattern = "\\d{1}"){
  first_digit = doc |> get_first_digit(pattern = pattern) |> convert_digits()
  last_digit = doc |> get_last_digit(pattern = pattern) |> convert_digits()

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

# ===== PART 2 =====

# set up new regular expression to also match digits 1:9 as english words
pattern_digits = (
  digit_list
  |> as.english() # converts digits to english words
  |> as.character()
  |> c("\\d")
  |> paste(collapse = "|")
)
pattern = paste0("(", pattern_digits, "){1}")

# ===== test with sample input =====
input = readLines(here("input", "day1-sample2.txt"))
input |> get_answer(pattern = pattern) |> print_answer()

# ===== real input =====
input = readLines(here("input", "day1.txt"))
input |> get_answer(pattern = pattern) |> print_answer()
