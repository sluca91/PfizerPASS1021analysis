# Input string
input_string <- "The value is 0.00 ."

# Regular expression pattern
pattern <- "\\b0\\.00\\b"

# Replacement string
replacement <- "< 0.01"

# Replace pattern with replacement
output_string <- gsub(pattern, replacement, input_string)

# Print the output
print(output_string)
print(input_string)



value <- " < 0.01"
if (grepl("^<\\s0\\.01$", value)) {
  print("Pattern matched")
} else {
  print("Pattern not matched")
}


value <- "    < 0.01"
if (grepl("^\\s*<\\s*0\\.01$", value)) {
  print("Pattern matched")
} else {
  print("Pattern not matched")
}
