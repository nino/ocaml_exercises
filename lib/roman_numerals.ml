let rec to_roman number =
  if number >= 1000 then "M" ^ to_roman (number - 1000)
  else if number >= 900 then "CM" ^ to_roman (number - 900)
  else if number >= 500 then "D" ^ to_roman (number - 500)
  else if number >= 400 then "CD" ^ to_roman (number - 400)
  else if number >= 100 then "C" ^ to_roman (number - 100)
  else if number >= 90 then "XC" ^ to_roman (number - 90)
  else if number >= 50 then "L" ^ to_roman (number - 50)
  else if number >= 40 then "XL" ^ to_roman (number - 40)
  else if number >= 10 then "X" ^ to_roman (number - 10)
  else if number >= 9 then "IX" ^ to_roman (number - 9)
  else if number >= 5 then "V" ^ to_roman (number - 5)
  else if number >= 4 then "IV" ^ to_roman (number - 4)
  else if number >= 1 then "I" ^ to_roman (number - 1)
  else ""
