open Base

type prompt = Question | Yelling | YelledQuestion | NotSayingAnything | Other

let is_only_whitespace (str : string) : bool = String.("" = strip str)

let response = function
  | Question -> "Sure."
  | Yelling -> "Whoa, chill out!"
  | YelledQuestion -> "Calm down, I know what I'm doing!"
  | NotSayingAnything -> "Fine. Be that way!"
  | Other -> "Whatever."

let is_yelling sentence =
  let stripped = String.strip sentence in
  String.(String.uppercase stripped = stripped)
  && String.(String.lowercase stripped <> stripped)

let is_yelling_questin sentence =
  let stripped = String.strip sentence in
  String.(String.uppercase stripped = stripped)
  && String.(String.lowercase stripped <> stripped)
  && String.is_suffix ~suffix:"?" stripped

let is_question sentence =
  sentence |> String.strip |> String.is_suffix ~suffix:"?"

let is_not_saying_anything sentence = is_only_whitespace sentence

let response_for sentence =
  let prompt =
    if is_yelling_questin sentence then YelledQuestion
    else if is_yelling sentence then Yelling
    else if is_question sentence then Question
    else if is_not_saying_anything sentence then NotSayingAnything
    else Other
  in
  response prompt
