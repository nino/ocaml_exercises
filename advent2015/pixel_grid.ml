open Base

type t = bool array array

let create rows columns =
  Array.init rows ~f:(fun _ -> Array.create ~len:columns false)

let set t row col value = t.(row).(col) <- value

let at_or_false t row col =
  try Array.get (Array.get t row) col with _ -> false

let to_braille t =
  let braille_rows =
    Float.(of_int (Array.length t) /. 4.0 |> round_up |> to_int)
  in
  let braille_cols =
    Float.(of_int (Array.length t.(0)) /. 2.0 |> round_up |> to_int)
  in
  let braille_char row col =
    let elements =
      [
        ( at_or_false t ((4 * row) + 0) ((2 * col) + 0),
          at_or_false t ((4 * row) + 0) ((2 * col) + 1) );
        ( at_or_false t ((4 * row) + 1) ((2 * col) + 0),
          at_or_false t ((4 * row) + 1) ((2 * col) + 1) );
        ( at_or_false t ((4 * row) + 2) ((2 * col) + 0),
          at_or_false t ((4 * row) + 2) ((2 * col) + 1) );
        ( at_or_false t ((4 * row) + 3) ((2 * col) + 0),
          at_or_false t ((4 * row) + 3) ((2 * col) + 1) );
      ]
    in
    match elements with
    | [ (false, false); (false, false); (false, false); (false, false) ] -> "⠀"
    | [ (true, false); (false, false); (false, false); (false, false) ] -> "⠁"
    | [ (false, false); (true, false); (false, false); (false, false) ] -> "⠂"
    | [ (true, false); (true, false); (false, false); (false, false) ] -> "⠃"
    | [ (false, false); (false, false); (true, false); (false, false) ] -> "⠄"
    | [ (true, false); (false, false); (true, false); (false, false) ] -> "⠅"
    | [ (false, false); (true, false); (true, false); (false, false) ] -> "⠆"
    | [ (true, false); (true, false); (true, false); (false, false) ] -> "⠇"
    | [ (false, true); (false, false); (false, false); (false, false) ] -> "⠈"
    | [ (true, true); (false, false); (false, false); (false, false) ] -> "⠉"
    | [ (false, true); (true, false); (false, false); (false, false) ] -> "⠊"
    | [ (true, true); (true, false); (false, false); (false, false) ] -> "⠋"
    | [ (false, true); (false, false); (true, false); (false, false) ] -> "⠌"
    | [ (true, true); (false, false); (true, false); (false, false) ] -> "⠍"
    | [ (false, true); (true, false); (true, false); (false, false) ] -> "⠎"
    | [ (true, true); (true, false); (true, false); (false, false) ] -> "⠏"
    | [ (false, false); (false, true); (false, false); (false, false) ] -> "⠐"
    | [ (true, false); (false, true); (false, false); (false, false) ] -> "⠑"
    | [ (false, false); (true, true); (false, false); (false, false) ] -> "⠒"
    | [ (true, false); (true, true); (false, false); (false, false) ] -> "⠓"
    | [ (false, false); (false, true); (true, false); (false, false) ] -> "⠔"
    | [ (true, false); (false, true); (true, false); (false, false) ] -> "⠕"
    | [ (false, false); (true, true); (true, false); (false, false) ] -> "⠖"
    | [ (true, false); (true, true); (true, false); (false, false) ] -> "⠗"
    | [ (false, true); (false, true); (false, false); (false, false) ] -> "⠘"
    | [ (true, true); (false, true); (false, false); (false, false) ] -> "⠙"
    | [ (false, true); (true, true); (false, false); (false, false) ] -> "⠚"
    | [ (true, true); (true, true); (false, false); (false, false) ] -> "⠛"
    | [ (false, true); (false, true); (true, false); (false, false) ] -> "⠜"
    | [ (true, true); (false, true); (true, false); (false, false) ] -> "⠝"
    | [ (false, true); (true, true); (true, false); (false, false) ] -> "⠞"
    | [ (true, true); (true, true); (true, false); (false, false) ] -> "⠟"
    | [ (false, false); (false, false); (false, true); (false, false) ] -> "⠠"
    | [ (true, false); (false, false); (false, true); (false, false) ] -> "⠡"
    | [ (false, false); (true, false); (false, true); (false, false) ] -> "⠢"
    | [ (true, false); (true, false); (false, true); (false, false) ] -> "⠣"
    | [ (false, false); (false, false); (true, true); (false, false) ] -> "⠤"
    | [ (true, false); (false, false); (true, true); (false, false) ] -> "⠥"
    | [ (false, false); (true, false); (true, true); (false, false) ] -> "⠦"
    | [ (true, false); (true, false); (true, true); (false, false) ] -> "⠧"
    | [ (false, true); (false, false); (false, true); (false, false) ] -> "⠨"
    | [ (true, true); (false, false); (false, true); (false, false) ] -> "⠩"
    | [ (false, true); (true, false); (false, true); (false, false) ] -> "⠪"
    | [ (true, true); (true, false); (false, true); (false, false) ] -> "⠫"
    | [ (false, true); (false, false); (true, true); (false, false) ] -> "⠬"
    | [ (true, true); (false, false); (true, true); (false, false) ] -> "⠭"
    | [ (false, true); (true, false); (true, true); (false, false) ] -> "⠮"
    | [ (true, true); (true, false); (true, true); (false, false) ] -> "⠯"
    | [ (false, false); (false, true); (false, true); (false, false) ] -> "⠰"
    | [ (true, false); (false, true); (false, true); (false, false) ] -> "⠱"
    | [ (false, false); (true, true); (false, true); (false, false) ] -> "⠲"
    | [ (true, false); (true, true); (false, true); (false, false) ] -> "⠳"
    | [ (false, false); (false, true); (true, true); (false, false) ] -> "⠴"
    | [ (true, false); (false, true); (true, true); (false, false) ] -> "⠵"
    | [ (false, false); (true, true); (true, true); (false, false) ] -> "⠶"
    | [ (true, false); (true, true); (true, true); (false, false) ] -> "⠷"
    | [ (false, true); (false, true); (false, true); (false, false) ] -> "⠸"
    | [ (true, true); (false, true); (false, true); (false, false) ] -> "⠹"
    | [ (false, true); (true, true); (false, true); (false, false) ] -> "⠺"
    | [ (true, true); (true, true); (false, true); (false, false) ] -> "⠻"
    | [ (false, true); (false, true); (true, true); (false, false) ] -> "⠼"
    | [ (true, true); (false, true); (true, true); (false, false) ] -> "⠽"
    | [ (false, true); (true, true); (true, true); (false, false) ] -> "⠾"
    | [ (true, true); (true, true); (true, true); (false, false) ] -> "⠿"
    | [ (false, false); (false, false); (false, false); (true, false) ] -> "⡀"
    | [ (true, false); (false, false); (false, false); (true, false) ] -> "⡁"
    | [ (false, false); (true, false); (false, false); (true, false) ] -> "⡂"
    | [ (true, false); (true, false); (false, false); (true, false) ] -> "⡃"
    | [ (false, false); (false, false); (true, false); (true, false) ] -> "⡄"
    | [ (true, false); (false, false); (true, false); (true, false) ] -> "⡅"
    | [ (false, false); (true, false); (true, false); (true, false) ] -> "⡆"
    | [ (true, false); (true, false); (true, false); (true, false) ] -> "⡇"
    | [ (false, true); (false, false); (false, false); (true, false) ] -> "⡈"
    | [ (true, true); (false, false); (false, false); (true, false) ] -> "⡉"
    | [ (false, true); (true, false); (false, false); (true, false) ] -> "⡊"
    | [ (true, true); (true, false); (false, false); (true, false) ] -> "⡋"
    | [ (false, true); (false, false); (true, false); (true, false) ] -> "⡌"
    | [ (true, true); (false, false); (true, false); (true, false) ] -> "⡍"
    | [ (false, true); (true, false); (true, false); (true, false) ] -> "⡎"
    | [ (true, true); (true, false); (true, false); (true, false) ] -> "⡏"
    | [ (false, false); (false, true); (false, false); (true, false) ] -> "⡐"
    | [ (true, false); (false, true); (false, false); (true, false) ] -> "⡑"
    | [ (false, false); (true, true); (false, false); (true, false) ] -> "⡒"
    | [ (true, false); (true, true); (false, false); (true, false) ] -> "⡓"
    | [ (false, false); (false, true); (true, false); (true, false) ] -> "⡔"
    | [ (true, false); (false, true); (true, false); (true, false) ] -> "⡕"
    | [ (false, false); (true, true); (true, false); (true, false) ] -> "⡖"
    | [ (true, false); (true, true); (true, false); (true, false) ] -> "⡗"
    | [ (false, true); (false, true); (false, false); (true, false) ] -> "⡘"
    | [ (true, true); (false, true); (false, false); (true, false) ] -> "⡙"
    | [ (false, true); (true, true); (false, false); (true, false) ] -> "⡚"
    | [ (true, true); (true, true); (false, false); (true, false) ] -> "⡛"
    | [ (false, true); (false, true); (true, false); (true, false) ] -> "⡜"
    | [ (true, true); (false, true); (true, false); (true, false) ] -> "⡝"
    | [ (false, true); (true, true); (true, false); (true, false) ] -> "⡞"
    | [ (true, true); (true, true); (true, false); (true, false) ] -> "⡟"
    | [ (false, false); (false, false); (false, true); (true, false) ] -> "⡠"
    | [ (true, false); (false, false); (false, true); (true, false) ] -> "⡡"
    | [ (false, false); (true, false); (false, true); (true, false) ] -> "⡢"
    | [ (true, false); (true, false); (false, true); (true, false) ] -> "⡣"
    | [ (false, false); (false, false); (true, true); (true, false) ] -> "⡤"
    | [ (true, false); (false, false); (true, true); (true, false) ] -> "⡥"
    | [ (false, false); (true, false); (true, true); (true, false) ] -> "⡦"
    | [ (true, false); (true, false); (true, true); (true, false) ] -> "⡧"
    | [ (false, true); (false, false); (false, true); (true, false) ] -> "⡨"
    | [ (true, true); (false, false); (false, true); (true, false) ] -> "⡩"
    | [ (false, true); (true, false); (false, true); (true, false) ] -> "⡪"
    | [ (true, true); (true, false); (false, true); (true, false) ] -> "⡫"
    | [ (false, true); (false, false); (true, true); (true, false) ] -> "⡬"
    | [ (true, true); (false, false); (true, true); (true, false) ] -> "⡭"
    | [ (false, true); (true, false); (true, true); (true, false) ] -> "⡮"
    | [ (true, true); (true, false); (true, true); (true, false) ] -> "⡯"
    | [ (false, false); (false, true); (false, true); (true, false) ] -> "⡰"
    | [ (true, false); (false, true); (false, true); (true, false) ] -> "⡱"
    | [ (false, false); (true, true); (false, true); (true, false) ] -> "⡲"
    | [ (true, false); (true, true); (false, true); (true, false) ] -> "⡳"
    | [ (false, false); (false, true); (true, true); (true, false) ] -> "⡴"
    | [ (true, false); (false, true); (true, true); (true, false) ] -> "⡵"
    | [ (false, false); (true, true); (true, true); (true, false) ] -> "⡶"
    | [ (true, false); (true, true); (true, true); (true, false) ] -> "⡷"
    | [ (false, true); (false, true); (false, true); (true, false) ] -> "⡸"
    | [ (true, true); (false, true); (false, true); (true, false) ] -> "⡹"
    | [ (false, true); (true, true); (false, true); (true, false) ] -> "⡺"
    | [ (true, true); (true, true); (false, true); (true, false) ] -> "⡻"
    | [ (false, true); (false, true); (true, true); (true, false) ] -> "⡼"
    | [ (true, true); (false, true); (true, true); (true, false) ] -> "⡽"
    | [ (false, true); (true, true); (true, true); (true, false) ] -> "⡾"
    | [ (true, true); (true, true); (true, true); (true, false) ] -> "⡿"
    | [ (false, false); (false, false); (false, false); (false, true) ] -> "⢀"
    | [ (true, false); (false, false); (false, false); (false, true) ] -> "⢁"
    | [ (false, false); (true, false); (false, false); (false, true) ] -> "⢂"
    | [ (true, false); (true, false); (false, false); (false, true) ] -> "⢃"
    | [ (false, false); (false, false); (true, false); (false, true) ] -> "⢄"
    | [ (true, false); (false, false); (true, false); (false, true) ] -> "⢅"
    | [ (false, false); (true, false); (true, false); (false, true) ] -> "⢆"
    | [ (true, false); (true, false); (true, false); (false, true) ] -> "⢇"
    | [ (false, true); (false, false); (false, false); (false, true) ] -> "⢈"
    | [ (true, true); (false, false); (false, false); (false, true) ] -> "⢉"
    | [ (false, true); (true, false); (false, false); (false, true) ] -> "⢊"
    | [ (true, true); (true, false); (false, false); (false, true) ] -> "⢋"
    | [ (false, true); (false, false); (true, false); (false, true) ] -> "⢌"
    | [ (true, true); (false, false); (true, false); (false, true) ] -> "⢍"
    | [ (false, true); (true, false); (true, false); (false, true) ] -> "⢎"
    | [ (true, true); (true, false); (true, false); (false, true) ] -> "⢏"
    | [ (false, false); (false, true); (false, false); (false, true) ] -> "⢐"
    | [ (true, false); (false, true); (false, false); (false, true) ] -> "⢑"
    | [ (false, false); (true, true); (false, false); (false, true) ] -> "⢒"
    | [ (true, false); (true, true); (false, false); (false, true) ] -> "⢓"
    | [ (false, false); (false, true); (true, false); (false, true) ] -> "⢔"
    | [ (true, false); (false, true); (true, false); (false, true) ] -> "⢕"
    | [ (false, false); (true, true); (true, false); (false, true) ] -> "⢖"
    | [ (true, false); (true, true); (true, false); (false, true) ] -> "⢗"
    | [ (false, true); (false, true); (false, false); (false, true) ] -> "⢘"
    | [ (true, true); (false, true); (false, false); (false, true) ] -> "⢙"
    | [ (false, true); (true, true); (false, false); (false, true) ] -> "⢚"
    | [ (true, true); (true, true); (false, false); (false, true) ] -> "⢛"
    | [ (false, true); (false, true); (true, false); (false, true) ] -> "⢜"
    | [ (true, true); (false, true); (true, false); (false, true) ] -> "⢝"
    | [ (false, true); (true, true); (true, false); (false, true) ] -> "⢞"
    | [ (true, true); (true, true); (true, false); (false, true) ] -> "⢟"
    | [ (false, false); (false, false); (false, true); (false, true) ] -> "⢠"
    | [ (true, false); (false, false); (false, true); (false, true) ] -> "⢡"
    | [ (false, false); (true, false); (false, true); (false, true) ] -> "⢢"
    | [ (true, false); (true, false); (false, true); (false, true) ] -> "⢣"
    | [ (false, false); (false, false); (true, true); (false, true) ] -> "⢤"
    | [ (true, false); (false, false); (true, true); (false, true) ] -> "⢥"
    | [ (false, false); (true, false); (true, true); (false, true) ] -> "⢦"
    | [ (true, false); (true, false); (true, true); (false, true) ] -> "⢧"
    | [ (false, true); (false, false); (false, true); (false, true) ] -> "⢨"
    | [ (true, true); (false, false); (false, true); (false, true) ] -> "⢩"
    | [ (false, true); (true, false); (false, true); (false, true) ] -> "⢪"
    | [ (true, true); (true, false); (false, true); (false, true) ] -> "⢫"
    | [ (false, true); (false, false); (true, true); (false, true) ] -> "⢬"
    | [ (true, true); (false, false); (true, true); (false, true) ] -> "⢭"
    | [ (false, true); (true, false); (true, true); (false, true) ] -> "⢮"
    | [ (true, true); (true, false); (true, true); (false, true) ] -> "⢯"
    | [ (false, false); (false, true); (false, true); (false, true) ] -> "⢰"
    | [ (true, false); (false, true); (false, true); (false, true) ] -> "⢱"
    | [ (false, false); (true, true); (false, true); (false, true) ] -> "⢲"
    | [ (true, false); (true, true); (false, true); (false, true) ] -> "⢳"
    | [ (false, false); (false, true); (true, true); (false, true) ] -> "⢴"
    | [ (true, false); (false, true); (true, true); (false, true) ] -> "⢵"
    | [ (false, false); (true, true); (true, true); (false, true) ] -> "⢶"
    | [ (true, false); (true, true); (true, true); (false, true) ] -> "⢷"
    | [ (false, true); (false, true); (false, true); (false, true) ] -> "⢸"
    | [ (true, true); (false, true); (false, true); (false, true) ] -> "⢹"
    | [ (false, true); (true, true); (false, true); (false, true) ] -> "⢺"
    | [ (true, true); (true, true); (false, true); (false, true) ] -> "⢻"
    | [ (false, true); (false, true); (true, true); (false, true) ] -> "⢼"
    | [ (true, true); (false, true); (true, true); (false, true) ] -> "⢽"
    | [ (false, true); (true, true); (true, true); (false, true) ] -> "⢾"
    | [ (true, true); (true, true); (true, true); (false, true) ] -> "⢿"
    | [ (false, false); (false, false); (false, false); (true, true) ] -> "⣀"
    | [ (true, false); (false, false); (false, false); (true, true) ] -> "⣁"
    | [ (false, false); (true, false); (false, false); (true, true) ] -> "⣂"
    | [ (true, false); (true, false); (false, false); (true, true) ] -> "⣃"
    | [ (false, false); (false, false); (true, false); (true, true) ] -> "⣄"
    | [ (true, false); (false, false); (true, false); (true, true) ] -> "⣅"
    | [ (false, false); (true, false); (true, false); (true, true) ] -> "⣆"
    | [ (true, false); (true, false); (true, false); (true, true) ] -> "⣇"
    | [ (false, true); (false, false); (false, false); (true, true) ] -> "⣈"
    | [ (true, true); (false, false); (false, false); (true, true) ] -> "⣉"
    | [ (false, true); (true, false); (false, false); (true, true) ] -> "⣊"
    | [ (true, true); (true, false); (false, false); (true, true) ] -> "⣋"
    | [ (false, true); (false, false); (true, false); (true, true) ] -> "⣌"
    | [ (true, true); (false, false); (true, false); (true, true) ] -> "⣍"
    | [ (false, true); (true, false); (true, false); (true, true) ] -> "⣎"
    | [ (true, true); (true, false); (true, false); (true, true) ] -> "⣏"
    | [ (false, false); (false, true); (false, false); (true, true) ] -> "⣐"
    | [ (true, false); (false, true); (false, false); (true, true) ] -> "⣑"
    | [ (false, false); (true, true); (false, false); (true, true) ] -> "⣒"
    | [ (true, false); (true, true); (false, false); (true, true) ] -> "⣓"
    | [ (false, false); (false, true); (true, false); (true, true) ] -> "⣔"
    | [ (true, false); (false, true); (true, false); (true, true) ] -> "⣕"
    | [ (false, false); (true, true); (true, false); (true, true) ] -> "⣖"
    | [ (true, false); (true, true); (true, false); (true, true) ] -> "⣗"
    | [ (false, true); (false, true); (false, false); (true, true) ] -> "⣘"
    | [ (true, true); (false, true); (false, false); (true, true) ] -> "⣙"
    | [ (false, true); (true, true); (false, false); (true, true) ] -> "⣚"
    | [ (true, true); (true, true); (false, false); (true, true) ] -> "⣛"
    | [ (false, true); (false, true); (true, false); (true, true) ] -> "⣜"
    | [ (true, true); (false, true); (true, false); (true, true) ] -> "⣝"
    | [ (false, true); (true, true); (true, false); (true, true) ] -> "⣞"
    | [ (true, true); (true, true); (true, false); (true, true) ] -> "⣟"
    | [ (false, false); (false, false); (false, true); (true, true) ] -> "⣠"
    | [ (true, false); (false, false); (false, true); (true, true) ] -> "⣡"
    | [ (false, false); (true, false); (false, true); (true, true) ] -> "⣢"
    | [ (true, false); (true, false); (false, true); (true, true) ] -> "⣣"
    | [ (false, false); (false, false); (true, true); (true, true) ] -> "⣤"
    | [ (true, false); (false, false); (true, true); (true, true) ] -> "⣥"
    | [ (false, false); (true, false); (true, true); (true, true) ] -> "⣦"
    | [ (true, false); (true, false); (true, true); (true, true) ] -> "⣧"
    | [ (false, true); (false, false); (false, true); (true, true) ] -> "⣨"
    | [ (true, true); (false, false); (false, true); (true, true) ] -> "⣩"
    | [ (false, true); (true, false); (false, true); (true, true) ] -> "⣪"
    | [ (true, true); (true, false); (false, true); (true, true) ] -> "⣫"
    | [ (false, true); (false, false); (true, true); (true, true) ] -> "⣬"
    | [ (true, true); (false, false); (true, true); (true, true) ] -> "⣭"
    | [ (false, true); (true, false); (true, true); (true, true) ] -> "⣮"
    | [ (true, true); (true, false); (true, true); (true, true) ] -> "⣯"
    | [ (false, false); (false, true); (false, true); (true, true) ] -> "⣰"
    | [ (true, false); (false, true); (false, true); (true, true) ] -> "⣱"
    | [ (false, false); (true, true); (false, true); (true, true) ] -> "⣲"
    | [ (true, false); (true, true); (false, true); (true, true) ] -> "⣳"
    | [ (false, false); (false, true); (true, true); (true, true) ] -> "⣴"
    | [ (true, false); (false, true); (true, true); (true, true) ] -> "⣵"
    | [ (false, false); (true, true); (true, true); (true, true) ] -> "⣶"
    | [ (true, false); (true, true); (true, true); (true, true) ] -> "⣷"
    | [ (false, true); (false, true); (false, true); (true, true) ] -> "⣸"
    | [ (true, true); (false, true); (false, true); (true, true) ] -> "⣹"
    | [ (false, true); (true, true); (false, true); (true, true) ] -> "⣺"
    | [ (true, true); (true, true); (false, true); (true, true) ] -> "⣻"
    | [ (false, true); (false, true); (true, true); (true, true) ] -> "⣼"
    | [ (true, true); (false, true); (true, true); (true, true) ] -> "⣽"
    | [ (false, true); (true, true); (true, true); (true, true) ] -> "⣾"
    | [ (true, true); (true, true); (true, true); (true, true) ] -> "⣿"
    | _ -> failwith "This shouldn't happen"
  in
  Array.init braille_rows ~f:(fun row ->
      Array.init braille_cols ~f:(fun col -> braille_char row col))

let print_braille t =
  let str =
    to_braille t
    |> Array.map ~f:(fun row -> Array.to_list row |> String.concat)
    |> Array.to_list |> String.concat ~sep:"\n"
  in
  Stdio.print_string str