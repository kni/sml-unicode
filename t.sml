open Utf8

fun hexToString w =
  if w < 0w127 
  then Char.toString (Char.chr (Word.toInt w))
  else "\\u" ^ Word.toString w

fun decodeToHex str = String.concat (List.map hexToString (decode str))


fun is g e name =
  if String.compare (g, e) = General.EQUAL
  then print ("OK: " ^ name ^ "\n")
  else print ("NOT OK: " ^ name ^ " (got: '" ^ g ^ "', expected: '" ^ e ^ "')\n")

val _ = print "UTF8\n"

(* zя☺ *)
val b = "z\209\143\226\152\186" 
val u = decodeToHex b
val _ = is u "z\\u44F\\u263A" ("decode " ^ b) 
(* perl -E 'say join "", map { $_ < 127 ? chr($_) : sprintf "\\%03u",  $_ } unpack "C*", "zя☺"' *)
(* perl -Mutf8 -E 'say join "", map { $_ < 127 ? chr($_) : sprintf "\\\\u%03X",  $_ } unpack "C*", "zя☺"' *)

(* Вітаю, hello. ☺ *)
val b = "\208\146\209\150\209\130\208\176\209\142\044\032\104\101\108\108\111\046\032\226\152\186"
val u = decodeToHex b
val _ = is u "\\u412\\u456\\u442\\u430\\u44E, hello. \\u263A" ("decode " ^ b)

(* 🐱 *)
val b = "\240\159\144\177"
val u = decodeToHex b
val _ = is u "\\u1F431" ("decode " ^ b)

(* ☺🐱 *)
val b = "\226\152\186\240\159\144\177"
val u = decodeToHex b
val _ = is u "\\u263A\\u1F431" ("decode " ^ b)

(* zя☺🐱 *)
val b = "\122\209\143\226\152\186\240\159\144\177"
val cps = [0wx7a, 0wx44f, 0wx263a, 0wx1f431]
val u = decodeToHex b
val _ = is u "z\\u44F\\u263A\\u1F431" ("decode " ^ b)

(* zя☺🐱 *)
val cps = [0wx7a, 0wx44f, 0wx263a, 0wx1f431]
val expected = "\122\209\143\226\152\186\240\159\144\177"
val s = encode cps
val _ = is s expected ("encode " ^ expected)


val _ = print "\nUnicode\n"


(* "ß ﬀ ﬃ ☺. вІтАю. 🐱."*)
val s = "\195\159\032\239\172\128\032\239\172\131\032\226\152\186\046\032\208\178\208\134\209\130\208\144\209\142\046\032\240\159\144\177\046"
val _ = print (s ^ "\n")

val expected = "\115\115\032\102\102\032\102\102\105\032\226\152\186\046\032\208\178\209\150\209\130\208\176\209\142\046\032\240\159\144\177\046"
val got = Utf8.encode (Unicode.toCaseFolded (Utf8.decode s))
val _ = is got expected ("toCaseFolded " ^ got)


val expected = "\195\159\032\239\172\128\032\239\172\131\032\226\152\186\046\032\208\178\209\150\209\130\208\176\209\142\046\032\240\159\144\177\046"
val got = Utf8.encode (Unicode.toLower (Utf8.decode s))
val _ = is got expected ("toLower " ^ got)


val expected = "\083\083\032\070\070\032\070\070\073\032\226\152\186\046\032\208\146\208\134\208\162\208\144\208\174\046\032\240\159\144\177\046"
val got = Utf8.encode (Unicode.toUpper (Utf8.decode s))
val _ = is got expected ("toUpper " ^ got)


val expected = "\083\083\032\070\070\032\070\070\073\032\226\152\186\046\032\208\146\208\134\208\162\208\144\208\174\046\032\240\159\144\177\046"
val got = Utf8.encode (Unicode.toTitle (Utf8.decode s))
val _ = is got expected ("toTitle " ^ got)
