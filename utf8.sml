structure Utf8 :
sig
 val decode  : string -> word list
 val encode  : word list -> string

 val decode' : word list -> (char, 'cs) StringCvt.reader -> (word list, 'cs) StringCvt.reader
 val encode' : word * char list -> char list
end
= 
struct

local
  val <<   = Word.<<
  val andb = Word.andb

  infix 7 << andb

  val replacement = 0wxfffd
in

fun decode' r getc strm =
  let

    fun add (w, c2) =
      let
        val w2 = Word.fromInt (ord c2)
      in
        if w2 > 0wxc0 (* 10xxxxxx *) orelse w = replacement then replacement else
        (w << 0w6) + (0wx3f andb w2) (* 00111111 *)
      end

    infix add

    fun scan strm r =
      case getc strm of NONE => SOME (r, strm) | SOME (c, strm) =>
      let
        val w = Word.fromInt (ord c)
      in
        if w < 0wx80 (* 0xxxxxxx *) then scan strm (w::r)

        else if w < 0wxe0 (* 110xxxxx *) then
          case getc strm of NONE => scan strm (replacement::r) | SOME (c2, strm) =>
          scan strm ((0wx1f andb w add c2)::r) (* 00011111 *)

        else if w < 0wxf0 (* 1110xxxx *) then
          case getc strm of NONE => scan strm (replacement::r) | SOME (c2, strm) =>
          case getc strm of NONE => scan strm (replacement::r) | SOME (c3, strm) =>
          scan strm ((0wx0f andb w add c2 add c3)::r) (* 00001111 *)

        else if w < 0wxf8 (* 11111xxx *) then
          case getc strm of NONE => scan strm (replacement::r) | SOME (c2, strm) =>
          case getc strm of NONE => scan strm (replacement::r) | SOME (c3, strm) =>
          case getc strm of NONE => scan strm (replacement::r) | SOME (c4, strm) =>
          scan strm ((0wx07 andb w add c2 add c3 add c4)::r) (* 00000111 *)

        else scan strm (replacement::r)
      end
  in
    scan strm r
  end

end



local
  val >>   = Word.>>
  val orb  = Word.orb
  val andb = Word.andb

  infix 7 >> orb andb

  val toChar = Char.chr o Word.toInt
in

  fun encode' (cp, r) =
    if cp < 0wx80 then
      toChar cp ::
      r
    else if cp < 0wx800 then
      toChar (0wxc0 orb (cp >> 0w6)) ::
      toChar (0wx80 orb (cp andb 0wx3f)) ::
      r
    else if cp < 0wx10000 then
      toChar (0wxe0 orb (cp >> 0w12)) ::
      toChar (0wx80 orb (cp >> 0w6 andb 0wx3f)) ::
      toChar (0wx80 orb (cp andb 0wx3f)) ::
      r
    else if cp < 0wx10ffff then
      toChar (0wxf0 orb (cp >> 0w18)) ::
      toChar (0wx80 orb (cp >> 0w12 andb 0wx3f)) ::
      toChar (0wx80 orb (cp >> 0w6 andb 0wx3f)) ::
      toChar (0wx80 orb (cp andb 0wx3f)) ::
      r
    else r
end


fun decode str = List.rev (Option.valOf (StringCvt.scanString (decode' []) str))

fun encode cps = String.implode (List.foldl encode' [] (List.rev cps))

end
