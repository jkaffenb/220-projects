fun catstrings x = foldr (fn (x, a) => x^a) "" x;

fun removedups _ [] = []
| removedups comp (a::rest) =
  a::removedups comp (List.filter (fn x => not (comp (x, a))) (a::rest));

fun flatten [] = [] | flatten (a::rest) = a @ (flatten rest);

fun inttostring x = if (x div 10) = 0 then str(chr((x mod 10) + 48))
                    else inttostring (x div 10) ^ str(chr((x mod 10) + 48));

(*Probably a better way, but I used List.take and List.drop to splice the lst*)
fun allplaceshelper x index list =
    if (length list) = (index - 1) then []
    else [List.take (list, index) @ [x] @ List.drop (list, index)] @
                                           (allplaceshelper x (index + 1) list);

fun allplaces x list = allplaceshelper x 0 list;

fun helper org_list [] = []
| helper org_list (a::rest) = if org_list = (a::rest) then []
                                else (allplaces a rest) @
                                (helper org_list (rest @ [a]));

fun helper2 _ [] = []
| helper2 item (a::rest) = if (item = a) then (helper2 item rest)
                           else [a] @ (helper2 item rest);

fun helper1 [] = []
| helper1 (a::rest) = [a] @ (helper1 (helper2 a rest));

fun perms [] = []
| perms [a] = [[a]]
| perms (a::rest) =
  helper1 ((allplaces a rest) @ (helper (a::rest) (rest @ [a])));

datatype 'a bst = EMPTY | NODE of 'a * 'a bst * 'a bst;

fun search _ _ _ EMPTY = NONE
| search equal less item (NODE(top, EMPTY, right)) =
  if equal(item, top) then SOME (NODE(top, EMPTY, right))
  else search equal less item right
| search equal less item (NODE(top, (NODE(topleft, lleft, lright)), right)) =
  if equal(item, top)
  then SOME (NODE(top, (NODE(topleft, lleft, lright)), right))
  else if less(item, topleft)
       then search equal less item (NODE(topleft, lleft, lright))
       else search equal less item right;


datatype bigint = BIG of string

exception Error;

fun isNumeral x = isSome (List.find (fn c => x = c) (explode "0123456789"));
fun isNeg x = isSome (List.find (fn c => x = c) (explode "~0123456789"));

fun validInt s =
    let fun help [] = false
          | help [c] = isNumeral c
          | help (a::b::rest) = if List.all isNeg [a] then b <> #"0" andalso
            List.all isNumeral (b::rest) else if List.all isNumeral (a::rest)
            andalso a <> #"0" then true else false
    in (help o explode) s end;


fun compare (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    let val (ax::restx) = explode x
        val (ay::resty) = explode y
    in
    (*if both negative*)
    if ax = (chr 126) andalso ay = (chr 126) then compare (BIG (implode resty), BIG (implode restx))
    else
    (*if ax negative*)
    if ax = (chr 126) then LESS
    else
    (*if ay is negative*)
    if ay = (chr 126) then GREATER
    else
    (*both are positive*)
    if length (explode x) < length (explode y) then LESS
    else if length (explode x) > length (explode y) then GREATER
    else
    if ax = ay andalso length (ax::restx) = 1 then EQUAL
    else if ax = ay then compare (BIG (implode restx), BIG (implode resty))
    else if ax > ay then GREATER
         else LESS
end;

fun addhelper [] [] 0 = []
| addhelper [] [] _ = [(chr 49)]
| addhelper [] (a::rest) carry = chr ((((ord a) - 48 + carry) mod 10) + 48)::(addhelper [] rest (((ord a) - 48 + carry) div 10))
| addhelper (a::rest) [] carry = chr ((((ord a) - 48 + carry) mod 10) + 48)::(addhelper rest [] (((ord a) - 48 + carry) div 10))
| addhelper (ax::restx) (ay::resty) carry =
  let val x' = (ord ax) - 48
      val y' = (ord ay) - 48
  in chr (((x' + y' + carry) mod 10) + 48)::(addhelper restx resty ((x' + y' + carry) div 10))
end;

fun subhelper [] [] 0 = []
| subhelper [] y _ = y
| subhelper (ax::restx) [] carry =
  if ((ord ax) - 48) - carry >= 0 then (chr ((ord ax) - carry))::(subhelper restx [] 0)
  else (chr ((ord ax) - carry + 10))::(subhelper restx [] 1)
| subhelper (ax::restx) (ay::resty) carry =
  let val x' = (ord ax) - 48
      val y' = (ord ay) - 48
  in if length (ay::resty) > length(ax::restx) then (subhelper (ay::resty) (ax::restx) 0) @ [(chr 126)]
  else
  if (x' - y' - carry) >= 0 then chr ((x' - y' - carry) + 48)::(subhelper restx resty 0)
  else if length (ax::restx) = 1 then chr (~1 * (x' - y' - carry) + 48)::[(chr 126)]
       else chr ((x' + 10 - y' - carry) + 48)::(subhelper restx resty 1)
end;


fun add (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    let val (ax::restx) = explode x
        val (ay::resty) = explode y
    in
    (*case for both negative WORKS*)
    if ax = (chr 126) andalso ay = (chr 126) then mul (add (BIG (implode restx), BIG (implode resty)), BIG "~1")
    else
    (*case for ax negative WORKS*)
    if ax = (chr 126) then subtract (BIG y, BIG (implode restx))
    else
    (*case for ay negative WORKS*)
    if ay = (chr 126) then subtract (BIG x, BIG (implode resty))
    else
    (*case for none negative WORKS*)
    BIG(implode(rev(addhelper (rev(explode x)) (rev(explode y)) 0)))
    end
and subtract (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    let val (ax::restx) = explode x
        val (ay::resty) = explode y
    in
    (*case for both negative WORKS*)
    if ax = (chr 126) andalso ay = (chr 126) then add ((BIG x), (BIG (implode(resty))))
    else
    (*case for first negative WORKS*)
    if ax = (chr 126) then mul(add (BIG (implode(restx)), BIG y), BIG "~1")
    else
    (*case for second negative WORKS*)
    if ay = (chr 126) then add ((BIG x), (BIG (implode(resty))))
    else
    (*case for none negative WORKS*)
    removeleading(BIG(implode(rev(subhelper (rev(explode x)) (rev(explode y)) 0))))
    end
and mul (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    let val (ax::restx) = explode x
        val (ay::resty) = explode y
    in
    (*case for both negative WORKS*)
    if ax = (chr 126) andalso ay = (chr 126) then mul (BIG (implode restx), BIG (implode resty))
    else
    (*write a case for multiplying by ~1 WORKS*)
    if compare (BIG x, BIG "~1") = EQUAL then BIG(implode((chr 126)::explode y))
    else
    if compare (BIG y, BIG "~1") = EQUAL then BIG(implode((chr 126)::explode x))
    else
    (*case for first negative WORKS*)
    if ax = (chr 126) then mul(mul (BIG (implode(restx)), BIG y), BIG "~1")
    else
    (*case for second negative WORKS*)
    if ay = (chr 126) then mul(mul (BIG (implode(resty)), BIG x), BIG "~1")
    else
    (*case for both positive WORKS*)
    mulhelper (BIG x, BIG y)
    end
and mulhelper (BIG x, BIG y) =
    if compare (BIG x, BIG "0") = EQUAL then BIG "0"
    else
    if compare (BIG y, BIG "0") = EQUAL then BIG "0"
    else
    add (BIG y, (mulhelper ((subtract (BIG x, BIG "1")), (BIG y))))
and removeleading (BIG x) =
    if compare (BIG x, BIG "0") = EQUAL then BIG "0"
    else
    let val (ax::restx) = explode x
    in
    if ax = (chr 126) then mul(removeleading (BIG (implode restx)), BIG "~1")
    else
    if ax = (chr 48) then removeleading (BIG (implode restx))
    else BIG x
    end
and pow (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    if (hd (explode y)) = (chr 126) then raise Error else
    if compare (BIG y, BIG "0") = EQUAL then BIG "1"
    else
    if compare (BIG y, BIG "1") = EQUAL then BIG x
    else
    if compare (BIG x, BIG "0") = EQUAL then BIG "0"
    else
    mul (BIG x, (pow (BIG x, subtract(BIG y, BIG "1"))))
and divide (BIG x, BIG y) =
    if not(validInt x) orelse not(validInt y) then raise Error else
    if (hd (explode y)) = (chr 48) then raise Error else
    if compare (BIG x, BIG "0") = EQUAL then BIG "0"
    else
    if compare (BIG y, BIG "1") = EQUAL then BIG x
    else
    let val (ax::restx) = explode x
        val (ay::resty) = explode y
    in
    (*case for both negative WORKS*)
    if ax = (chr 126) andalso ay = (chr 126) then divide (BIG (implode restx), BIG (implode resty))
    else
    (*case for first negative*)
    if ax = (chr 126) then mul(divide (BIG (implode restx), BIG y), BIG "~1")
    else
    (*case for second negative*)
    if ay = (chr 126) then mul(divide (BIG x,BIG (implode resty)), BIG "~1")
    else
    (*case for both positive WORKS*)
    dividehelper (BIG x, BIG y)
    end
and dividehelper (BIG x, BIG y) =
    if compare (BIG x, BIG y) = EQUAL then BIG "1"
    else
    if compare (BIG x, BIG y) = LESS then BIG "0"
    else
    add (BIG "1", dividehelper ((subtract(BIG x, BIG y)), BIG y));
