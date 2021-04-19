(* projet.ml : projet 
************************************************************************************************************************************************************
*                                                                                                                                                          *
*                                                              Projet Ocaml                                                                                *
*                                                                                                                                                          *
************************************************************************************************************************************************************
*Date      : February 14, 2019                                                                                                                             *
*Group     : MIN-Int                                                                                                                                       *
*Student 1 : Duriez Kévin                                                                                                                                  *
*Student 2 : Da Costa Barros Fabien                                                                                                                        *
************************************************************************************************************************************************************
*)

(*Q1*)

type 'a ensemble = Ve | Ce of 'a * 'a ensemble;;

let rec cardinalite ens =
  match ens with
  |Ve->0
  |Ce(_,a)->1+cardinalite a;;

let rec appartenance elt ens =
  match ens with
  |Ve->false
  |Ce(a,b)->if a=elt then true else appartenance elt b;;

let rec inclusion ens1 ens2 =
  match ens1 with
  |Ve->true
  |Ce(a,b)->if appartenance a ens2 then inclusion b ens2 else false;;
    
let ajout elt ens =
  match ens with
  |Ve->Ce(elt,Ve)
  |Ce(a,b)->if appartenance elt ens then ens else Ce(elt,Ce(a,b));;

let rec suppression elt ens =
  match ens with
  |Ve->ens
  |Ce(a,b)->if a=elt then b else Ce(a,suppression elt b);;


let egalite ens1 ens2 =
  (inclusion ens1 ens2) && ((cardinalite ens1) = (cardinalite ens2));;

let rec intersection ens1 ens2 =
  match ens1 with
  |Ve->Ve
  |Ce(a,b)->if appartenance a ens2 then Ce(a,intersection b ens2)else intersection b ens2;;

let rec union ens1 ens2 =
  match ens1 with
  |Ve->ens2
  |Ce(_,_)->match ens2 with
            |Ve->ens1
            |Ce(a1,b1)->if appartenance a1 ens1 then Ce(a1,(union (suppression a1 ens1) b1))
                                                else Ce(a1,(union ens1 b1));;

let rec difference ens1 ens2 =
  match ens1 with
  |Ve->Ve
  |Ce(a,b)->if appartenance a ens2 then difference (suppression a ens1) ens2 else Ce(a,difference b ens2);;


let rec difference_symetrique ens1 ens2 =
  difference (union ens1 ens2) (intersection ens1 ens2);;



(* Q2 *)
type 'a ensemble = 'a list;;


let t =  [1;12;28;6];;
let rec cardinalite ens =
  match ens with
  |[] ->0
  |_::a-> 1+cardinalite a;;

cardinalite t;;

let rec appartenance elt ens =
  match ens with
  |[]->false
  |a::b->if a=elt then true else appartenance elt b;;

appartenance 8 t;;
appartenance 28 t;;

let a =  [28;6];;
let z = [12;28];;
let e = [54;6];;

let rec inclusion ens1 ens2 =
  match ens1 with
  |[]->true
  |a::b->if appartenance a ens2 then inclusion b ens2 else false;;

inclusion a t;;
inclusion z t;;
inclusion e t;;

let ajout elt ens =
  match ens with
  |[]->elt::[]
  |a::b->if appartenance elt ens then ens else elt::(a::b);;

ajout 6 t;;
ajout 8 t;;

let rec suppression elt ens =
  match ens with
  |[]->ens
  |a::b->if a=elt then b else a::(suppression elt b);;

suppression 6 t;;
suppression 5 t;;

let egalite ens1 ens2 =
  (inclusion ens1 ens2) && ((cardinalite ens1) = (cardinalite ens2));;

let rec intersection ens1 ens2 =
  match ens1 with
  |[]->[]
  |a::b->if appartenance a ens2 then a::intersection b ens2 else intersection b ens2;;


let rec union ens1 ens2 =
  match ens1 with
  |[]-> ens2
  |_::_->match ens2 with
            |[]->ens1
            |a1::b1->if appartenance a1 ens1 then let ens1 = suppression a1 ens1 in
                                                  a1::(union ens1 b1)
                                                else a1::(union ens1 b1);;

let z = [35;28];;
let e = [54;6];;
union e t;;
union z t;;


let rec difference ens1 ens2 =
  match ens1 with
  |[]-> []
  |a::b->if appartenance a ens2 then difference (suppression a ens1) ens2 else a::(difference b ens2);;

difference [1;12;28;6;] [28;6];;


let rec difference_symetrique ens1 ens2 =
  difference (union ens1 ens2) (intersection ens1 ens2);;

difference_symetrique [1;12;28;6;] [28;6;32];;

(*Q3*)

let cardinalite ens = List.fold_left (fun a b -> a+1) 0 ens;;
cardinalite [1;12;28;6];;

let exist (p: 'a -> bool) (l:'a list) = List.fold_left (fun res elt -> res || (p elt)) false l;;
let appartenance elt ens = exist (fun a -> a=elt) ens;;
appartenance 8 [1;12;28;6];;
appartenance 28 [1;12;28;6];;

let pour_tous (p: 'a -> bool) (l:'a list) = List.fold_left (fun res elt -> res && (p elt)) true l;;
let inclusion ens1 ens2 = pour_tous (fun a -> appartenance a ens2) ens1;;
inclusion [28;12;6] [1;12;28;6];;
inclusion [28;12;6;56] [1;12;28;6];;

let ajout elt ens = if appartenance elt ens then ens else elt::ens;;

(*This function "filtre" reverse the list more than filter it *)
let filtre (p: 'a -> bool) (l:'a list) = List.fold_left (fun acc elt -> if (p elt) then elt::acc else acc) [] l;;

let suppression elt ens = List.filter (fun a -> elt != a) ens;;
suppression 12 [1;12;28;6];;

let egalite ens1 ens2 = (inclusion ens1 ens2) && ((cardinalite ens1) = (cardinalite ens2));;

let intersection ens1 ens2 = List.filter (fun a -> appartenance a ens1) ens2;;
intersection [28;12;6;56] [1;12;28;6];;
intersection [1;12;28;6] [28;12;6;56];;

let union ens1 ens2 = ens1@(List.filter (fun a ->not (appartenance a ens1)) ens2);;

union [1;12;28;6] [28;12;6;56];;
union [1;12;28;6] [32;72;64;56];;
union [1;12;28;6] [1;12;28;6];;
union [1;12;28;6] [];;

let difference ens1 ens2 = List.filter (fun a->not (appartenance a ens2)) ens1;;

difference [1;12;28;6] [28;12;6;56];;
difference [1;12;28;6] [32;72;64;56];;
difference [1;12;28;6] [1;12;28;6];;
difference [1;12;28;6] [];;

let difference_symetrique ens1 ens2 = difference (union ens1 ens2) (intersection ens1 ens2);;
(*Q4*)
type 'a multielement = 'a *int;;
(*type 'a multiensemble = Ve | Ce of 'a multielement * 'a multiensemble;; *)

type 'a multiensemble='a multielement ensemble;;

(*Q5*)
let rec cardinalite (mens:'a multiensemble) =
  match mens with
  |[]->(0,0)
  |(_,m)::ens-> let (a,b)= (cardinalite ens) in (a+1,b+m);;

let cardinalite (mens:'a multiensemble) = List.fold_left (+) 0 ( List.map (fun (_,a) -> a) mens);;

cardinalite [('m',2);('u',1)];;
cardinalite [];;

let rec occurence x mens =
  match mens with
  |[]-> 0
  |(a,b)::ens->if a=x then b else occurence x ens ;;

let occurence (x:'a) (mens:'a multiensemble) =cardinalite (List.filter(fun (a,b)-> x=a) mens);;

occurence 'm' [('m',2);('u',1)];;
occurence 'j' [('m',2);('u',1)];;

let rec appartenance melt mens =
  match mens with
  |[]->false
  |(a,b)::ens->let (c,d)=melt in if (a=c) && (b>=d) then true else appartenance melt ens;;

let appartenance melt mens = let (c,d)=melt in List.exists (fun (a,b) ->(a=c) && (b>=d)) mens;;

let rec inclusion mens1 mens2 =
  match mens1 with
  |[]->true
  |(a,b)::ens->if appartenance (a,b) mens2 then inclusion ens mens2 else false;;

let inclusion mens1 mens2 = (List.for_all (fun x -> appartenance x mens2) mens1) ;;

inclusion [('o',2);('m',1);('j',1)] [('o',5);('c',3);('a',1);('m',2);('l',4)];;

let rec ajout melt mens =
  match mens with
  |[]->[melt]
  |(a,b)::ens->let (c,d)=melt in if a=c then (a,b+d)::ens else (a,b)::ajout melt ens;;

let ajout (melt:'a multielement) (mens:'a multiensemble) = let (c,d)=melt in if appartenance (c,1) mens
                                                                                 then  (List.map (fun (a,b) -> if a=c then (a,b+d) else (a,b)) mens)
                                                                                 else melt::mens;;
ajout ('z',8) [('o',5);('c',3);('a',1);('m',2);('l',4)];;

let rec suppression melt mens =
  match mens with
  |[]->mens
  |(a,b)::ens->let (c,d)=melt in if a=c then if (b-d)<=0 then ens else (a,b-d)::ens else (a,b)::suppression melt ens;;

let suppression (melt:'a multielement) (mens:'a multiensemble) = let (c,d)=melt in (List.map (fun (a,b) -> if a=c then (a,b-d) else (a,b)) (List.filter (fun (a,b) -> a=c && (b-d)>0 || a!=c) mens));;

suppression ('c',2) [('o',5);('c',3);('a',1);('m',2);('l',4)];;
suppression ('l',8) [('o',5);('c',3);('a',1);('m',2);('l',4)];;

let egalite mens1 mens2 =
  (inclusion mens1 mens2) && ((cardinalite mens1) = (cardinalite mens2));;

let intersection mens1 mens2 = List.map (fun (a,_) -> if  occurence a mens1 > occurence a mens2
                                                          then (a,occurence a mens2)
                                                          else (a,occurence a mens1)) (List.filter (fun (a,_) -> appartenance (a,1) mens2) mens1);;

intersection [('o',2);('m',1);('j',1)] [('o',5);('c',3);('a',1);('m',2);('l',4)];;
intersection [('o',5);('c',3);('a',1);('m',2);('l',4)] [('o',2);('m',1);('j',1)];;

let rec union mens1 mens2 =
  match mens1 with
  |[]->mens2
  |(a,b)::_->match mens2 with
            |[]->mens1 
            |(c,d)::ens->if a=c then (a,b+d)::(union (suppression (a,b) mens1) ens)
                            else (c,d)::(union mens1 ens);;

let union mens1 mens2 = List.map (fun (a,b) -> (a, occurence a mens1 + occurence a mens2)) mens1@(List.filter (fun (a,_) -> not (appartenance (a,1) mens1)) mens2);;

union [('o',2);('m',1);('j',1)] [('o',5);('c',3);('a',1);('m',2);('l',4)];;

let rec difference mens1 mens2 =
  match mens1 with
  |[]->[]
  |(a,b)::ens->if appartenance (a,b) mens2 then difference (suppression (a,b) mens1) mens2 else (a,b)::difference ens mens2;;

let difference mens1 mens2 = List.fold_left (fun x y ->(intersection x y)) (union mens1 mens2) (List.map (fun x ->  suppression x mens1) mens2);;

difference [('o',2);('m',1);('j',1)] [('o',5);('c',3);('a',1);('m',2);('l',4)];;
difference [('o',5);('c',3);('a',1);('m',2);('l',4)] [('o',2);('m',1);('j',1)];;
difference [('o',5);('c',3);('a',1);('m',2);('l',4)] [('o',2);('m',1);('c',12)];;

let rec difference_symetrique mens1 mens2 =
  difference (union mens1 mens2) (intersection mens1 mens2);;

type 'a ensemble = 'a list;;
(*Q6*)
type lettre= char;;
type mot= lettre list;;

(*Q7*)

type dictionnaire = mot ensemble;;

(*Q8 et Q9*)

let cst_DICO=[['q';'u';'e';'l';'l';'e'];['m';'i';'n';'i';'s';'t';'r';'e'];['s';'e';'c';'h';'e'];['l';'a'];['p';'o';'u';'l';'e'];['q';'u';'i'];['m';'u';'e'];['r';'u';'e'];['d';'e'];['l';'a'];['p';'a';'i';'x']];;

(*Q10*)

type phrase = mot list ;;

(*Q11*)

let rec supprimePrefixeCommun mot1 mot2 =
  if (List.hd mot1) = (List.hd mot2) then ((List.tl mot1),(List.tl mot2)) else (mot1,mot2);;

supprimePrefixeCommun ['s';'e';'c';'h';'e'] ['s';'e';'c';'h';'e'];;
supprimePrefixeCommun ['s';'e';'c';'h';'e'] ['m';'e';'c';'h';'e'];;

let suffixeEgaux mot1 mot2 =
  if ((List.hd mot1)!=(List.hd mot2)) && ((List.tl mot1) = (List.tl mot2)) then true else false;;

suffixeEgaux ['s';'e';'c';'h';'e'] ['s';'e';'c';'h';'e'];;
suffixeEgaux ['s';'e';'c';'h';'e'] ['m';'e';'c';'h';'e'];;

 (*Q12*)

let motsSontContrepet ensmt1 ensmt2 =
  let (m1,m2)=ensmt1 and (m'1,m'2)=ensmt2 in
  ((List.hd m1) = (List.hd m'2)) && ((List.hd m2) = (List.hd m'1)) && (suffixeEgaux m1 m'1)&&(suffixeEgaux m2 m'2);;

motsSontContrepet (['m';'i';'n'],['s';'e';'c';'h';'e']) (['s';'i';'n'],['m';'e';'c';'h';'e']);;
motsSontContrepet (['m';'i';'n'],['s';'e';'c';'h';'e']) (['s';'i';'n'],['p';'e';'c';'h';'e']);;
motsSontContrepet (['m';'i';'n'],['s';'e';'c';'h';'e']) (['m';'i';'n'],['s';'e';'c';'h';'e']);;

(*Q13 à refaire*)

let rec phrasesSontContrepet ph1 ph2 =
  if (List.hd ph1) = (List.hd ph2)
  then phrasesSontContrepet (List.tl ph1) (List.tl ph2)
  else let m1=(List.hd ph1) and m'1=(List.hd ph2) in
       if (List.length (List.tl ph1))>0 && (List.length (List.tl ph2))>0
       then if (List.hd (List.tl ph1))!=(List.hd (List.tl ph2))
            then let m2=(List.hd (List.tl ph1)) and m'2=(List.hd (List.tl ph2)) in
            motsSontContrepet (m1,m2) (m'1,m'2)
            else phrasesSontContrepet (List.tl (List.tl ph1)) (List.tl (List.tl ph2))
       else false;;

phrasesSontContrepet [['q';'u';'e';'l';'l';'e'];['m';'i';'n';'i';'s';'t';'r';'e'];['s';'e';'c';'h';'e']] [['q';'u';'e';'l';'l';'e'];['s';'i';'n';'i';'s';'t';'r';'e'];['m';'e';'c';'h';'e']];;
phrasesSontContrepet [['q';'u';'e';'l';'l';'e'];['m';'i';'n';'i';'s';'t';'r';'e'];['q';'u';'e';'l';'l';'e'];['s';'e';'c';'h';'e']] [['q';'u';'e';'l';'l';'e'];['m';'i';'n';'i';'s';'t';'r';'e'];['q';'u';'e';'l';'l';'e'];['p';'e';'c';'h';'e']];;

(*Q14*)

let rec decompose (mot:mot) =
  match mot with
  |[a]->[([],a,[])]
  |x::xs->let decomp_xs=decompose xs in
          ([],x,xs)::(List.map (fun (p,l,s)->(x::p,l,s)) decomp_xs);;

decompose ['a'];;
decompose ['s';'i';'n'];;

let echange (p1,l1,s1) (p2,l2,s2) =
  (p1@(l2::s1),p2@(l1::s2));;

let cst_dico1 = [['m';'i';'n';'i';'s';'t';'r';'e'];['s';'i';'n';'i';'s';'t';'r';'e'];['m';'e';'c';'h';'e'];['s';'e';'c';'h';'e'];['t';'a';'r';'e'];['r';'a';'m';'e'];['m';'a';'r';'e'];['r';'a';'t';'e']];;

let appartenance elt ens = List.exists (fun a -> a=elt) ens;;

let contrepetreMots (dico:dictionnaire) (m1:mot) (m2:mot) : (mot*mot) list=
  let dec_m1 = decompose m1 and dec_m2 = decompose m2 in
  List.fold_left
    (fun acc1 dec_mot1 ->
        acc1@List.fold_left
               (fun acc2 dec_mot2 -> let (p1,l1,s1) = dec_mot1 and (p2,l2,s2)= dec_mot2 in if let(mot1,mot2)=echange (p1,l1,s1) (p2,l2,s2) in (appartenance mot1 dico && appartenance mot2 dico) && not (mot1=m1 && mot2=m2) then echange (p1,l1,s1) (p2,l2,s2)::acc2 else acc2)
               [] dec_m2 )
    [] dec_m1;;

let echangeMot (motASup1:mot) (motASup2:mot) (motsContrepetries:mot*mot) (phrase:phrase) : phrase = let (nouvMot1,nouvMot2)=motsContrepetries in List.fold_left (fun acc mot -> if (mot=motASup1) then acc@[nouvMot1] else if (mot=motASup2) then acc@[nouvMot2] else acc@[mot]) [] phrase;;

let contrepetries (dico:dictionnaire) (phrase:phrase) : phrase ensemble =
  List.fold_left
    (fun acc1 mot1 -> (List.fold_left
                         (fun acc2 mot2 ->
                           if not ((contrepetreMots dico mot1 mot2)=[]) then (List.fold_left (fun acc3 motsCtrp -> ((echangeMot mot1 mot2 motsCtrp phrase)::acc2)@acc3) [] (contrepetreMots dico mot1 mot2)) else acc2)
                        [] phrase)(*@acc1*))
    [] phrase;;
(*Concatenate acc1 is usefull because that will add in double some sentences already added with acc2 furthermore add thanks to constructor is better*)

let cst_dico1 = [['m';'i';'n';'i';'s';'t';'r';'e'];['s';'i';'n';'i';'s';'t';'r';'e'];['m';'e';'c';'h';'e'];['s';'e';'c';'h';'e'];['t';'a';'r';'e'];['r';'a';'m';'e'];['m';'a';'r';'e'];['r';'a';'t';'e'];['m';'a';'t';'e'];['r';'a';'r';'e']];;

let cst_phrase=[['i';'l'];['y'];['a'];['u';'n';'e'];['r';'a';'m';'e'];[','];['u';'n';'e';];['m';'a';'r';'e'];['e';'t'];['u';'n';'e'];['t';'a';'r';'e']];;

contrepetries cst_dico1 cst_phrase ;;

(* 6 - Optimisaton et extension *)

(* Q17 *)

type 'a ensemble = 'a list;; (* sorted *)

(* We can sort the list as we have done during pratical work *)
let insertion_pos (x:'a) (l:'a list) (n:int) : 'a list = List.fold_left (fun a x1 -> if n=(List.fold_left(fun a x-> a+1) 0 a) then a@[x;x1] else if n-1 = (List.fold_left(fun a x-> a+1) 0 a) then a@[x1;x] else  a@[x1]) [] l;;
insertion_pos 28 [1;5;8;6;7;5] 2;;

let insertion (cmp:'a -> 'a -> int) (x: 'a) (l : 'a list) : 'a list = if l = [] then [x] else insertion_pos x l ( List.fold_left (fun a elt -> if (cmp x elt < 0) then a+1 else a) 0  l);;

let tri_insertion (cmp:'a -> 'a -> int) (l: 'a list) = List.fold_left (fun acc elt -> insertion cmp elt acc) [] l;;

tri_insertion (fun x y -> y-x) [1;8;6;85;4;7;56;95;2;48];; (* y-x > 0 => x<y*)
tri_insertion (fun x y -> (int_of_char y)- (int_of_char x)) ['a';'z';'e';'r';'t';'y'];;
tri_insertion (fun s0 s1 -> String.compare s1 s0) ["Clément";"Mickael";"Hussein abdelmoneim El Fakharany";"Thomas";"Fabien";"Tanguy"];;

(* We can use these function to sort different set *)
let sort_int_increasing (x:int) (y:int) = y-x ;;
let sort_int_decreasing (x:int) (y:int) = x-y ;;
let sort_float_increasing (x:float) (y:float) = y-.x ;;
let sort_float_decreasing (x:float) (y:float) = x-.y ;;
let sort_char_alphabetic (x:char) (y:char) =(int_of_char y)- (int_of_char x);;
let sort_string_alphabetic (so:string) (s1:string) = String.compare s1 s0;;


(* Ensemble efficace *)

let rec appartenance elt ens =
  match ens with
  |[]->false
  |a::b->if a=elt then true else if elt>a then false else appartenance elt b;;

(* inclusion is optimised thanks to the appartenance optimisation *)
let rec inclusion ens1 ens2 =
  match ens1 with
  |[]->true
  |a::b->if appartenance a ens2 then inclusion b ens2 else false;;

(* ajout must add the element at the right place to return a sorted list *)
let ajout elt ens f = insertion f elt ens;;

let rec suppression elt ens =
  match ens with
  |[]->ens
  |a::b->if a=elt then b else if elt >a then (a::b) else Ce(a,suppression elt b);;

(* intersection is optimised thanks to the appartenance optimisation *)
let rec intersection ens1 ens2 =
  match ens1 with
  |[]->[]
  |a::b->if appartenance a ens2 then Ce(a,intersection b ens2)else intersection b ens2;;


(* Multi-ensemble efficaces *)
type 'a multielement = 'a *int;;
type 'a multiensemble='a multielement ensemble;; (* sorted *)

let rec appartenance melt mens =
  match mens with
  |[]->false
  |(a,b)::ens->let (c,d)=melt in if (a=c) && (b>=d) then true else if c<a then false else appartenance melt ens;;

let rec occurence x mens =
  match mens with
  |[]-> 0
  |(a,b)::ens->if x<a then 0 else if a=x then b else occurence x ens ;;

let sort_int_increasing (x:int*int) (y:int*int) =let (x0,_)=x and (y0,_)=y in y0-x0 ;;
let sort_int_decreasing (x:int*int) (y:int*int) =let (x0,_)=x and (y0,_)=y in x0 - y0 ;;
let sort_float_increasing (x:float*int) (y:float*int) =let (x0,_)=x and (y0,_)=y in y0-.x0 ;;
let sort_float_decreasing (x:float*int) (y:float*int) =let (x0,_)=x and (y0,_)=y in x0-.y0 ;;
let sort_char_alphabetic (x:char*int) (y:char*int) =let (x0,_)=x and (y0,_)=y in (int_of_char y0)- (int_of_char x0);;
let sort_string_alphabetic (s0:string*int) (s1:string*int) = let (x0,_)=s0 and (x1,_)=s1 in String.compare x1 x0;;

let ajout (melt:'a multielement) (mens:'a multiensemble) (fun_sort: 'a multielement -> 'a multielement -> int) = insertion fun_sort melt mens;;

let rec suppression melt mens =
  match mens with
  |[]->mens
  |(a,b)::ens->let (c,d)=melt in if a=c then if (b-d)<=0 then ens else (a,b-d)::ens else  if c<a then (a,b)::ens else(a,b)::suppression melt ens;;

(* intersection is optimised thanks to appartenance and occurence optimisation *)

let intersection mens1 mens2 = List.map (fun (a,_) -> if  occurence a mens1 > occurence a mens2
                                                          then (a,occurence a mens2)
                                                          else (a,occurence a mens1)) (List.filter (fun (a,_) -> appartenance (a,1) mens2) mens1);;

let rec union mens1 mens2 fun_sort:'a multiensemble =
  match mens1 with
  |[]->mens2
  |(a,b)::_->match mens2 with
            |[]->mens1 
            |(c,d)::ens->if a=c then (a,b+d)::(union (suppression (a,b) mens1) ens fun_sort)
                            else ajout (c,d) (union mens1 ens fun_sort) fun_sort;;

union [('j', 1); ('m', 1); ('o', 2)] [('a', 1); ('c', 3); ('l', 4); ('m', 2); ('o', 5)] sort_char_alphabetic;;
union [('a', 1); ('c', 3); ('l', 4); ('m', 2); ('o', 5)] [('j', 1); ('m', 1); ('o', 2)] sort_char_alphabetic;;

(* difference is optimised thanks to appartenance and suppression optimisation *)
let rec difference mens1 mens2 =
  match mens1 with
  |[]->[]
  |(a,b)::ens->if appartenance (a,b) mens2 then difference (suppression (a,b) mens1) mens2 else (a,b)::difference ens mens2;;

(* 6.2 Dictionnaires "efficaces" *)

type 'a arbre = | Vide
             | Noeuds of ('a arbre * 'a * 'a arbre);;

type dictionnaireEff = mot arbre;;

let miniLettre (c:char) : char = if c>='A' && c<='Z' then char_of_int ((int_of_char c)-((int_of_char 'A') - (int_of_char 'a'))) else c;;

let rec compareMot (m1:mot) (m2:mot) : int = match m1 with
  |[] -> if m2=[] then 0 else -1
  |l1::r1 -> match m2 with
             |[] -> 1
             |l2::r2 -> if (miniLettre l1)=(miniLettre l2) then compareMot r1 r2 else (int_of_char (miniLettre l1)) - (int_of_char (miniLettre l2));;

compareMot ['G';'o'] ['a';'l';'l';'e';'z'];;
compareMot ['A';'l';'l';'e'] ['A';'l';'l';'e';'z'];;

let rec insert (a :dictionnaireEff) (m:mot):dictionnaireEff = match a with
  |Vide -> Noeuds(Vide,m,Vide)
  |Noeuds(fg,p,fd) -> if (compareMot m p)>0 then Noeuds(fg,p,insert fd m)
                                        else Noeuds(insert fg m,p,fd);;

insert (Noeuds(Vide,['G';'o'],Vide)) ['a';'l';'l';'e';'z'];;

(* For contrepetries appartenance modification is the only modification necessary because is the only scanning the "dictionnaire" *)

let rec  appartDicoEff (elt:mot) (dico:dictionnaireEff) = match dico with
  |Vide -> false
  |Noeuds(fg,m,fd) -> if m=elt then true else if (compareMot elt m)>0 then (appartDicoEff elt fd) else (appartDicoEff elt fg) ;;

let echange (p1,l1,s1) (p2,l2,s2) =
  (p1@(l2::s1),p2@(l1::s2));;

let contrepetreMots (dico:dictionnaireEff) (m1:mot) (m2:mot) : (mot*mot) list=
  let dec_m1 = decompose m1 and dec_m2 = decompose m2 in
  List.fold_left
    (fun acc1 dec_mot1 ->
        acc1@List.fold_left
               (fun acc2 dec_mot2 -> let (p1,l1,s1) = dec_mot1 and (p2,l2,s2)= dec_mot2 in if let(mot1,mot2)=echange (p1,l1,s1) (p2,l2,s2) in (appartDicoEff mot1 dico && appartDicoEff mot2 dico) && not (mot1=m1 && mot2=m2) then echange (p1,l1,s1) (p2,l2,s2)::acc2 else acc2)
               [] dec_m2 )
    [] dec_m1;;


let echangeMot (motASup1:mot) (motASup2:mot) (motsContrepetries:mot*mot) (phrase:phrase) : phrase = let (nouvMot1,nouvMot2)=motsContrepetries in List.fold_left (fun acc mot -> if (mot=motASup1) then acc@[nouvMot1] else if (mot=motASup2) then acc@[nouvMot2] else acc@[mot]) [] phrase;;

let contrepetriesDicEff (dico:dictionnaireEff) (phrase:phrase) : phrase ensemble =
  List.fold_left
    (fun acc1 mot1 -> (List.fold_left
                         (fun acc2 mot2 ->
                           if not ((contrepetreMots dico mot1 mot2)=[]) then (List.fold_left (fun acc3 motsCtrp -> ((echangeMot mot1 mot2 motsCtrp phrase)::acc2)@acc3) [] (contrepetreMots dico mot1 mot2)) else acc2)
                        [] phrase)(*@acc1*))
    [] phrase;;

let cst_dico1 = [['m';'i';'n';'i';'s';'t';'r';'e'];['s';'i';'n';'i';'s';'t';'r';'e'];['m';'e';'c';'h';'e'];['s';'e';'c';'h';'e'];['t';'a';'r';'e'];['r';'a';'m';'e'];['m';'a';'r';'e'];['r';'a';'t';'e'];['m';'a';'t';'e'];['r';'a';'r';'e']];;

let cst_phrase=[['i';'l'];['y'];['a'];['u';'n';'e'];['r';'a';'m';'e'];[','];['u';'n';'e';];['m';'a';'r';'e'];['e';'t'];['u';'n';'e'];['t';'a';'r';'e']];;

let tree_of_list (l: 'a list) : 'a arbre = List.fold_left ( insert ) (Vide) l;;
tree_of_list cst_dico1;;
contrepetriesDicEff (tree_of_list cst_dico1) cst_phrase ;;

(* 6.3 Couple de lettre *)

type 'a ensemble = 'a list;;

type lettre= char;;
type mot= lettre list;;

type dictionnaire = mot ensemble;;

type phrase = mot list ;;

let appartenance elt ens = List.exists (fun a -> a=elt) ens;;

let cardinalite ens = List.fold_left (fun a b -> a+1) 0 ens;;

let shift_lettre (decomp:mot * lettre list * mot):(mot * lettre list * mot)=
  let (p,l,s)=decomp in (p@(List.hd l)::[],(List.tl l)@(List.hd s)::[],(List.tl s));;

let decomp_prime (mot:mot) : (mot * lettre list * mot) list =
  let lettre1=[List.hd mot] and finMot=List.tl mot in if ((cardinalite mot) = 1) then ([],lettre1,[])::[] else  let lettre1et2=(List.hd mot::List.hd finMot::[]) and reste1et2=List.tl finMot in
  (List.fold_left (fun decompList pls -> let (p,l,s) = (List.hd decompList) in shift_lettre (p,l,s)::decompList) (([],lettre1,finMot)::[]) finMot)@(List.fold_left (fun decompList pls -> let (p,l,s) = (List.hd decompList) in shift_lettre (p,l,s)::decompList) (([],lettre1et2,reste1et2)::[]) reste1et2);;

decomp_prime ['g'];;
decomp_prime ['s';'i';'n'];;

let echange_prime (p1,l1,s1) (p2,l2,s2) =
  (p1@l2@s1),(p2@l1@s2);;

let cst_dicoTEST = [['e';'n';'t';'r';'a'];['f';'l';'o';'u'];['e';'n';'f';'l';'a'];['t';'r';'o';'u'];['f';'l';'e';'u';'r'];['p';'i';'c'];['p';'e';'u';'r'];['f';'l';'i';'c']];;
                 
let contrepetreMots_prime (dico:dictionnaire) (m1:mot) (m2:mot) : (mot*mot) list=
  let dec_m1 = decomp_prime m1 and dec_m2 = decomp_prime m2 in
  List.fold_left
    (fun acc1 dec_mot1 ->
        acc1@List.fold_left
               (fun acc2 dec_mot2 -> let (p1,l1,s1) = dec_mot1 and (p2,l2,s2)= dec_mot2 in if let(mot1,mot2)=echange_prime (p1,l1,s1) (p2,l2,s2) in (appartenance mot1 dico && appartenance mot2 dico) && not (mot1=m1 && mot2=m2) then echange_prime (p1,l1,s1) (p2,l2,s2)::acc2 else acc2)
               [] dec_m2 )
    [] dec_m1;;

contrepetreMots_prime cst_dicoTEST ['e';'n';'t';'r';'a'] ['f';'l';'o';'u'];;
contrepetreMots_prime cst_dicoTEST ['e';'n';'f';'l';'a'] ['t';'r';'o';'u'];;
contrepetreMots_prime cst_dicoTEST ['f';'l';'e';'u';'r'] ['p';'i';'c'];;
contrepetreMots_prime cst_dicoTEST ['p';'e';'u';'r'] ['f';'l';'i';'c'];;

(* contrepetries will work with these new functions who can inverse two letters using decomp_prime instead of decompose and echange_prime instead of echange or allow to pass functions in parameters *)
