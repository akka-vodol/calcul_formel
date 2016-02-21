(* Tokens
+ - * / 
= ( ) ; 
let def derive 
<Nombre> <Identificateur>
*)

type token = 
	| Plus
	| Mult
	| Minus
	| Div
	| Affect
	| OpenPar
	| ClosePar
	| SemiColon
	| NumSep
	| Let
	| Define
	| Derive
	| Exp
	| Log
	| Number of float
	| Ident of string
	| EOF
	| Error
;;

let affToken tok = match tok with
	| Plus -> "Plus"
	| Mult -> "Mult"
	| Minus -> "Minus"
	| Div -> "Div"
	| Affect -> "Affect"
	| OpenPar -> "OpenPar"
	| ClosePar -> "ClosePar"
	| SemiColon -> "SemiColon"
	| NumSep -> "NumSep"
	| Let -> "Let"
	| Define -> "Define"
	| Derive -> "Derive"
	| Exp -> "Exp"
	| Log -> "Log"
	| Number(f) -> string_of_float f
	| Ident(str) -> str
	| EOF -> "EOF"
	| Error -> "Error"
;;

let rec print_list li toStringType = match li with
	| [] -> print_string "\n"
	| x::q -> print_string (toStringType x); print_string " "; print_list q toStringType
;;

let print_strList li = print_string (String.concat "" li);;
let print_bool e = if e then print_string "true" else print_string "false"; print_string "\n";;

let isLetter c = let letters = "abcdefghijklmnopqrstuvwxyz" in (String.contains letters c) || (String.contains (String.uppercase letters) c);;
let isDigit c = let digits = "0123456789" in (String.contains digits c);;
let isChar c = (isLetter c) || (isDigit c);;
let isOp c = let oper = "+-*/" in (String.contains oper c);;
let isSpe c = let spe = " \n\r\t" in (String.contains spe c);;
let isPonct c = let ponc = "()=;" in (String.contains ponc c);;

let toChar i = (string_of_int i).[0];;
let string_of_char c = String.make 1 c;;

let inverse li = 
	let rec aux l accu = match l with
		| [] -> accu
		| x::q -> aux q (x::accu)
	in aux li []
;;

let add el refL = refL := el::(!refL);;

let rec isIn elem li = match li with
	| [] -> false
	| x::q -> (x==elem) || (isIn elem q)
;;


(*********************************************************************************************)
(*Automage lexical *)

type autoLex = {mutable state : int; mutable pile : token list};;

let newAuto = {state = 0; pile = []};;

exception Error of int*string;;

let process lex code = 	
	let n = String.length code in
  	let accIdent = ref [] in
	let accNumber = ref [] in
	let move state = lex.state <- state
 	in
	let ajoute elem = lex.pile <- elem::lex.pile 
	in
	let throw index message = raise (Error(index, message))
	in
	let transferIdent () = 
		let temp = String.concat "" (inverse (!accIdent)) in
		accIdent := [];
		match temp with
			| "" -> ()
			| _ -> ajoute (Ident(temp))
	in
	let transferNumber () = 
		let temp = String.concat "" (inverse (!accNumber)) in
		accNumber := [];
		match temp with
			| "" -> ()
			| _ -> ajoute (Number(float_of_string temp))
	in
	let getOp op pos = match op with
		| '+' -> Plus
		| '/' -> Div
		| '*' -> Mult
		| '-' -> Minus
		| _ -> throw pos "pas un operateur"
	in
	let getPonct pu pos = match pu with
		| '(' -> OpenPar
		| ')' -> ClosePar
		| ';' -> SemiColon
		| '=' -> Affect
		| _ -> throw pos "pas une ponctuation"
	in
	let generSpe = [0;1;2;3;4;5;7;8;9;30;11;12;21;22] in
	let generPu = [0;1;2;3;4;5;7;8;9;30;11;12;21;22] in
	let generOp = [0;1;2;3;4;5;7;8;9;30;11;12;21;22] in
	let generChar = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;21;22;23] in 
	let process lex c pos = match lex.state, c with
		| 0, 'd' -> move 1; add "d" accIdent 
		| 0, 'l' -> move 11; add "l" accIdent
		| 0, 'e' -> move 21; add "e" accIdent
		| 0, x when isDigit x -> move 40; add (string_of_char x) accNumber
		| 1, 'e' -> move 2; add "e" accIdent 
		| 2, 'f' -> move 3; add "f" accIdent
		| 2, 'r' -> move 7; add "r" accIdent
		| 3, 'i' -> move 4; add "i" accIdent
		| 4, 'n' -> move 5; add "n" accIdent
		| 5, 'e' ->move 6; add "e" accIdent
		| 6, spe when (isSpe spe) -> move 0; ajoute Define; accIdent := []
		| 6, pu when (isPonct pu) -> move 0; ajoute Define; accIdent := []; ajoute (getPonct pu pos)
		| 6, op when (isOp op) -> move 0; ajoute Define; accIdent := []; ajoute (getOp op pos)
		| 7, 'i' -> move 8; add "i" accIdent
		| 8, 'v' -> move 9; add "v" accIdent
		| 9, 'e' -> move 10; add "e" accIdent
		| 10, spe when (isSpe spe) -> move 0; ajoute Derive; accIdent := []
		| 10, pu when (isPonct pu) -> move 0; ajoute Derive; accIdent := []; ajoute (getPonct pu pos)	
		| 10, op when (isOp op) -> move 0; ajoute Derive; accIdent := []; ajoute (getOp op pos)
		| 11, 'e' -> move 12; add "e" accIdent
		| 11, 'n' -> move 14; add "n" accIdent
		| 12, 't' -> move 13; add "t" accIdent
		| 13, spe when (isSpe spe) -> move 0; ajoute Let; accIdent := []
		| 13, op when (isOp op) -> move 0; ajoute Let; ajoute (getOp op pos); accIdent := []
		| 13, pu when (isPonct pu) -> move 0; ajoute Let; ajoute (getPonct pu pos); accIdent := []
		| 14, spe when (isSpe spe) -> move 0; ajoute Log; accIdent := []
		| 14, op when (isOp op) -> move 0; ajoute Log; ajoute (getOp op pos); accIdent := []
		| 14, pu when (isPonct pu) -> move 0; ajoute Log; ajoute (getPonct pu pos); accIdent := []
		| 21, 'x' -> move 22; add "x" accIdent
		| 22, 'p' -> move 23; add "p" accIdent
		| 23, spe when (isSpe spe) -> move 0; ajoute Exp; accIdent := []
		| 23, op when (isOp op) -> move 0; ajoute Exp; ajoute (getOp op pos); accIdent := []
		| 23, pu when (isPonct pu) -> move 0; ajoute Exp; ajoute (getPonct pu pos); accIdent := []
		| 30, x when (isChar x) -> add (string_of_char x) accIdent 
		| 40, x when (isDigit x) -> add (string_of_char x) accNumber
		| 40, '.' -> move 41; add "." accNumber
		| 40, x when (isLetter x) -> throw pos "Syntax error"
		| 40, pu when (isPonct pu) -> move 0; transferNumber (); ajoute (getPonct pu pos)
		| 40, op when (isOp op) -> move 0; transferNumber (); ajoute (getOp op pos)
		| 40, spe when (isSpe spe) -> move 0; transferNumber ()
		| 41, x when (isDigit x) ->add (string_of_char x) accNumber
		| 41, pu when (isPonct pu) -> move 0; transferNumber (); ajoute (getPonct pu pos)
		| 41, op when (isOp op) -> move 0; transferNumber (); ajoute (getOp op pos)
		| 41, spe when (isSpe spe) -> move 0; transferNumber ();
		| 41, '.' -> throw pos "Syntax error : too many NumSeps"
		| a, spe when (isSpe spe) && (isIn a generSpe) -> move 0; transferIdent ()
		| a, pu when (isPonct pu) && (isIn a generPu) -> move 0; transferIdent (); ajoute (getPonct pu pos)
		| a, op when (isOp op) && (isIn a generOp) -> move 0; transferIdent (); ajoute (getOp op pos)
		| a, x when (isChar x) && (isIn a generChar) -> move 30; add (string_of_char x) accIdent
		| _,x when (isChar x) || (isPonct x) || (isOp x) || (isSpe x) || (String.contains "." x) -> throw pos "Syntax error"
		| _,_ -> throw pos (String.concat " : " ["Caractere inexistant"; string_of_char c])
	in 
	for i = 0 to (n-1) do
		process lex (code.[i]) i
	done;
	lex.pile <- inverse (lex.pile)
;;

let code = "define pi = ln(3.14.12) + exp(3.1\n * zeta) ";;
process newAuto code;;
print_list (newAuto.pile) affToken;;
