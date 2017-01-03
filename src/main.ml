(*
	PROJET : parser de partitions

	Le projet est incrémental : on propose plusieurs objectifs de plus en plus technique et difficile.
	C'est un guide d'évaluation mais il vous est possible de sauter certain objectif pour s'attaquer aux suivants.

	- 10/20 : transformation de partition en colonne vers une liste triée de sons datés

	- 12/20 : précédent + gestion des fractions de tempo
						 (noire "1" = 1 temps, blanche "1o"=2 temps, ronde "1@" = 4 temps,  croche "."=1/2 temps,
						 double croche ":" = 1/4 de temps, triolet "^" = 1/3 de temps)

	- 14/20 : précédent + transformation de la liste triée de sons datés en une liste d'accords datés
						 au lieu de  [ (1.0,"poum") ; (1.0, "wizz") ; (1.2, "crack") ] on regoupe les sons qui doivent
						 être joués simultanément. On retourne [ (1.0, ["poum";"wizz"]) ; (1.2, ["crack"]) ]

	- 16/20 : précédent + gestion des données longues entre accolades {}

	- 18/20 : précédent + gestion des répétitions  * nombre [ mesures entre crochets ]

	- 20/20 : précédent + player (graphique) après parsing de la liste triées des accords (façon karaoké)
*)

(* DEBUGGING *)
(** Module parseur de partition *)

(** variable de débuggage *)
let _DEBUG_ = true ;;


(** Si _DEBUG_ est à true, la fonction message affiche la String passé en paramètre *)
let (message: string -> unit) = fun string -> if _DEBUG_ then print_string string else ();;

(* PART 1 -GIVEN- Example of sheet *)

(** Partition exemple à parser *)
let row_sheet_given =
"R:                                                                                                                                                                                                                                                                                                                                       *4                                                                  |
T:|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12 .  3         4            5          6       7               8    |12.  3          4        5             6    7         8         |12.     3        4          5           6         7        8        |1      23      45      6          7        8      [123456          7        8      ]12345  6          7        8      |
V:|        |        |        |        |        |        |        |        |   {I}{know you}{don't get a}{chance to}{take a}{break this of-}{ten}|  {I}{know your}{life is}{speeding and}{any}{thing is}{stopping}|  {here}{take my}{shirt and}{just go a-}{head and}{wipe up}{all the}|{sweat} {sweat} {sweat}{Lose your}{self to}{dance}[     {Lose your}{self to}{dance}]       {Lose your}{self to}{dance}|
D:|bBbBbBbB|bBbBbBbB|bBbBbBbB|bBbBbBbB|bBbBbBbB|bBbBbBbB|bBbBbBbB|bBbBbBbB|bB    b         B            b          B       b               B    |bB   b          B        b             B    b         B         |bB      b        B          b           B         b        B        |c      Bc      Bc      B          b        B      [bBbBbB          b        B      ]cBcBc  B          b        B      |
G:|        |        |gggggggg|gggggggg|gggggggg|gggggggg|gggggggg|gggggggg|gg                                                                   |                                                                |                                                                    |                                                  [                                ]                                  |
J:|        |        |        |        |        |        |        |        |                                                                     |                                                                |                                                                    |                                                  [                                ]                                  |
";;

(*CHANGED: afin de pouvoir tester tous ce que nous avons implémenté, voila la partition que nous avons utilisé. *)
let row_sheet =
"R:                                                                                                                                                                                       *4                                                                  |
T:|1234@8|123o567o|1:::234:::5678|1^^23^^45^^67^^8|12 .  3         4            5          6       7               8    |12.  3          4        5             6    7         8         [123456          7        8      ]12345  6          7        8      |
V:|      |        |              |                |   {I}{know you}{don't get a}{chance to}{take a}{break this of-}{ten}|  {I}{know your}{life is}{speeding and}{any}{thing is}{stopping}[     {Lose your}{self to}{dance}]       {Lose your}{self to}{dance}|
D:|bBbB  |bB  bB  |b   BbB   bBbB|b  Bb  Bb  Bb  B|bB    b         B            b          B       b               B    |bB   b          B        b             B    b         B         [bBbBbB          b        B      ]cBcBc  B          b        B      |
G:|g  g g|  g   g |gggg  gggg    |ggg ggg ggg ggg |gg g                                                                 |ggg                                                             [                                ]                                  |
J:|      |        |              |                |                                                                     |                                                                [                                ]                                  |
";;



(* PART 2 -GIVEN- Auxilliary functions on list and string *)

(* 2.1 OPERATIONS on LIST *)

(** Module contenant des opérations pour la manipulation de liste *)
module MyList =
	(struct
		(** Retourne les n premier élements d'un liste *)
		let (first: int -> 't list -> 't list) =
			let rec (first_rec: int -> 't list -> 't list) = fun n ts ->
				match ts with
				| [] -> []
				| t::ts -> if n>0 then t::(first_rec (n-1) ts) else []
			in
				fun n ts -> first_rec n ts

		(** Renvoie une liste de liste crée à partir d'une liste et d'une liste de liste. Le n ème de la liste est
		concaténé à la n ème liste dans la liste de liste. Si la liste de liste ne possède pas ne nème element, alors
		elle est considéré comme étant la liste vide. *)
		let rec (safe_zipper: 't -> 't list -> ('t list) list -> ('t list) list) = fun default ts ls ->
			match ts,ls with
			| [],[] -> []
			| [], l::ls -> l::ls (* (default::l) :: (safe_zipper default [] ls) *)
			| t::ts, [] -> (t::[]) :: (safe_zipper default ts [])
			| t::ts, l::ls -> (t::l) :: (safe_zipper default ts ls)


		(** Type définissant l'ensemble des fonctions qui associe à une liste et une liste de liste une liste de liste *)
		type 't zipper = 't list -> ('t list) list -> ('t list) list

		(** Crée une liste de liste en appliquant récursivement une fonction zipper au premier élement d'une liste de
		liste et zip_with du reste de la liste  *)
		let rec (zip_with: 't zipper -> ('t list) list -> ('t list) list) = fun zipper ->
			function
			| [] -> []
			| ts::others -> zipper ts (zip_with zipper others)

		(** Associé à une liste d'élement du type 'x une liste de type 'y en appliquant à chaque élement une fonction
		de conversion passé en paramètre transformant un 'x en une liste de 'y *)
		let rec (foreach: 'x list -> ('x -> 'y list) -> 'y list) = fun xs f ->
			match xs with
			| [] -> []
			| x::others -> (f x) @ (foreach others f)
	end)

(* 2.2 OPERATIONS on STRING: transposition from lines to columns **)

(** Module contenant des opérations pour la manipulation de chaine de caractères *)
module MyString =
	(struct
		(** liste de char *)
		type line = char list

		(** liste de char *)
		type column = char list

		(** fold f a string est équivalent à f (... (f (f a b1) b2) ...) bn avec b1, b2 bn les charactère de la chaine string*)
		let (fold: ('o -> char -> 'o) -> 'o -> string -> 'o) = fun update default string ->
			let rec (fold_rec: int -> 'o -> 'o) = fun i output ->
				try
					let char = String.get string i
					in let updated_output = update output char
					in fold_rec (i+1) updated_output
				with _ -> output
			in
				fold_rec 0 default

		(** transforme une string en liste de line *)
		let (to_line_list: string -> line list) = fun string ->
			let lines =
				match fold (fun (line,lines) char ->
					if (char = '\n') then
						([],(List.rev line)::lines)
					else
						(char::line,lines)) ([],[]) string
				with
				| ([],[]) -> []
				| ([],lines) -> lines
				| (line,lines) -> line::lines
			in
				(List.rev lines)

		(** Transpose une liste de chaine de charactère en ligne sous la forme de liste de chaine de charactère en colonnes *)
		let (transpose: line list -> column list) = fun lines ->
			MyList.zip_with (MyList.safe_zipper '%') lines

		(** transforme une char list en String *)
		let (char_list_to_string: char list -> string) = fun chars ->
			let string = Bytes.create (List.length chars)
			in let rec (char_list_to_string_rec: int -> char list -> string) = fun i ->
				function
				| [] -> string
				| c::cs -> begin Bytes.set string i c ; char_list_to_string_rec (i+1) cs end
			in
				char_list_to_string_rec 0 chars

	end)


(* PART 3 -GIVEN- Labelling of automata transitions *)

(* 3.1 PATTERN instead of SYMBOL on transition **)
(** Définition du type pattern *)
type 'a pattern =
	| ANY
	| KEY of 'a
	| BUT of 'a
	| IN  of 'a list
	| OUT of 'a list

(** Module contenant des opérations pour la manipulation du type pattern *)
module Pattern =
	(struct
		(** Type non utilisé *)
		type 'a t = 'a pattern

		(** Retourne true si le pattern est du type KEY et que la valeur du pattern à la valeur a *)
		let (exactly: 'a pattern -> 'a -> bool) = fun pattern a ->
			pattern = KEY a

		(** Prend un pattern de type 'a et une variable de type 'a et renvoie :
			true si le pattern est du type ANY
			true si le pattern est du type KEY et que la valeur du patern vaut 'a
			true si le pattern est du type BUT et que la valeur du pattern est différente de 'a
			true si le pattern est du type IN et que 'a est dans la liste des valeurs du pattern
			true si le pattern est du type OUT et que 'a n'est pas dans la liste des valeurs du pattern
			false sinon *)
		let (matches: 'a pattern -> 'a -> bool) = fun pattern a' ->
			match pattern with
			| ANY -> true
			| KEY a -> a = a'
			| BUT a -> a <> a'
			| IN  aS -> List.mem a' aS
			| OUT aS -> not (List.mem a' aS)

		(** Retourne une chaine de caractère formée en appliquant la fonction passé en paramètre à la valeur du pattern
		passé en paramètre *)
		let (pretty: ('a -> string) -> 'a pattern -> string) = fun pp pattern ->
			match pattern with
			| ANY -> "_"
			| KEY a -> pp a
			| BUT a -> "~" ^ (pp a)
			| IN  aS -> "{" ^ (String.concat "," (List.map pp aS)) ^ "}"
			| OUT aS -> "~{" ^ (String.concat "," (List.map pp aS)) ^ "}"

		(** affiche dans stdout la chaine retournée par la fonction pretty avec comme paramètre la fonction et le
		pattern donnée *)
		let (print: ('a -> string) -> 'a pattern -> unit) = fun pp pattern ->
			print_string (pretty pp pattern)
	end)



(* PART 4 -GIVEN- The output of a parser is a SoundTrack = sequence of timed sound *)

(* 3.1 Clock **)

(** Définition du type clock permettant de définir des dates ou des durées *)
type clock = float

(** Module contenant des opérations pour la manipulation du type clock *)
module Clock =
	(struct
		type date     = float
		type duration = float
		let (initial: date) = 0.0
		let (shift_by: duration -> date -> date) = fun du da -> da +. du

		(* CHANGED: function next_full *)
		(** change la date passé en paramètre et la décale pour obtenir la procahaine date pleine *)
		let (next_full: date -> date) = fun da -> floor (da +. 1.)
	end)


(* 3.2 Sound, SoundTrack, Sound In Construction **)

(** Définition du type sound permettant de définir un son lu sur la partition *)
type sound = string

(** Définition du type timed_sound associant une clock(date) à un son *)
type timed_sound = clock * sound

(** Définition du type séquence. Une séquence est une liste *)
type 't sequence = 't list

(** Définition du type sountrack comme étant une séquence de timed_sound *)
type soundtrack = timed_sound sequence

(* CHANGED: définition de 3 nouveaux types pour la soundtrack finale *)
(** Définition du type sounds comme étant une liste de son *)
type sounds = sound sequence

(** Définition du type timed_sounds associant une clock(date) à des sons *)
type timed_sounds = clock * sounds

(** Définition du type sountracks comme étant une séquence de timed_sounds *)
type soundtracks = (clock * string sequence) sequence


(** Définition du type sic représentant un son en construction. Il peut être Nul ou être une liste de string associé à
une clock(date) *)
type sic = (* Sound In Construction *)
	| Nil
	| Sic of (clock * string list)

(** Module contenant des opérations pour la manipulation du type sic *)
module Sound =
	(struct

		(* CHANGED: Renverse la liste dans sic avant de la concatener pour bien retrouver l'ordre de lecture *)
		(** Retourne une soundtrack à partir d'un sic *)
		let (finalize: sic -> soundtrack) = fun sic ->
			match sic with
			| Sic (clock,strings) -> [ (clock, String.concat "" (List.rev strings)) ]
			| Nil                 -> [ ]

		(** Etend un son en construction avec une nouvelle string et une clock(durée) *)
		let (extend_with: string -> clock -> sic -> sic) = fun string current_clock sic ->
			match sic with
			| Sic (clock,strings) -> Sic (clock, string::strings)
			| Nil                 -> Sic (current_clock, [string])
	end)


(* 3.3 Data stored by each parsing process **)

(** Définition du type data, un type composé d'une clock(date), d'un son en construction et d'une soundtrack *)
type data = { clock:clock ; sound: sic ; soundtrack: soundtrack }

(** Module contenant des opérations pour la manipulation du type data *)
module Data =
	(struct
		(** Initialise une variable data *)
		let (initial: data) = { clock = Clock.initial ; sound = Nil ; soundtrack = [] }

		(** Finalise une variable data en ajoutant le son en construction à la soundtrack puis en le détruisant *)
		let (finalize: data -> data) =
			fun data ->
			{
				data with
				sound = Nil;
				soundtrack = (Sound.finalize data.sound) @ data.soundtrack
			}

		(** Retourne la clock contenue dans une data *)
		let (get_clock: data -> clock) = fun data ->
			data.clock

		(** Retourne la soundtrack contenu dans une data finalisé *)
		let (get_soundtrack: data -> soundtrack) = fun data -> ( finalize data ).soundtrack

		(** met à jour la clock d'une data *)
		let (update_clock: clock -> data -> data) = fun clock data ->
			{ data with clock = clock }
	end)


(* 3.4 Symbol: The automata read a sequence of symbol where **)

(** type définissant un symbole lu sur la partition *)
type symbol = string

(* 3.5 Actions on data **)

(** Définition du type action représentant l'ensemble des fonctions prennant en paramètre un triplet du type clock, data,
 symbol et retournant une data *)
type action = (clock * data * symbol) -> data

(** Module contenant des opérations pour la manipulation du type action *)
module Action =
	(struct

		(* USAGE: (increase_clock_by 1.0)  or (increase_clock_by 0.5)  *)
		(** Crée l'action d'incrémentation de la clock par la durée passé en paramètre *)
		let (increase_clock_by: Clock.duration -> action) = fun duration ->
			fun (clock,data,symbol) ->
				{ data with clock = Clock.shift_by duration clock }

		(* USAGE:  increase_clock_to_next_full() *)
		(* CHANGED: function increase_clock_to_next_full*)
		(** Crée l'action d'incrémentation de la clock pour la placer sur le prochain temps plein *)
		let (increase_clock_to_next_full: action) =
			fun (clock,data,symbol) ->
				{ data with clock = Clock.next_full clock }

		(* CHANGED: Si la String est "%", c'est le symbole qu'il faut rajouter dans le son*)
		(* USAGE: (extend_sound_with "Tchack" ) *)
		(** Crée l'action d'extension du son avec un string donnée *)
		let (extend_sound_with: string -> action) = fun string ->
			fun (clock,data,symbol) ->
				if (string = "%") then
					{ data with sound = Sound.extend_with symbol clock data.sound }
				else
					{ data with sound = Sound.extend_with string clock data.sound }

		(* USAGE: finalize_sound *)
		(** Crée l'action de finalisation du son *)
		let (finalize_sound: action) =
			fun (clock,data,symbol) ->
				Data.finalize data

		(* USAGE: (make_sound "Poum" ) *)
		(* CHANGED: function Action.make_sound *)
		(* CHANGED: Si la String est "%", c'est le symbole qu'il faut rajouter dans le son*)
		(** Crée l'action de création d'un son avec un string donnée *)
		let (make_sound: string -> action) = fun string ->
			fun (clock,data,symbol) ->
				if (string = "%") then
					{
						data with sound = Nil;
						soundtrack = (Sound.finalize (Sound.extend_with symbol clock data.sound)) @ data.soundtrack
					}
				else
					{
						data with sound = Nil;
						soundtrack = (Sound.finalize (Sound.extend_with string clock data.sound)) @ data.soundtrack
					}

		(* DEFAULT ACTION: update_clock *)
		(* CHANGED: function Action.update_clock *)
		(** Crée l'action de mise à jour d'une clock avec une autre clock passée en paramètre *)
		let (update_clock: action) =
			fun (clock,data,symbol) ->
				{ data with clock = clock }

		(** Crée une action comme étant l'application de la séquence d'action passé en paramètre *)
		let rec (apply_sequence_of: action sequence -> action) = fun actions ->
			fun (clock,data,symbol) ->
				let data = update_clock (clock,data,symbol) in (* /!\ Il faut commencer par mettre à jour la clock *)
				match actions with
				| [] -> data
				| action::other_actions ->
					let data'= action (clock,data,symbol)
					in let clock' = Data.get_clock data'
					in apply_sequence_of other_actions (clock',data',symbol)
	end)



(* PART 4 - Automaton engine *)

(** Définition de l'epsilon pour la transition epsilon *)
let epsilon = ""

(** Définition du type inputs comme étant une list de symbol *)
type inputs = symbol sequence

(** Définition du type node comme étant un entier identifiant un état *)
type node = int

(** Définition du type transition comme étant la transition entre deux nodes *)
type transition = node * label * action sequence * node

(** Définition du type label comme étant un pattern du type symbol *)
and label = symbol pattern

(** Définition du type automaton définit comme étant un nom et une listre de transition *)
type automaton = { name: string ; transitions: transition list }

(** Module contenant des opérations pour la manipulation des automates *)
module Automaton =
	(struct
		(** Node initial (le node 1) *)
		let (initial_node: node) = 1

		(** Crée un automate à partir d'un string et d'une liste de transition *)
		let (make: string -> transition list -> automaton) = fun name transitions ->
			{ name = name ; transitions = transitions }

		(* CHANGED: function Automaton.get_transition_on *)
		(** Retourne la transition qui doit être utilsé dans le cas de la lecture du symbole par l'automate se trouvant dans un état donnée *)
		let (get_transition_on: symbol -> automaton -> node -> transition option) = fun symbol automaton current_node ->
			let enabled_transition = List.filter (fun aTransition -> let (wNode,wLabel,_,_) = aTransition in wNode=current_node && Pattern.matches wLabel symbol) automaton.transitions
			in match enabled_transition with
			| [] -> None
			| wTransition::wQ -> Some wTransition
			(* Récupère les symboles pattern, Utilise la fonction Pattern.matches avec le symbole && vérifie curent_node pour vérifier si transition ok *)
			(* Utilise List.filter predicatList list -> liste
			Si 0 liste, retourne none
			Si 1 liste retourné, retourne la liste
			Si n listes retournés, retourne la tête
			 *)

		(** Transforme une liste d'automate en une liste de couple nom*automate représentant un automate associé à son nom *)
		let (install: automaton list -> (string * automaton) list) = fun automata ->
			List.map (fun aut -> (aut.name,aut)) automata

		(** Retourne l'automate associé à un nom passé en paramètre en faisant la recher dans une listre de couple nom*automate *)
		let (named: string -> (string * automaton) list -> automaton) = fun name automata ->
			List.assoc name automata
	end)


(* PART 5 - The automata *)

(* 5.1 the default automaton **)
(** automate par défault  *)
let adef = Automaton.make "Default"
		[ (1, ANY , [ Action.update_clock ], 1) ] ;;

(* 5.2 Line [T] the tempo parser **)
(** liste des chiffres sous forme de caractères *)
let digit = List.map string_of_int [0;1;2;3;4;5;6;7;8;9] ;;


(* CHANGED: Rajout de la gestion des rondes, blanches, croches, doubles et triolets *)
(** Automate parsant le tempo *)
let atempo = Automaton.make "Tempo"
		[
			(1, KEY "T"              , [                                    ] , 2);
			(1, ANY                  , [                                    ] , 3);
			(2, KEY ":"              , [                                    ] , 4);
			(2, ANY                  , [                                    ] , 3);

			(4, KEY "@"              , [ Action.increase_clock_by 3.0       ] , 4);
			(4, KEY "o"              , [ Action.increase_clock_by 1.0       ] , 4);
			(4, IN digit             , [ Action.increase_clock_to_next_full ] , 4);
			(4, KEY "."              , [ Action.increase_clock_by 0.5       ] , 4);
			(4, KEY ":"              , [ Action.increase_clock_by 0.25      ] , 4);
			(4, KEY "^"              , [ Action.increase_clock_by 0.3       ] , 4);
			(4, IN ["|";"[";"]";" "] , [                                    ] , 4);
			(4, ANY                  , [                                    ] , 3)
		]
	;;

(* CHANGED: automate guitare *)
(** Automate parsant la guitare *)
let aguitar = Automaton.make "Guitar"
	[
		(1, KEY "G"              , [                          ] , 2);
		(1, ANY                  , [                          ] , 3);
		(2, KEY ":"              , [                          ] , 4);
		(2, ANY                  , [                          ] , 3);

		(4, KEY "g"              , [ Action.make_sound "Grat" ] , 4);
		(4, IN ["|";"[";"]";" "] , [ Action.finalize_sound    ] , 4);
		(4, ANY                  , [                          ] , 3)
	]
	;;

(* CHANGED: automate voix *)
(** Automate parsant la voix *)
let avoice = Automaton.make "Voice"
	[
		(1, KEY "V"              , [                              ] , 2);
		(1, ANY                  , [                              ] , 3);
		(2, KEY ":"              , [                              ] , 4);
		(2, ANY                  , [                              ] , 3);

		(4, KEY "{"              , [                              ], 5);
		(5, KEY "}"              , [ Action.finalize_sound        ], 4);
		(5, ANY                  , [ Action.extend_sound_with "%" ], 5);
		(4, IN ["|";"[";"]";" "] , [ Action.finalize_sound        ], 4);
		(4, ANY                  , [                              ], 3)
	]
	;;

(* 5.3 Line [D] the drum parser **)
(** Automate parsant la batterie *)
let adrum = Automaton.make "Drum"
		[
			(1, KEY "D" , [                                ] , 2);
			(1, ANY     , [                                ] , 3);
			(2, KEY ":" , [                                ] , 4);
			(2, ANY     , [                                ] , 3);

			(4, KEY "b"          , [ Action.extend_sound_with "Tchak" ], 4);
			(4, KEY "c"          , [ Action.make_sound "Crack"        ], 4);
			(4, KEY "B"          , [ Action.make_sound "Poum"         ], 4);
			(4, KEY " "          , [                                  ], 4);
			(4, IN ["|";"[";"]"] , [ Action.finalize_sound            ], 4);
			(4, ANY              , [                                  ], 3)
		]
	;;

(** 5.4 The repository of automata **)
(** Liste des automates associés à leurs nom *)
let _AUTOMATA_ = Automaton.install [ adef ; atempo ; adrum ; aguitar ; avoice ] ;


(* PART 6 - The sheet parser *)

(* 6.1 STATE: a state of the execution of an automaton is defined bu the current node and the data under construction **)
(** Définition du type state définit comme étant un node et la donnée du son en construction *)
type state = { node: node ; data: data }

(** Module contenant des opérations pour la manipulation du type state *)
module State =
	(struct

		(* initialize *)
		(** Création d'un state avec les donnée initiales *)
		let (initial: state) = { node = Automaton.initial_node ; data = Data.initial }

		(* get / update *)
		(** Retourne la clock d'un state *)
		let (get_clock: state -> clock) = fun state ->
			Data.get_clock state.data

		(** Récupère la sountrack contenu dans la data contenu dans le state *)
		let (get_soundtrack: state -> soundtrack) = fun state ->
			Data.get_soundtrack state.data

		(** Récupère la sountrack d'un état *)
		let (update_clock: clock -> state -> state) = fun clock state ->
			{ state with data = Data.update_clock clock state.data }

		(** Met à jour un état en changeant le node courant et en appliquant une séquence d'action *)
		let (update: (clock * state) -> (symbol * action sequence * node) -> state) = fun (clock,state) (symbol,actions,target_node) ->
			{
				node = target_node ;
				data = Action.apply_sequence_of actions (clock,state.data,symbol)
			}
	end)


(* 6.2 Process : a process is a running automatond with a current state **)

(** Définition du type process définit comme étant un automate et un état *)
type process = { automaton: string ; state: state  }

(** Module contenant des opérations pour la manipulation de process *)
module Process =
	(struct

		(* get / update *)
		(** Récupère la clock du state contenu dans un process *)
		let (get_clock: process -> clock)   = fun process -> State.get_clock process.state

		(** Récupère la soundtrack du state contenu dans un process *)
		let (get_soundtrack: process -> soundtrack) = fun process -> State.get_soundtrack process.state

		(** Mise à jour de la clock de l'état du process *)
		let (update_clock: clock -> process -> process) = fun clock process ->
			{ process with state = State.update_clock clock process.state }

		(* initialize *)

		(** Initialise un process avec un automate *)
		let (initialize: automaton -> process) = fun automaton -> { automaton = automaton.name ; state = State.initial }

		(* CHANGED: function Process.one_step_on *)
		(** Avance d'un état dans un process. Récupère la transition, créer un action à partir de cettee transition et l'execute. *)
		let (one_step_on: symbol -> (clock * process) -> (clock * process)) = fun symbol (clock,process) ->
			let wAutomate = Automaton.named process.automaton _AUTOMATA_
			in let wTransition = Automaton.get_transition_on symbol wAutomate (process.state.node)
			in match wTransition with
			| None -> (clock, process)
			| Some (wOldNode, _, wActions, wNewNode) ->
				let wNewState = State.update (clock, (process.state)) (symbol,wActions,wNewNode)
				in let wNewClock = State.get_clock wNewState
				and wNewProcess = {process with state = wNewState}
				in (wNewClock, wNewProcess)

		(* CHANGED: Rajout du cas _ dans le match *)
		(* GIVEN: one step each in parallel *)
		(** Avance d'un état dans un ensemble de process *)
		let (one_step_each_process: symbol list -> (clock * process list) -> (clock * process list)) =
			let rec (one_step_each_tailrec: (clock * process list) -> (symbol list * process list) -> (clock * process list)) = fun (clock, moved_process) (more_symbols,more_process) ->
				match (more_symbols, more_process) with
				| [], [] -> (clock , List.rev (List.map (update_clock clock) moved_process))
				| symbol::other_symbols, process::other_process ->
					let  (clock', process') = one_step_on symbol (clock,process)
					in  one_step_each_tailrec (clock' , process' :: moved_process) (other_symbols,other_process)
				| _ -> failwith "Error during the step of this process"
			in
				fun  symbols (clock,processus) -> one_step_each_tailrec (clock,[]) (symbols,processus)

	end)


(* 6.3 SheetParser:
	-	a sheet parser is a list of processus. Each process is specialized to analyse one line of the sheet.
		All processus run simultaneously and in synchronization.
	-	a run is defined by the reference clock for all process, the list of process, the remainder of the sheet to read
		when the sheet comes to its end, the soundtracks is filled with the soundtrack of each processus
*)

(** Définition du type frame comme étant une liste de char *)
type frame = char list
(** Définition du type sheet comme étant une liste de frame. Une sheet est une partition. *)
type sheet = frame list

(** Définition du type run comme étant une clock(date), une liste de process, une sheet et une soundtracks *)
type run = { clock: clock ; processus: process list ; sheet: sheet ; soundtracks: soundtracks }

(** Module contenant des opérations pour parser une sheet *)
module SheetParser =
	(struct

		(* initialize *)
		(** Initialise une variable de type run *)
		let (initialize: automaton list -> sheet -> run) = fun automata sheet ->
			{
				clock = Clock.initial ;
				processus = List.map Process.initialize automata ;
				sheet = sheet ;
				soundtracks = []
		}

		(* CHANGED: function SheetParser.one_step *)
		(** Avance d'un étape dans chaque process. Si la sheet dans le run est vide, finalise soundtracks en y mettant chaque soundtrack contenu dans les process *)
		let (one_step: run -> run) = fun run ->
			match run.sheet with
			| [] -> { run with soundtracks =
						(let rec (insert: soundtracks -> timed_sound -> soundtracks ) = fun aSoundTracks aTimed_sound ->
							let (wClockInsert, wSoundInsert) = aTimed_sound
							in match aSoundTracks with
							| [] -> [(wClockInsert, [wSoundInsert])]
							| (wClock, wSound)::wQ ->
								if wClockInsert = wClock then
									(wClock, (wSound@[wSoundInsert]))::wQ
								else if wClockInsert > wClock then
									(wClock, wSound)::(insert wQ aTimed_sound)
								else
									(wClockInsert, [wSoundInsert])::(wClock, wSound)::wQ
						in
							List.fold_left
								(List.fold_left
									insert
								)
								[]
								(List.map (fun process -> Process.get_soundtrack process) run.processus)
						)
					}
			| frame::sheet' ->
				let symbols = List.map (String.make 1) frame
				in let (clock', processus') = Process.one_step_each_process symbols (run.clock, run.processus)
				in
					{ run with clock = clock' ; processus = processus' ; sheet = sheet' }
	end)


(* PART 7 - Demo *)

(* 7.1 Using imperative feature to run the parser STEP BY STEP **)
(** Utilisation de l'impératif pour la lecture de la partition *)
let _RUN = ref { clock = Clock.initial ; processus = [] ; sheet = [] ; soundtracks = [] } ;;

(** Initialise le parser en utilisant la fonction initialise du module SheetParser *)
let (initialize: automaton list -> sheet -> run) = fun automata sheet ->
	begin
		_RUN := SheetParser.initialize automata sheet ;
		!(_RUN)
	end

(** Avance d'un pas dans la lecture de la partition *)
let (one_step: unit -> run) = fun () ->
	begin
		_RUN := SheetParser.one_step !(_RUN) ;
		!(_RUN)
	end
(** Avance de 100 pas dans la lecture de la partition *)
let (n_step: int -> run) = fun aNumberOfSteps ->
	begin
		for i=1 to aNumberOfSteps do
			one_step();
		done;
		!(_RUN)
	end

let (get_soundtracks: unit -> soundtracks) = fun () ->
	!(_RUN).soundtracks

(* 7.2 The sheet parser is a list of processes that run some automata **)
(** Crée une curryfication de la fonction initialize *)
let sheetparser = initialize [ adef ; atempo ; avoice ; adrum ; aguitar ; adef ] ;;

(* 7.3 Example of sheet in frame  **)
(** Transposition de la partition row_sheet pour lire la partition à la verticale *)
let full_sheet_given = MyString.transpose (MyString.to_line_list row_sheet_given) ;;
(* CHANGED: partition de test que nous avons utilisé *)
let full_sheet = MyString.transpose (MyString.to_line_list row_sheet) ;;

(** Récupération de la première mesure de la partition *)
let sheet_given = MyList.first 11 full_sheet_given ;;
let sheet = MyList.first 50 full_sheet ;;

(*
NOTICE D'EXECUTION :
- Chaque lignes composant la partition doivent avoir le même nombre de caractères.
- Dans le cas ou il y a un o où un @ sur la ligne du tempo, la colonne ne doit pas comporter de note.
	Si il y a une note dans cette colonne, elle sera placé sur le dernier temp de la ronde ou blanche

METHODE D'EXECUTION :
- Charge le fichier .ml -> #use "main.ml"
- Initiliaser les parsers pour lancer le parser sur la partition -> sheetparser full_sheet()
- Répéter la lecture d'une frame -> one_step();;
 	Pour lancer n fois one_step (n un entier) : -> n_step n;;

REMARQUES :
- Nous avons décidé de ne pas "grouper" les symboles. Chaque ligne de la partition est donc toujours lu caractère par
	caractères.
- On lit Tchak puis Poum sur la partition. Le son formé n'est donc pas "PoumTchak" mais "TchakPoum"
- Si il y a une erreur sur une des lignes de la partition, l'automate la parsant va dans l'état puits et plus aucun son
	n'est produit (le son en construction sera tout de même finalisé)

REMERCIEMENTS :
- Nous tenons à remercier Mathieu NOGUERON qui a crée un documentation du code qui à été fournis. C'est grâce à celle ci
que nous avons compris le fonctionnement et l'utilité de certaines fonctions.
*)

(* IDEA: Pour le 18/20 :
	- rajouter un automate pour la ligne R
	- rajouter dans le type process une soundtrack "sountrack en construction". Par défault, elle vaut []
	- rajouter dans le type process un entier "répétition". Par défault, répétition vaut 1
	- rajouter dans le type process une clock "old". Par défault, répétition vaut 1.0
	- rajouter dans le type run un entier "répétition". Par défault, répétition vaut 1
	- Lorsque l'automate sur la ligne R lie *Number, "répétition" du process est passé à Number.
	- Lorsque un process se lance, on lui donne la valeur de "répétition" dans run
	- Si on trouve le symbole "[" dans une ligne, l'automate passe dans un état depuis lequel les actions vont utiliser
	la "soundtrack en construction" et enregistrer la clock actuelle (celle dans le process) dans old. Pour la ligne Tempo,
	la clock est mise à Clock.initial.
	- Quand on trouve le symbole "]", on finalise la "soundtrack en construction" en la reproduisant "répétition" fois
	dans la soundtrack (en gérant la clock de chaque son). La clock est ensuite mise à la valeur "old"+("répétition"*"actuelle")
 *)

(* TESTS :*)
(*
sheetparser full_sheet;;
n_step 255;;
get_soundtracks();;
List.rev (get_soundtracks());;
*)
