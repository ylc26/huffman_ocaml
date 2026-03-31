(*
NOM:LOUAGUENOUNI
Prenom:Yanis
Groupe: 2
Compilation: ocamlc -o DM_Huffman_executable LOUAGUENOUNI_DM_Huffman_Yanis.ml
Executer: ./DM_Huffman_executable 
*)

(*Question 1*)
type arbre =
  | C of char
  | N of arbre * arbre

type mot = int list

let t =
  let gauche = 
    let nf = N(C 'n', C 'f') in      (* 000, 001 *)
    let a_nf = N(nf, C 'a') in       (* 01 *)
    a_nf
  in
  let droit_gauche = C 's' in        (* 10 *)
  let droit_droit = N(C 'i', C 't') in (* 110, 111 *)
  let droite = N(droit_gauche, droit_droit) in
  N(gauche, droite)


(* Question 2*)
let rec taille t =
  match t with
  | C _ -> 1
  | N (gauche, droite) -> taille gauche + taille droite

let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 2\n";
  Printf.printf "Taille de l’arbre t : %d\n" (taille t)

(*Question 3 *)
let rec contient c t =
  match t with
  | C x -> x = c
  | N (gauche, droite) -> contient c gauche || contient c droite

let () =
    Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 3\n";
  Printf.printf "Contient 'a' : %b\n" (contient 'a' t);
  Printf.printf "Contient 'x' : %b\n" (contient 'x' t)

(* Question 4 *)
let rec code_char c t =
  match t with
  | C x ->
      if x = c then []
      else raise Not_found
  | N (g, d) ->
      try 0 :: code_char c g
      with Not_found -> 1 :: code_char c d


let () =
    Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 1&4\n";
  List.iter (fun c ->
    Printf.printf "Code de '%c' : " c;
    List.iter (Printf.printf "%d ") (code_char c t);
    print_newline ()
  ) ['n'; 'f'; 'a'; 's'; 'i'; 't']

(* Prompt IA: quelle est la complexité de la fonction code_char dans un arbre de Huffman, et peut-on la lier au nombre de caracteres distinct ? *)
(* Question 5
LLa fonction `code_char` explore récursivement l'arbre de Huffman jusqu'à trouver la feuille contenant le caractère `c`. Le chemin suivi correspond exactement au code binaire du caractère dans l’arbre.
La complexité de cette fonction est proportionnelle à la profondeur de la feuille contenant `c`, c’est-à-dire O(h) où h est la hauteur de l’arbre.
Dans le pire cas, on descend jusqu’à une profondeur maximale, donc on a O(h) comparaisons. Comme l’arbre de Huffman est au mieux équilibré de façon logarithmique, on peut liée la hauteur h à n (le nombre de feuilles) par h appatient à O(log n), donc la complexité devient O(log n) dans le cas optimal.
Mais si l’arbre est très déséquilibré, elle peut atteindre O(n) dans le pire cas. Ainsi, oui, la complexité est bien liée au nombre de caractères de l’arbre, car ce dernier influence sa hauteur, qui elle-même borne le coût de la recherche du caractère.*)

(*Question 6 *)
let rec reconnait m t =
  match m, t with
  | [], C _ -> true                       (* mot vide, feuille atteinte *)
  | [], N _ -> false                      (* mot vide, mais pas une feuille *)
  | 0 :: q, N(g, _) -> reconnait q g     (* 0 : gauche *)
  | 1 :: q, N(_, d) -> reconnait q d     (* 1 : droite *)
  | _, C _ -> false                      (* bit restant mais on est déjà sur une feuille *)
  | _ -> false                           (* tout autre cas *)

let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 6\n";
  let test mot =
    Printf.printf "Reconnait %s : %b\n"
      (String.concat "" (List.map string_of_int mot))
      (reconnait mot t)
  in
  test [0; 0; 0];  (* 'n' : true *)
  test [0; 0; 1];  (* 'f' : true *)
  test [0; 1];     (* 'a' : true *)
  test [1; 1];     (* nœud: false *)
  test [0; 0; 0; 1]; (* trop long: false *)
  test [1; 1; 1];  (* 't' : true *)
;;
(* Question 7 *)
let rec decode_mot_simple m t =
  match m, t with
  | [], C c -> c                          (* fin du mot, feuille atteinte *)
  | [], N _ -> raise (Invalid_argument "mot non reconnu : terminé trop tôt")
  | 0 :: q, N(g, _) -> decode_mot_simple q g
  | 1 :: q, N(_, d) -> decode_mot_simple q d
  | _, C _ -> raise (Invalid_argument "mot non reconnu : trop long")
  | _ -> raise (Invalid_argument "mot non reconnu")
  
let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 7\n";
  let test m =
    try
      let c = decode_mot_simple m t in
      Printf.printf "Le mot %s decode '%c'\n"
        (String.concat "" (List.map string_of_int m)) c
    with Invalid_argument msg ->
      Printf.printf "Erreur pour %s : %s\n"
        (String.concat "" (List.map string_of_int m)) msg
  in
  test [0; 0; 0];  (* 'n' *)
  test [0; 1];     (* 'a' *)
  test [1; 1; 1];  (* 't' *)
  test [1; 1];     (* Erreur *)
  test [0; 0; 0; 1]; (* Erreur *)

(* Question 8
La fonction `decode_mot_simple` parcourtt l’arbre de Huffman en suivant le mot binaire bit par bit.
À chaque bit :
- on descend d’un niveau dans l’arbre,
- on effectue une opération constante (matcher sur le nœud et prendre un fils).
La complexité est donc linéaire en la longueur du mot, soit O(k) où `k` est la longueur du mot binaire. Dans le cas d’un mot valide, k correspond à la profondeur de la feuille représentant le caractère à décoder.
On peut donc aussi dire que la complexité est proportionnelle à la profondeur du caractère dans l’arbre. Comme la profondeur maximal est liée à la hauteur de l’arbre, on a dans le pire cas : O(h), où `h` est la hauteur de l’arbre. Et comme pour `code_char`, si l’arbre est équilibré, `h appartient à O(log n)` avec `n` le nombre de caracteres distincts.*)
;;

(*Question 9 *)
let rec code_texte l t =
  match l with
  | [] -> []
  | c :: q -> code_char c t @ code_texte q t
;;
let exemple = ['s'; 'a'; 't'; 'i'; 's'; 'f'; 'a'; 'i'; 't']

let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 9\n";
  let code = code_texte exemple t in
  Printf.printf "Code du texte : ";
  List.iter (Printf.printf "%d") code;
  print_newline ()
;;

(* Prompt IA : comment expliquer qu’il existe au plus un k tel que les k premiers bits d’un mot codent un caractère dans un arbre de Huffman ? *)
(* Question 10
Chaque caractère est représenté par un chemin unique depuis la racine jusqu'à une feuille. 
Cela signifie que :
- Si un mot m code une séquence de caractères,
- Alors il ne peut existet qu’un seul entier k tel que les k premiers bits de m soient exactement le code complet d’un caractère.
En effet, dès que le parcours dans l’arbre atteint une feuille, on s’arrête : on ne peut pas continuer à décoder plus de bits pour obtenir un autre caractère Cela garantit l’unicité. Cettepropriété rend le décodage sans ambiguïté possible, car on sait qu’en lisant les bits de gauche à droite, on trouve exactement un caractère à la fois, et on peut reprendre la lecture après ce code. Donc, pour tout mot m et tout arbre de Huffman t, il existe au plus un entier k tel que les k premiers bits de m constituent le code valide d’un caractère.*)
;;
(* Question 11  *)
let rec decode_mot m t =
  match m, t with
  | _, C c -> (c, m)                         (* on atteint une feuille, on retourne le char et le reste du mot *)
  | 0 :: q, N(g, _) -> decode_mot q g
  | 1 :: q, N(_, d) -> decode_mot q d
  | [], N _ -> raise (Invalid_argument "mot incomplet (finie sur un nœud)")
  | _ -> raise (Invalid_argument "mot invalide")
;;
(* Question 12 *)
let rec decode_texte m t =
  if m = [] then []
  else
    let (c, reste) = decode_mot m t in
    c :: decode_texte reste t
;;
let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 11&12\n";
  let original = ['s'; 'a'; 't'; 'i'; 's'; 'f'; 'a'; 'i'; 't'] in
  let encoded = code_texte original t in
  let decoded = decode_texte encoded t in
  let s = String.of_seq (List.to_seq decoded) in
  Printf.printf "Texte décodé : %s\n" s
;;
(* Prompt IA : aide pour détailler étape par étape la construction d’un arbre de Huffman à partir de fréquences. *)
(* Question 13
On considère les fréquences suivantes pour les caractères :
-Étape 1 – Regrouper 'f' et 'n' : arbre1 = N('f', 'n'), poids 2
  arbre1(2), 'i'(2), 't'(22), 'a'(3), 's'(3)
-Étape 2 – Regrouper arbre1(2) et 'i'(2) : arbre2 = N(arbre1, 'i'), poids 4
  't'(2), 'a'(3), 's'(3), arbre2(4)
-Étape 3 – Regrouper 't'(2) et 'a'(3) : arbre3 = N('t', 'a'), poids 5
  's'(3), arbre2(4), arbre3(5)
-Étape 4 – Regrouper 's'(3) et arbre2(4) : arbre4 = N('s', arbre2), poids 7
  arbre3(5), arbre4(7)
-Étape 5 – Regrouper arbre3(5) et arbre4(7) : arbre final = N(arbre3, arbre4), poids 12
Ce processus reconstruit exactement l’arbre donné en première page du sujet :
  - 'n' : 000
  - 'f' : 001
  - 'a' : 01
  - 's' : 10
  - 'i' : 110
  - 't' : 111
Conclusion : la constructiob suit parfaitement l’algorithme de Huffman.*)
;;
(*Question 14
La fonction poids ci-dessous calcule la somme des fréquences (poids) des caractères présents dans un arbre de Huffman :*)

let rec poids t stats =
  match t with
  | C c -> stats.(Char.code c)
  | N (g, d) -> poids g stats + poids d stats

(* Ensuite, on construit l’arbre de Huffman à partir des fréquences en utilisant une file de priorité.
Cette file contient des couples (poids, arbre), et on construit l’arbre final en suivant l’algorithme :
1. Créer une file vide.
2. Pour chaque caractère ayant une occurrence > 0, insérer (occurrence, feuille contenant le caractère).
3. Répéter tant qu’il y a plus d’un arbre dans la file :
- Extraire les deux arbres les plus légers,
- Les combiner dans un nouveau nœud,
- Réinsérer le nouvel arbre avec son poids (somme des deux).
4. À la fin, il reste un seul arbre : c’est l’arbre de Huffman optimal.
Voici le squelette donné par le sujet :

let huffman stats =
  let file = create () in
  Array.iteri
    (fun i occ -> if occ > 0 then insert (occ, C (Char.chr i)) file)
    stats;
  while not (is_singleton file) do
    assert (not (is_empty file));
    let p1, t1 = extract_min file in
    let p2, t2 = extract_min file in
    insert (p1 + p2, N(t1, t2)) file
  done;
  snd (extract_min file)

Cette procédure garantit que les regroupements se font toujours entre les arbres les plus légers, ce qui assure un arbre de Huffman minimal en terme de coût total de codage.*)

(* Question 15
Le type de file utilisé dans la fonction `huffman` est :(int * arbre) prio
Il s'agit d'une file de priorité (ou tas) contenant des couples :
  - un entier représentant le poids total de l'arbre,
  - un arbre de Huffman (`arbre`).
Le tri est effectué en fonction de l'entier (le poids), de sorte que les arbres les plus légers soient extraits en premier, ce qui est essentiel pour l’algorithme de Huffman.*)

(* Question 16
La fonction `huffman` peut échouer dans les cas suivants :
1. stats ne contient aucun caractère avec une fréquence > 0 :
- La file sera vide, donc le premier `extract_min` échouera.
- L'assertion `assert (not (is_empty file))` échouera immédiatement.
2. stats ne contient qu'un seul caractere avec une fréquence > 0 :
- La boucle `while not (is_singleton file)` ne s'exécutera pas.
- L'extraction finale retournera directement le seul arbre : ce cas est correctement géré.
3. En théorie, si `insert` ou `extract_min` sont mal implémentés (ex : ne respectent pas la priorité), l'arbre final peut être invalide.
En pratique, le seul cas critique est l'absence total de caractères fréquents dans `stats`, auquel cas une vérification préalable devrait être ajoutée si on souhaite sécuriser davantage.*)

(* Question 17
Dans un tas de Braun (ou tout tas binaire satisfaisant la propriété de tas) :
- L’élément minimal est toujours à la racine du tas.Cela résulte directement de la propriété de tas : chaque nœud est inférieur ou égal à ses fils.Leminimum est au sommet.
- L’élément maximal se trouve quelque part dans une feuille,mais sa position n’est pas déterminée.Le tas ne garantit pas un ordre global, donc le maximum peut être à gauche ou à droite, mais jamais à la racine (sauf si tous les éléments sont égaux). Trouver le maximum nécessite de parcourir tout le tas.
Exemple : Minimum : 3 (racine) et maximum : 8 (feuille à gauchz))*)


(* Question 18
Il existe exactement un arbre de Braun pour chaque taille k ≥ 1.
Voici des représentations en Caml d’arbres de Braun de tailles 2 à 7.
Taille 2 :
N(x, N(y, E, E), E)
Taille 3 :
N(x, N(y, E, E), N(z, E, E))
Taille 4 :
N(x,
  N(y, N(z, E, E), E),
  N(w, E, E)
)
Taille 5 :
N(x,
  N(y, N(z, E, E), N(w, E, E)),
  N(t, E, E)
)
Taille 6 :
N(x,
  N(y, N(z, E, E), N(w, E, E)),
  N(t, N(u, E, E), E)
)
Taille 7 :
N(x,
  N(yy, N(z, N(a, E, E), N(b, E, E)), N(c, E, E)),
  N(d, N(e, E, E), E)
) *)

(* Question 19
Dans un tas de Braun, la propriété structurelle impose que le sous-arbre gauche
a toujours une taille égale ou supérieure de 1 à celle du sous-arbre droit.
Cela garantit un bon équilibre de l'arbre :
- À chaque insertion, on répartit les éléments entre gauche et droite,en conservant la compacité.
Donc la hauteur h d’un tas de Braun de taille n est en : h appartien à O(log n)
La hauteur est logarithmique en la taille du tas.
Cela permet aux opérations comme `insert` ou `extract_min` de rester efficaces,
avec une complexité en O(log n).*)

(*  Question 20  *)

(* Définition du tas de Braun *)
type 'a prio =
  | E
  | N of 'a * 'a prio * 'a prio

(* Fonctions de base *)

let create () = E

let is_empty p =
  match p with
  | E -> true
  | _ -> false

let is_singleton p =
  match p with
  | N(_, E, E) -> true
  | _ -> false

(* Prompt IA : ma fonction insert pour un tas de Braun en OCaml ne respecte pas toujours la structure correcte, elle peut créer un déséquilibre entre les sous-arbres. Peux-tu m'expliquer pourquoi ? *)

let rec taille_prio = function
  | E -> 0
  | N(_, g, d) -> 1 + taille_prio g + taille_prio d

let rec insert x = function
  | E -> N(x, E, E)
  | N(y, g, d) ->
      if x <= y then
        let new_tree =
          if taille_prio d < taille_prio g then
            N(x, g, insert y d)
          else
            N(x, insert y g, d)
        in new_tree
      else
        let new_tree =
          if taille_prio d < taille_prio g then
            N(y, g, insert x d)
          else
            N(y, insert x g, d)
        in new_tree

;;
(* Affichage simple *)
let rec to_list = function
  | E -> []
  | N(x, g, d) -> x :: (to_list g @ to_list d)
;;
let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 20\n";
  let open Printf in
  let p0 = create () in
  let p1 = insert 5 p0 in
  let p2 = insert 3 p1 in
  let p3 = insert 7 p2 in
  let p4 = insert 1 p3 in
  printf "Tas de Braun : %s\n"
    (String.concat "; " (List.map string_of_int (to_list p4)))
;;
(* Question 21
On applique la fonction `ajoute` à l’élément 3 et au tas :Ce tas est représenté en OCaml par :
let t =
  N(2,
    N(6, N(8, E, E), N(7, E, E)),
    N(4, E, E)
  )
On effectue : let t2 = insert 3 t
Le nouvel élément `3` est placé sans violzr la propriété de tas, et l’arbre reste équilibré selon les règles des tas de Braun.
On peut vérifier avec un affichage en préfixe que le tas contient bien tous les éléments et reste valide.*)
let t =
  N(2,
    N(6,
      N(8, E, E),
      N(7, E, E)
    ),
    N(4, E, E)
  )

let t2 = insert 3 t

let rec to_list = function
  | E -> []
  | N(x, g, d) -> x :: (to_list g @ to_list d)

let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 21\n";
  let t2 = insert 3 t in
  let elems = to_list t2 in
  Printf.printf "Tas après insertion de 3 : %s\n"
    (String.concat "; " (List.map string_of_int elems))
    
(* Prompt IA : montrer par récurrence que taille(ajoute(x, t)) = 1 + taille(t) pour un tas de Braun *)
(* Question 22
Nous démontrons par récurrence structurelle sur t que : taille (ajoute(x, t)) = 1 + taille(t) où `taille` est la fonction qui renvoie le nombre de nœuds d’un tas de Braun.
— Cas de base :: t = E (tas vide)
alors ajoute(x, E) = N(x, E, E)
donc taille(ajoute(x, E)) = 1
et taille(E) = 0 ⇒ 1 + taille(E) = 1
propriété vérifié.

— Cas inductif : t = N(n, t1, t2)
Supposons la propriété vraie pour t2 :
taille(ajoute(..., t2)) = 1 + taille(t2)
Selon les cas :
- si x ≤ n : ajoute(x, N(n, t1, t2)) = N(x, ajoutz(n, t2), t1)
- sinon : ajoute(x, N(n, t1, t2)) = N(n, ajoute(x, t2), t1)

Dans les deux cas, on construit un nouveau nœud avec :
- un sous-arbre droit = t1
- un sous-arbre gauche = ajoute(..., t2), qui a donc une taille = 1 +taille(t2)

Donc : taille(ajoute(x, t)) = 1 + taille(t1) + taille(ajoute(..., t2)) = 
1 + taille(t1) + (1 + taille(t2))= 2 + taille(t1) + taille(t2) 
= 1 + (1 + taille(t1) + taille(t2)= 1 + taille(t)

Propriété vérifiée au cas inductiff.
Conclusion :
Par récurrence structurelle, on a bien : taille(ajoute(x, t)) = 1 + taille(t)
En conséquence, on peut en déduire que :
- L’arbre retourné par `ajoute` contient un élément de plus que l’arbre initial.
- Sa construction par insertion préserve la propriété d’équilibre des arbres de Braun :
  puisque l’insertionn se fait toujours en rebalançant gauche et droite, on garde l'invariant : taille(gauche) = taille(droite) + 1
Donc `ajoute(x, t)` retourne toujours un arbre de Braun si `t` en est un.*)

(* Question 23 *)

let rec extrait_gauche = function
  | E -> failwith "tas vide"
  | N(x, E, E) -> (x, E)  (* cas terminal : feuille unique *)
  | N(x, g, d) ->
      let (y, g') = extrait_gauche g in
      (y, N(x, d, g'))
let () =
Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 23\n";
  let t = N(1, N(2, N(3, E, E), E), N(4, E, E)) in
  let (last, t2) = extrait_gauche t in
  Printf.printf "Dernier élément : %d\n" last;
  Printf.printf "Tas restant : %s\n"
    (String.concat "; " (List.map string_of_int (to_list t2)))

(* Prompt IA utilisé : pourquoi cette fonction fusion viole-t-elle la structure des tas de Braun ? *)
(* Question 24
Le problème de la définition proposée de `fusion` est qu'elle ne garantit pas
que le résultat est un tas de Braun valide.
Deux propriétés essentielles doivent être respectées dans un tas de Braun :
1. La propriété de Braun (équilibre) : Le sous-arbre gauche doit avoir la même taille que le droit, ou une taille de 1 supérieure.
2. La propriété de tas : La valeur d’un nœud est inférieure ou égale à celles de ses fils.
Or, dans cette définition : fusion(N(na, a1, a2), N(nb, b1, b2)) = N(na, N(nb, b1, b2), fusion(a1, a2)) si na ≤ nb
Le fils gauche est un arbre complet (N(nb, b1, b2)), tandis que le fils droit est le résultat d’une fusion récursive. Cela entraîne un déséquilibre structurel : le sous-arbre gauche devient trop profond, et la structure de Braun est rompue. De plus, la fusion récursive sur les sous-arbres (`a1`, `a2`) ou (`b1`, `b2`) est faite sans contrôle sur les priorités locales ni les tailles, ce qui peut violer la propriété de tas.
En résumé : La fonction proposée ne préserve ni l'équilibre de Braun, ni l'ordre de tas.*)

(*Question 25*)

let rec fusion t1 t2 =
  match t1, t2 with
  | E, _ -> t2
  | _, E -> t1
  | N(x, g1, d1), N(y, g2, d2) ->
      if x <= y then
        let sous_arbre = fusion d1 t2 in
        if taille_prio g1 >= taille_prio sous_arbre then
          N(x, g1, sous_arbre)
        else
          N(x, sous_arbre, g1)
      else
        fusion t2 t1

let tas1 = insert 1 (insert 3 (insert 5 (create ())))
let tas2 = insert 2 (insert 4 (create ()))

let tas_fusionne = fusion tas1 tas2

let () =
  Printf.printf "-------------------------------\n";
  Printf.printf "Test Question 25\n";
  Printf.printf "Fusion : %s\n"
    (String.concat "; " (List.map string_of_int (to_list tas_fusionne)))

(*BONUS*)

(* Prompt IA  : 
- aide moi à corriger une erreur de compilation liée à l’ordre de définition des fonctions (Unbound value). 
- aide moi et informe moi en Ocaml sur la manipulation de texte et la lecture/écriture de fichiers pour la compression .
*)



(* Type arbre de Huffman *)
type arbreH =
  | C of char
  | N_H of arbreH * arbreH

(* Fonction pour lire un fichier texte *)
let lire_fichier nom =
  let ic = open_in nom in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Fonction pour écrire un fichier *)
let ecrire_fichier nom contenu =
  let oc = open_out nom in
  output_string oc contenu;
  close_out oc

(* Convertit une string en char list *)
let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

(* Conversion rapide avec table *)
let code_texte_rapide (texte : char list) (code : mot array) : mot =
  List.flatten (List.map (fun c -> code.(Char.code c)) texte)

(* Construction du tableau de codage *)
let table_code (t : arbreH) : mot array =
  let table = Array.make 256 [] in
  let rec construire_code chemin = function
    | C c -> table.(Char.code c) <- chemin
    | N_H(g, d) ->
        construire_code (chemin @ [0]) g;
        construire_code (chemin @ [1]) d
  in
  construire_code [] t;
  table

(* Arbre de test *)
let arbre_test =
  let nf = N_H(C 'n', C 'f') in           
  let a_nf = N_H(nf, C 'a') in           
  let gauche = a_nf in                  
  
  let s = C 's' in                        
  let i_t = N_H(C 'i', C 't') in         
  let droite = N_H(s, i_t) in             

  N_H(gauche, droite)                    


(* Fonction de codage depuis un fichier *)
let encoder_fichier texte_in texte_out arbre =
  let code_tbl = table_code arbre in
  let contenu = lire_fichier texte_in in
  let chars = string_to_char_list contenu in
  let code = code_texte_rapide chars code_tbl in
  let bits_string = String.concat "" (List.map string_of_int code) in
  ecrire_fichier texte_out bits_string

(* Test *)
let () =
Printf.printf "-------------------------------\n";
  Printf.printf "BONUS : Accélération du codage et Lecture/écriture de fichiers texte\n";
  Printf.printf "Encodage avec arbre_test dans code.txt\n";
  encoder_fichier "test.txt" "code.txt" arbre_test
  
  

