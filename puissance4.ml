type case = X|O|Vide;;
type joueur = Xavier|Ophelie;;

type plateau = {
        p: case array array;
        largeur: int;
        hauteur: int;
        joueur: joueur;
};;

(* Fonction pour choisir la taille du plateau *)

let hauteur_plateau = 6;; (* mettre un instream pour changer la valeur *)
let largeur_plateau = 7;; (* ici aussi *)

(* Fonction pour afficher le plateau *)
(* On parcours le tableau avec les deux boucles, on affiche un espace si la case
 est vide et les caractères "X" ou "O" selon les pions dans les cases *)

let afficher_plateau_it (plateau:plateau) =
        print_newline();
        for i = 0 to plateau.hauteur - 1 do
                for j = 0 to plateau.largeur - 1 do
                        print_string("|");
                        match plateau.p.(j).(plateau.hauteur - i - 1) with
                                Vide -> print_string(" ");
                                |X -> print_string("X");
                                |O -> print_string("O");
                done;
                print_string("|");
                print_newline();
        done;
        for i = 1 to plateau.largeur do
                print_string(" "); print_int(i);
        done
;;

(* fait exactement la même chose que la fonction du dessus mais en récursif *)
let afficher_plateau (plateau:plateau) =
        print_newline();
        let rec afficher_case_ligne y n = (* affiche les cases d'une ligne donnée *)
                if n < plateau.largeur then
                        begin
                                print_string "|";
                                (match plateau.p.(n).(y) with
                                        Vide -> print_string " "
                                        |X -> print_string "X"
                                        |O -> print_string "O");
                                afficher_case_ligne y (n+1)
                        end
        in
        let rec afficher_ligne n = (* affiche toutes les lignes en utilisant afficher_case_ligne *)
                if n >= 0 then
                        begin
                                afficher_case_ligne n 0;
                                print_string "|";
                                print_newline();
                                afficher_ligne (n-1)
                        end
        in
        afficher_ligne (plateau.hauteur - 1); (* affiche le plateau *)
        let afficher_index_colonnes n =
                let rec aux i =
                        if i <= n then 
                                begin
                                        print_string " ";
                                        print_int i;
                                        aux (i+1)
                                end
                in
                aux 1;
        in
        afficher_index_colonnes (plateau.largeur) (* affiche les index *)
;;

(* Test fonction afficher_plateau :

let plateau1 = {
        p = [|[|X;Vide;Vide|];[|O;O;Vide|];[|O;X;X|]|];
        hauteur = 3;
        largeur = 3;
};;

afficher_plateau_it plateau1;;
afficher_plateau plateau1;;

let plateau2 = {
        p = Array.make largeur_plateau (Array.make hauteur_plateau Vide);
        hauteur = hauteur_plateau;
        largeur = largeur_plateau;
};;

afficher_plateau_it plateau2;;
afficher_plateau plateau2;;


*)

(* Fonction pour jouer un coup *)
(* Cette fonction trouve une ligne vide dans la colonne indiquee par le joueur et
 met un jeton dans cette case si elle est pas vide, elle change aussi le tour du 
 joueur en cours *)

let jouer_un_coup (plateau: plateau) (col: int) : plateau =
        if col < 1 || col > plateau.largeur then (
                print_endline "Colonne invalide.";
                plateau
        )
        else
                let col_index = col - 1 in
                let rec trouver_ligne_vide ligne =
                        if ligne >= plateau.hauteur then None
                        else match plateau.p.(col_index).(ligne) with
                        | Vide -> Some ligne
                        | _ -> trouver_ligne_vide (ligne + 1)
                in
                match trouver_ligne_vide 0 with
                | None -> 
                        print_endline "Colonne pleine";
                        plateau
                | Some ligne ->
                        let new_p = Array.map Array.copy plateau.p in
                        new_p.(col_index).(ligne) <- 
                                (match plateau.joueur with
                                | Xavier -> X
                                | Ophelie -> O);
                        let joueur_suivant = match plateau.joueur with
                                | Xavier -> Ophelie
                                | Ophelie -> Xavier
                        in
                        { plateau with p = new_p; joueur = joueur_suivant }
;;

(* Fonction pour voir si un joueur à gagner *)

let a_gagner plateau = 
;;

(* Game loop *)

let () =
        
