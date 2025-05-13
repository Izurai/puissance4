type case = X|O|Vide;;

type plateau = {
        p: case array array;
        largeur: int;
        hauteur: int;
};;

(* Fonction pour choisir la taille du plateau *)

let hauteur_plateau = 6;; (* mettre un instream pour changer la valeur *)
let largeur_plateau = 7;; (* ici aussi *)

(* Fonction pour afficher le plateau *)
(* On parcours le tableau avec les deux boucles, on affiche un espace si la case
 est vide et les caractères "x" ou "o" selon les pions dans les cases *)

let afficher_plateau (plateau:plateau) =
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
        done;;

(* Test fonction afficher_plateau : *)

let plateau1 = {
        p = [|[|X;Vide;Vide|];[|O;O;Vide|];[|O;X;X|]|];
        hauteur = 3;
        largeur = 3;
};;

afficher_plateau plateau1;;

let plateau2 = {
        p = Array.make largeur_plateau (Array.make hauteur_plateau Vide);
        hauteur = hauteur_plateau;
        largeur = largeur_plateau;
};;


(* a *)

(* Fonction pour jouer un coup *)

(* Fonction pour voir si un joueur à gagner *)

(* let a_gagner (plateau:plateau) = *)
(*         for i = 0 to plateau.largeur - 1 do *)
(*                 for j = 0 to plateau.hauteur - 1 do *)
(**)
(*                 done; *)
(*         done;; *)
(**)
