type plateau = {
        p: string array array;
        largeur: int;
        hauteur: int;
};;

(* Fonction pour choisir la taille du plateau *)

let hauteur_plateau = 6;; (* mettre un instream pour changer la valeur *)
let largeur_plateau = 7;; (* ici aussi *)
let plateau1 = {
        p = Array.make largeur_plateau (Array.make hauteur_plateau "0");
        hauteur = hauteur_plateau;
        largeur = largeur_plateau;
};;

(* Fonction pour afficher le plateau *)
(* On parcours le tableau avec les deux boucles, on affiche un espace si la case
 est vide et les caractères "x" ou "o" selon les pions dans les cases *)

let afficher_plateau (plateau:plateau) =
        for i = 0 to plateau.hauteur - 1 do
                for j = 0 to plateau.largeur - 1 do
                        print_string("||");
                        if plateau.p.(j).(i) = "0" then (* voir la définition du type plateau, ici plateau.p désigne les cases (string array array) *)
                                print_string("󰴈")
                        else print_string(plateau.p.(j).(i));
                done;
                print_newline();
        done;;

(* Fonction pour jouer un coup *)

(* Fonction pour voir si un joueur à gagner *)

afficher_plateau plateau1
