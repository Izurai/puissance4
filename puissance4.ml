(* Fonction pour choisir la taille du plateau *)

let hauteur_plateau = 6;;
let largeur_plateau = 7;;

let plateau = Array.make largeur_plateau (Array.make hauteur_plateau "0");;

(* Fonction pour afficher le plateau *)
(* On parcours le tableau avec les deux boucles, on affiche un espace si la case
 est vide et les caractères "x" ou "o" selon les pions dans les cases *)

let afficher_plateau p =
        for i = 0 to hauteur_plateau - 1 do
                for j = 0 to largeur_plateau - 1 do
                        print_string("||");
                        if p.(j).(i) = "0" then
                                print_string(" ")
                        else print_str(p.(j).(i));
                done;
                print_newline();
        done;;

(* Fonction pour jouer un coup *)

(* Fonction pour voir si un joueur à gagner *)
