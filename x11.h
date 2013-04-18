/*
   Interface a X windows.

   ==> Ne pas oublier de specifier -lX11 a la compilation.  Par exemple:

           cc -o monprog monprog.c x.c -lX11

   * Le systeme de coordonnee place l'origine en bas et a gauche de la
     fenetre.
   * L'unite de mesure pour les coordonnees est le pixel.
   * Les couleurs:  0 = noir, 1 = blanc, > 1 = couleurs de l'arc-en-ciel (sur
     un ecran couleur bien sur!).
   * Procedures:

     - 'x_open_window' cree une fenetre (resultat = identificateur de
       la fenetre).
     - 'x_close_window' ferme une fenetre.
     - 'x_clear_window' efface le contenu de la fenetre (en noir).
     - 'x_draw_text' affiche une chaine de caracteres a la coordonnee <x,y>
       et de couleur 'col'.  Le parametre 'center' indique si la chaine est
       centree.
     - 'x_draw_line' affiche une ligne de couleur 'col' entre les points
       <x1,y1> et <x2,y2>.
     - 'x_draw_image' affiche une image rectangulaire a la coordonnee <x,y>.
       La taille de l'image est 'width' par 'height' et la couleur de chaque
       pixel est donnee par le vecteur d'entiers 'data' (de longueur
       width*height).  On y retrouve sequentiellement chaque ligne de l'image
       en partant de celle du bas et, pour chaque ligne, les pixels de gauche
       a droite.
*/
       

extern int  x_open_window( /* name, width, height */ );

extern void x_close_window( /* w */ );

extern void x_clear_window( /* w */ );

extern void x_draw_text( /* w, col, x, y, str, center */ );

extern void x_draw_line( /* w, col, x1, y1, x2, y2 */ );

extern void x_draw_image( /* w, x, y, width, height, data */ );
