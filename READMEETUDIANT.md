

## Table des matières

- [Auteur.e.s](#auteur.e.s)
- [Présentation](#présentation)
- [Fonctionnalités](#fonctionnalités)
- [Installation](#installation)

---

## Auteur.e.s

FILAUDEAU Marius (22211749)
LOEUILLIETTE Julie (22221156)

---

## Présentation

Le projet que nous vous présentons est une implémentation d'un langage pour modéliser le déplacement d'un robot.
Sur le squelette donné, nous avons codé dans un style purement fonctionnel tous les fichiers dans le dossier src
Pour l'implémentation graphique, le code se trouve dans bin/interp.ml, le fichier est très long car il est très inélégant de coder du graphique en ocaml.
Le fichier est abondamment commenté pour expliquer ce que fait chaque fonction et la logique de l'organisation
Toutes les fonctions graphiques prennent en argument les options que l'utilisateur peut passer
Nous avons également ajouté des tests sur les cas limites/pas forcément testés de base afin d'être sûrs que notre code était bien robuste


---

## Fonctionnalités

Le langage : voir sujet

L'interface graphique : trois programmes prédéfinis sont disponibles, le premier est un programme en spirale, le deuxième un programme très indéterministe, et le troisième illustre les rotations du langage de déplacement. Nous avons choisi une diversité de programmes qui puissent montrer les divers aspects du langage de déplacement - le caractère indéterministe avec les approximations, la possibilité de faire quelque chose de très structuré (spirale), et les rotations simples.

Le langage implémenté est une modélisation d'un déplacement de robot - on peut imaginer que ce robot est sur une autre planète et qu'on ne peut le suivre/anticiper ses mouvement que comme ça. Ainsi, nous avons choisi pour chaque programme une cible à atteindre, qui est visible au lancement du programme, et un message apparaît à la fin en fonction de si la cible a été atteinte ou non.

La liste des options implémentées est détaillés dans installation (car elles concernent la ligne d'exécution du programme)


---

## Installation

run:

dune build

then :

dune exec -- interp -options [PROGRAM NUMBER!]

/!\ vous ne pouvez pas lancer l'interface sans numéro de programme (1,2 ou 3)

options:

-abs X_MIN Y_MIN X_MAX Y_MAX -> affichage d'approximation avec position initiale /!\ (0,0) doit être dans le rectangle

-cr -> affichage de points

-bc r v b -> couleur de l’arrière plan

-fc r v b -> couleur de l’avant plan

-rc r v b -> couleur du rectangle

-pc r v b -> couleur du point

-size W H -> la dimension de la fenêtre en pixels

-print -> affichage des instructions

les options peuvent être combinées et même toutes être présentes.
