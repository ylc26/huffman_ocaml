# Codage de Huffman en OCaml

Projet académique d’algorithmique réalisé en OCaml. Il met en pratique la programmation fonctionnelle, les arbres binaires, le codage préfixe et les files de priorité à travers l’algorithme de Huffman.

## Objectifs

- représenter un arbre de Huffman ;
- encoder et décoder un caractère ou un texte ;
- calculer les fréquences d’apparition des caractères ;
- construire un arbre de codage à partir de ces fréquences ;
- manipuler un tas de Braun servant de file de priorité ;
- lire un fichier texte et produire sa représentation binaire.

## Structure du dépôt

```text
.
├── examples/
│   ├── encoded_sample.txt     # Exemple de sortie encodée
│   └── input.txt              # Texte d’entrée utilisé par la démonstration
├── src/
│   └── huffman.ml             # Implémentation et tests pédagogiques
├── .gitignore
├── Makefile
└── README.md
```

## Prérequis

- OCaml et le compilateur `ocamlc` ;
- `make` pour utiliser les commandes proposées.

## Compilation et exécution

Depuis la racine du dépôt :

```bash
make
make run
```

L’exécutable est créé dans `build/`. La démonstration lit `examples/input.txt` et écrit le résultat dans `output/code.txt`.

Pour supprimer les fichiers générés :

```bash
make clean
```

## Notions mises en œuvre

- récursivité et filtrage par motifs ;
- arbres binaires et parcours ;
- codes préfixes ;
- analyse de complexité ;
- tas de Braun et file de priorité ;
- entrées-sorties sur fichiers.

## Contexte

Projet universitaire réalisé dans le cadre de la Licence Informatique à l’Université Paris-Saclay.
