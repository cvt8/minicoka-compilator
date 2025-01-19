# Compilateur de Mini coka
Auteurs: Constantin Vaillant-Tenzer

Un compilateur pour le langage Coka.


## Stucture
La structure du compilateur dune/menhir peut être organisée comme suit :
```
- lexer.mll # Implémentation du lexer - Fonctionne bien. Inclus l'ensemble des fonctionnalités demandées.
- parser.mly # Implémentation du parseur - Fonctionne bien. Inclus l'ensemble des fonctionalités demandés, y compris le sucre synataxique.
- ast.ml # Définition de l'arbre syntaxique abstrait
- tests/ # Répertoire de tests
- README.md # Documentation du projet
- dune # Fichier de configuration du système de construction
- dune-project # Spécifie les exigences pour compiler le code
- test.sh # Script d'automatisation des tests
- Makefile # Compile le code avec make, le nettoie avec make clean.
- compile.ml # Implémente la production de code, i.e. le compilateur à proprement parlé.
- compile.mli # Implémete l'environnement de typage, nottament l'envoi du code à la fonction x86_64.
- interp.ml # Ne contient que des fonctions vides pour éviter des bugs dans le programme principal
- kokac_old.ml # Permet de tester l'analyse syntaxique et le typage
- kokac.ml # Fichier principal de compilation
- test0.koka #Fichier de test sur lequel la compilation s'exécute.
```


## Notre travail
### Ce que nous avons fait
J'ai implémenté le lexer, le parser et le typeur - qui compilent correctement et réussissent une majorité des tests. 
Toutes les fonctions demandées ont été implémentées.
Pour la production de code, j'ai implémenté une closure syntaxique et utilisé des fonctions assembleurs. Néanmoins, la gestion des chaines de caractères ne fonctionne pas pour l'affichage.


## Automatisation du test


Pour exécuter l'automatisation des tests, vous pouvez lancer la commande suivante :
```
./test.sh -n ./kokac.exe
```
Où `n` peut être 1 pour les tests d'analyseur syntaxique, 2 pour les tests de vérification de type, ou 3 pour les tests de génération de code.