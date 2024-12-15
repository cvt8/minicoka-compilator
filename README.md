# Compilateur de Mini coka
Auteurs: Constantin Vaillant-Tenzer et Dominique Amar.

Un compilateur pour le langage Coka.


## Stucture
La structure du compilateur dune/menhir peut être organisée comme suit :
```
- lexer.mll # Implémentation du lexer - Fonctionne bien
- parser.mly # Implémentation du parseur
- ast.ml # Définition de l'arbre syntaxique abstrait
- Tests/ # Répertoire de tests
- README.md # Documentation du projet
- dune # Fichier de configuration du système de construction
- dune-project # Spécifie les exigences pour compiler le code
- test.sh # Script d'automatisation des tests
- Makefile # Compile le code avec make, le nettoie avec make clean.
```


## Notre travail
### Ce que nous avons fait
Nous avons implémenté avec succès le lexer pour le langage Coka. Cependant, nous avons rencontré quelques problèmes avec l'analyseur syntaxique. En particulier, nous avons reçu les erreurs suivantes :
```
Fichier « parser.mly », ligne 238, caractères 0-4 :
Warning : symbol elif is unreachable from any of the start symbol(s).
Fichier « parser.mly », ligne 210, caractères 16-20 :
Erreur : Unbound value expr
Indice : Vous vouliez dire exp ?
make : *** [Makefile:3 : all] Erreur 1
```
Malgré ces erreurs, nous avons implémenté toute la syntaxe du langage. Pour faciliter le débogage, nous avons commenté le code qui fonctionne actuellement et mis en place des contraintes de lecture de droite à gauche. De plus, nous avons inclus des fonctions de sucre syntaxique dans notre code, bien qu'elles soient actuellement commentées.


### Typer
Le typographe est complet.


### A faire
Comme notre code n'est pas encore compilé, nous ne pouvons pas vérifier tous les tests. 




## Automatisation du test


Pour exécuter l'automatisation des tests, vous pouvez lancer la commande suivante :
```
./test.sh -n binaire-compilo
```
Où `n` peut être 1 pour les tests d'analyseur syntaxique, 2 pour les tests de vérification de type, ou 3 pour les tests de génération de code.