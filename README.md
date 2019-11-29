
# SERVYCE: application shiny pour analyser les variations d'activité et valorisation d'une année à l'autre

## Input:

- données au format Rdata préparées au préalable par le package dimRactivité, qui permet entre autre de valoriser au rum les données de remontée PMSI chargées avec le package pmeasyr.
- les donnée utilisées par l'application shiny sont à renseigner via 3 parametres dans le fichier global.R


## Output:

- l'appli shiny permet un traitement des données (calcul de scores de comorbidité, chargement de référentiels, etc...)
- puis de choisir le service qui sera analysé
- pour obtenir des analyses réparties en plusieurs domaines
    - données d'activité (ip, nombre et type de séjours, etc...)
    - valorisation globale des rss des sejours passés par le service selon le type de sejours et suivi des outliers
    - description et ACP de la valorisation des rums selon differents criteres (typologie des GHM, sévérité, domaine, etc...)
    - analyse des variations du Poids Moyen du Cas Traité (CA moyen ici) et de l'impact des évolutions de la clef de répartition
    - descriptif des parcours intra-hospitaliers de patients et des flux inter-services pour les multi-rums passés par le service analysé
    

## Reste à faire:

- refaire le calcul des recettes théoriques
- graphique d'evolution mensuelle des DMS
- mettre un progress pour chaque page
- ne pas recalculer les pages à chaque click
- empecher erreur quand click avant chargement des données
- effacer les objets en sortant: session$onSessionEnded({}) ou: onStop(function() {}) ?

