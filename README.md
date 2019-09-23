
# SERVYCE: application shiny pour analyser les variations d'activité et valorisation d'une année à l'autre

## Input:

- données au format Rdata préparées au préalable avec l'interface shiny "imports" (pour obtenir un Ddata avec la ventilation de la valorisation notamment)


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

- dernier graphique des flux de patient à mettre en place (journées selon type d'autorisation) à faire par regroupement et non par urm
- refaire le calcul du gisement en calculant la recette theorique ainsi: propghS_giac*nbr_ghS*tarif_ghS (tarifs année n) pour produire un tableau avec 1) difference due aux tarifs, 2) difference avec la recette theorique, et 3) le reste <=> effet casemix (cad hors tarifs et hors variation de niveaux par racine). Produire aussi une table avec effet tarifs et recette theorique par racine.
- changer referime en nomensland pour accès externe
- 2eme et 3eme sunburst à mettre par regroupement comme le 1er
- verifier les top GHM des parcours 
- graphique d'evolution mensuelle des DMS
- mettre un progress pour chaque page
- ne pas recalculer les pages à chaque click
- changer la skin
- premier parcours ne respecte pas les couleurs
- empecher erreur quand click avant chargement des données
- effacer les objets en sortant: session$onSessionEnded({}) ou: onStop(function() {}) ?

