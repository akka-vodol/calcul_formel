# calcul_formel
Notre TIPE sur le calcul formel

Je propose qu'à partir de maintenant tout ce qu'on fait soit mis à jour sur github. J'ai séparé le code dans plusieurs fichiers pour qu'on s'y retrouve plus facilement.

 -le fichier code_basique contient juste les opérations de base du logiciel.
 
 -le fichier interface contient tous les programmes d'affichage, ainsi que d'éventuels parseurs en caml.
 
 -le fichier simplification contient tout ce qui à été fait sur la simplification.


Sur la simplification : Au lieu d'implémenter l'idée de benjamin de faire du backtracking, j'ai créé un algoritme qui regroupe par rapport à une variable. Cela devrai permettre de simplifier un bon nombre de cas non triviaux. Par exemple si on a f(a) - g(a), avec f en fait égal à g, on se ramène à un polynome (si possible) et la simplification des coefficients (qui sont des entiers) trouve qu'ils sont tous nuls. Si on a l'expression : f(a, b) - g(a, b) avec f qui est en fait égale à g, la simplification ramène à 
(fn(b)-gn(b))a^n + ... + (f1(b)-g1(b))a + f0(b) - g0(b)
Il ne reste plus qu'à trouver que fi(b) = gi(b). On s'est ramené à des fonctions à une variable.
