include "globals.mzn";

int: nPeces;         % Nombre de peces a tocar
int: nInstruments;   % Nombre d'instruments a tocar
int: nMusics;        % Nombre de musics

array[1..nPeces] of int: durada;                    % Durada de les peces.
array[1..nPeces] of int: d = [durada[i] div 5 | i in 1..nPeces];
array[1..nPeces,1..nInstruments] of int: requereix;   % Instruments requerits per cada peça
array[1..nMusics,1..nInstruments] of bool: saptocar;  % True si el music sap tocar l'instrument en qüestió
%array[1..nMusics] of int: saptocarNum;
int: pressupost;      
array[1..nMusics] of int: salari;

int: nPrecs;
array[1..nPrecs] of 1..nPeces: pred;
array[1..nPrecs] of 1..nPeces: succ;

int: dummyUB = sum(i in 1..nPeces)(d[i]);

array[1..nPeces,1..nMusics] of var 0..nInstruments: musicsAssignats;

array [1..nPeces] of var 0..dummyUB: s;  % Temps d'inici de cada peça
%array [1..nPeces] of var 0..dummyUB: f;  % Temps de finalització de cada peça
var 0..pressupost: preu = sum([d[i]*salari[j]| j in 1..nMusics, i in 1..nPeces where musicsAssignats[i,j]!=0]);               % Pressupost

array[1..nInstruments] of int: poolInstruments = [sum(j in 1..nMusics)(saptocar[j,i])| i in 1..nInstruments]; % Number of each instrument we have.

% Assegura que apareixeran el nombre d'instruments que son requerits. No pot ser global cardinality closed perquè no existeix l'instrument 0.
%constraint global_cardinality([musicsAssignats[i,j]|i in 1..nPeces, j in 1..nMusics],[i|i in 1..nInstruments],[sum([requereix[p,i] | p in 1..nPeces])|i in 1..nInstruments]);

% Lower bound per el màxim de temps
constraint max([s[i]+d[i]| i in 1..nPeces]) > max([sum([d[p]*requereix[p,i]/poolInstruments[i]| p in 1..nPeces]) | i in 1..nInstruments]);

% Constraint que assegura que es respecta la prioritat
constraint forall(i in 1..nPrecs) (
  s[succ[i]] >= s[pred[i]]+d[pred[i]]
);

% Una cançó o bé comença al principi, o bé comença quan una altra cançó acaba

/*constraint forall(i in 1..nPeces)(
  s[i] = 0 \/ exists(j in 1..nPeces where j != i)(s[j] + d[j] = s[i])
);*/

constraint forall(p in 1..nPeces, m in 1..nMusics) (
  if not exists(i in 1..nInstruments) (saptocar[m, i] /\ requereix[p, i]>0) then 
    musicsAssignats[p, m] = 0
  endif
);

% La primera cançó ha de començar al principi del festival
constraint min([s[i] | i in 1..nPeces]) = 0;

% Que cada peça tingui els instruments que necessita durant l'execució
constraint forall(i in 1..nPeces, j in 1..nInstruments)(
    count_eq([musicsAssignats[i,k]|k in 1..nMusics], j, requereix[i,j])
);


% Que un music toqui un instrument que li toca.
constraint forall(i in 1..nPeces, j in 1..nMusics)(
    if musicsAssignats[i,j]!=0 then saptocar[j,musicsAssignats[i,j]] endif
);

% Constraint que assegura que un músic no pugui de tocar dos peces simultàniament
/*constraint forall(i in 1..nPeces, m in 1..nMusics, j in 1..nPeces)(
      % Si dues peces són concurrents, llavors un mateix músic no pot tocar a les dues
      if s[i] <= s[j] /\ s[i] + d[i] > s[j] /\ i!=j then musicsAssignats[i,m]*musicsAssignats[j,m]=0 endif
);*/

/* Si dos músics son exactament iguals farem servir més el que cobra menys */
constraint forall(i in 1..nMusics, j in i..nMusics)(
  if forall(k in 1..nInstruments)(saptocar[i,k] = saptocar[j,k]) then
    if salari[i] <= salari[j] then
      sum([d[p] | p in 1..nPeces where musicsAssignats[p,i]>0]) >= sum([d[p] | p in 1..nPeces where musicsAssignats[p,j]>0])
    else
      sum([d[p] | p in 1..nPeces where musicsAssignats[p,i]>0]) <= sum([d[p] | p in 1..nPeces where musicsAssignats[p,j]>0])
    endif
  endif
);

% Si dues peces són concurrents, llavors un mateix músic no pot tocar a les dues
constraint forall(i in 1..nPeces, m in 1..nMusics, j in (i+1)..nPeces)(
      if musicsAssignats[i,m] != 0 /\ musicsAssignats[j,m] != 0 then
        disjunctive([s[i],s[j]],[d[i],d[j]])
      endif
);

% Assegura que per cada instrument en cap moment s'assignin més peces que el toquen que els musics que el saben tocar.
constraint forall(i in 1..nInstruments) (
  cumulative(s,d,[requereix[p,i]|p in 1..nPeces],poolInstruments[i])
);

% Constraint per dir que no es poden executar més musics a la vegada que els que tenim.
%constraint cumulative(s,d,[sum([requereix[p,i] | i in 1..nInstruments])|p in 1..nPeces],nMusics);

%solve minimize max([s[i]+d[i]| i in 1..nPeces])*pressupost + preu;
solve :: int_search(s, smallest, indomain_min) minimize max([s[i]+d[i]| i in 1..nPeces]);

var int: max_value = max([s[i]*5+d[i]*5 | i in 1..nPeces]);

output[
  "Hora d’inici del festival: 10h 0min \nHora d'acabada del festival: " ++ show(10 + max_value div 60)++ "h " ++
  show(max_value mod 60) ++ 
  "min \nCost de contractació: " ++ show(fix(preu)) ++ "\n"
] ++ [
  "Peça " ++ show(p) ++ ":\n" ++ 
  "  Inici: " ++ show(fix(10+s[p]*5 div 60)) ++ "h " ++ show(fix(s[p]*5 mod 60)) ++ " min\n" ++ 
  "  Final: " ++ show(fix(10+((s[p]*5+d[p]*5) div 60))) ++ "h " ++ show(fix((s[p]*5+d[p]*5) mod 60)) ++ " min\n" ++
  concat(["  Music " ++ show(m) ++ " toca instrument " ++ show(fix(musicsAssignats[p,m])) ++ "\n" | m in 1..nMusics where fix(musicsAssignats[p,m])!=0]) |  
  p in 1..nPeces
];

/* CÀLCULS DE LOWER BOUNDS I UPPER BOUNDS */
/*constraint forall(i in 1..nPeces) ( % LB
    s[i] >= max([s[pred[j]]+d[pred[j]] | j in 1..nPrecs where succ[j]==i])
);

constraint forall(i in 1..nPeces) ( % UB
    s[i] <= min([s[i]-d[pred[j]] | j in 1..nPrecs where succ[j]==i])
);*/









