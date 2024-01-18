include "globals.mzn";

int: nPeces;         % Nombre de peces a tocar
int: nInstruments;   % Nombre d'instruments a tocar
int: nMusics;        % Nombre de musics

array[1..nPeces] of int: durada;                    % Durada de les peces.
array[1..nPeces] of int: d = [durada[i] div 5 | i in 1..nPeces];
array[1..nPeces,1..nInstruments] of int: requereix;   % Instruments requerits per cada peça
array[1..nMusics,1..nInstruments] of bool: saptocar;  % True si el music sap tocar l'instrument en qüestió

int: pressupost;      
array[1..nMusics] of int: salari;

int: nPrecs;
array[1..nPrecs] of 1..nPeces: pred;
array[1..nPrecs] of 1..nPeces: succ;

int: dummyUB = sum(i in 1..nPeces)(d[i]);

array[1..nPeces,1..nMusics] of var 0..nInstruments: musicsAssignats;
array [1..nPeces, 0..dummyUB] of var bool: x;  % Si una peça s'està executant en un temps

var 0..pressupost: preu = sum([d[i]*salari[j]| j in 1..nMusics, i in 1..nPeces where musicsAssignats[i,j]!=0]);               % Pressupost
array[1..nInstruments] of int: poolInstruments = [sum(j in 1..nMusics)(saptocar[j,i])| i in 1..nInstruments]; % Number of each instrument we have.


/* CONSTRAINTS */

%constraint max([t| t in 1..dummyUB, p in 1..nPeces where x[p,t]]) > max([sum([d[p]*requereix[p,i]/poolInstruments[i]| p in 1..nPeces]) | i in 1..nInstruments]);


% Constraint que assegura que es respecta la prioritat
constraint forall(i in 1..nPrecs, j in 0..dummyUB, t in j..dummyUB) (
    % x[succ[i],j] -> not x[pred[i],t]
    % si el successor s'està tocant a j, no pot ser que el predecessor es toqui a t (j o més tard) 
    not x[succ[i],j] \/ not x[pred[i],t]
);

constraint forall(t in 0..dummyUB, m in 1..nMusics) (
    count(i in 1..nPeces where x[i,t])(musicsAssignats[i,m]>0)<=1
);

% La primera cançó ha de començar al principi del festival
constraint exists(i in 1..nPeces)(x[i,0]);

% Una cançó o bé comença al principi, o bé comença quan una altra cançó acaba
/* constraint forall(p in 1..nPeces, t in 0..dummyUB)(
   not x[p,t] \/ t=0 \/ x[p,t-1] \/ exists(i in 1..nPeces)(x[i,t-1] /\ not x[i,t])
); */


% No hi pot haver forats buits!
/*constraint forall(t in 0..dummyUB where 5*t < max_value) (
  exists(p in 1..nPeces)(x[p,t])
);*/

% Que cada peça tingui els instruments que necessita durant l'execució
constraint forall(i in 1..nPeces, j in 1..nInstruments)(
    count([musicsAssignats[i,k]|k in 1..nMusics], j) == requereix[i,j]
);

% Que cada cançó duri el que ha de durar
constraint forall(i in 1..nPeces)(
    count([x[i,t]|t in 0..dummyUB], true) == d[i]
);

% Que un music toqui un instrument que li toca.
constraint forall(i in 1..nPeces, j in 1..nMusics)(
    if musicsAssignats[i,j]!=0 then saptocar[j,musicsAssignats[i,j]] endif
);

%solve minimize max([s[i]+d[i]| i in 1..nPeces])*pressupost + preu;
solve minimize max([t| i in 1..nPeces, t in 0..dummyUB where x[i,t]]);


array[1..nPeces] of var 0..dummyUB: min_values = [ min([t | t in 0..dummyUB where x[p,t]]) | p in 1..nPeces ];
array[1..nPeces] of var 0..dummyUB: max_values = [ max([t | t in 0..dummyUB where x[p,t]]) | p in 1..nPeces ];
var int: max_value = max(max_values);

constraint forall(p in 1..nPeces)(
  d[p] = max_values[p]-min_values[p]+1
);

output [
  "Hora d’inici del festival: 10h 0min \nHora d'acabada del festival: " ++ show(10 + ((max_value+1)*5) div 60)++ "h " ++
  show(((max_value+1)*5) mod 60) ++ 
  "min \nCost de contractació: " ++ show(fix(preu)) ++ "\n"
] ++ [
  "Peça " ++ show(p) ++ ":\n" ++ 
  "  Inici: " ++ show(fix(10+min_values[p]*5 div 60)) ++ "h " ++ show(fix(min_values[p]*5 mod 60)) ++ " min\n" ++ 
  "  Final: " ++ show(fix(10+((max_values[p]+1)*5 div 60))) ++ "h " ++ show(fix((max_values[p]+1) * 5 mod 60)) ++ " min\n" ++ 
  concat(["  Music " ++ show(m) ++ " toca instrument " ++ show(fix(musicsAssignats[p,m])) ++ "\n" | m in 1..nMusics where fix(musicsAssignats[p,m])!=0]) |  p in 1..nPeces ];

