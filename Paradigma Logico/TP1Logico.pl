% ------ Base de Conocimiento ----- %
%estaEn(Lugar, Region)%

estaEn(comarca,eriador).
estaEn(rivendel,eriador).
estaEn(moria,montanasNubladas).
estaEn(lothlorien, montanasNubladas).
estaEn(edoras, rohan).
estaEn(isengard, rohan).
estaEn(abismoDeHelm, rohan).
estaEn(minasTirith, gondor).
estaEn(minasMorgul, mordor).
estaEn(monteDelDestino, mordor).

% ---------------------- Punto 2 ---------------------------- %
% --- Caminos de ejemplo --- %
camino([comarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul,monteDelDestino]).
caminoDos([comarca, rivendel, lothlorien, edoras, monteDelDestino]).
caminoTres([rivendel, lothlorien, edoras, isengard, abismoDeHelm, minasTirith, minasMorgul]).

% ---------------------------- Punto 3 ------------------------- %

sonLimitrofes(Zona,OtraZona):- esZona(Zona),esZona(OtraZona),estaEn(Zona,Region),estaEn(OtraZona,Region) ,Zona \= OtraZona.
sonLimitrofes(Zona,OtraZona):- zonaLimitrofes(Zona,OtraZona).

zonaLimitrofes(Zona, OtraZona):- esZona(Zona), esZona(OtraZona), limitrofes(Zona, OtraZona).
zonaLimitrofes(Zona, OtraZona):- esZona(Zona), esZona(OtraZona), limitrofes(OtraZona, Zona).

limitrofes(rivendel,moria).
limitrofes(moria,isengard).
limitrofes(lothlorien,edoras).
limitrofes(edoras,minasTirith).
limitrofes(minasTirith,minasMorgul).

% ------------------------- Punto 4 -------------------------- %

esZona(Zona):- estaEn(Zona,_).
esRegion(Region) :- estaEn(_,Region).

% Parte A ---> Martin Tomkinson

regionLimitrofe(Region, OtraRegion) :- esRegion(Region), esRegion(OtraRegion), Region \= OtraRegion, estaEn(Zona,Region), 
    estaEn(OtraZona, OtraRegion),sonLimitrofes(Zona,OtraZona).

% Parte B ---> Walter Jaldin

regionLejana(Region, OtraRegion) :- esRegion(Region), esRegion(OtraRegion), not(regionLimitrofe(Region,OtraRegion)), Region \= OtraRegion, regionLimitrofe(Region, TerceraRegion),
        not(regionLimitrofe(OtraRegion, TerceraRegion)).

% ------------------------- Punto 5 ----------------------------- %

% A) Martin Tomkinson ---------------------------- %

puedeSeguirCon(Camino, Zona) :- esZona(Zona), last(Camino, UltimaZona), sonLimitrofes(Zona, UltimaZona).

% B) Walter Jaldin ----------------------------- %

sonConsecutivos(Camino, [PrimeraZona|_]):- last(Camino, UltimaZona), sonLimitrofes(UltimaZona, PrimeraZona).

%---------------------------- Punto 6 -------------------------------- %
% A) Martin Tomkinson ----- %

caminoConLogica([_]).
caminoConLogica([Zona, OtraZona | Cola]) :- sonLimitrofes(Zona,OtraZona), caminoConLogica([OtraZona | Cola]).

% B) Walter Jaldin ----- %

caminoSeguro([_]).
caminoSeguro([_,_]).
caminoSeguro([PrimeraZona, SegundaZona,TerceraZona | Resto]) :-tramoSeguro(PrimeraZona,SegundaZona,TerceraZona), caminoSeguro([SegundaZona,TerceraZona | Resto]).

tramoSeguro(PrimeraZona,SegundaZona,TerceraZona) :- estaEn(PrimeraZona,Region), estaEn(SegundaZona,Region), not(estaEn(TerceraZona, Region)).
tramoSeguro(PrimeraZona,SegundaZona,_) :- estaEn(PrimeraZona,Region), not(estaEn(SegundaZona,Region)).
