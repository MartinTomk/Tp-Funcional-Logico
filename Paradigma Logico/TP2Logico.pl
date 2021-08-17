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

% ------------------------------------------ PARTE 2 ----------------------------------------------%

%--------------------------------------------Punto 1--------------------------------------%
% A) Martin Tomkinson

cantidadRegiones(Camino, CantRegiones) :- findall(Region,(estaEn(Zona,Region),member(Zona,Camino)),ListaRegiones), list_to_set(ListaRegiones,ListaRegionesNoRep),
                                length(ListaRegionesNoRep, CantRegiones).

% B) Walter Jaldin

esVueltero(Camino) :- not(zonasDiferentes(Camino)).

zonasDiferentes([]).
zonasDiferentes([_]).
zonasDiferentes([PrimeraZona | Cola]) :-not(member(PrimeraZona,Cola)), zonasDiferentes(Cola).

% C) Ambos integrantes

todosLosCaminosConducenAMordor([]).
todosLosCaminosConducenAMordor([PrimerCamino | Cola]) :- last(PrimerCamino,Ultimo), estaEn(Ultimo, mordor), todosLosCaminosConducenAMordor(Cola).

%----------------------------------------Punto 2 ------------------------%

% Ambos integrantes

viajero(gandalf,maiar(maiar,[25,260])).

viajero(legolas,guerrero(elfo,[arma(espada,20),arma(arco,29)])).
viajero(gimli,guerrero(enano,[arma(hacha,26)])).
viajero(aragorn,guerrero(dunedain,[arma(espada,30)])).
viajero(boromir,guerrero(hombre,[arma(espada,26)])).
viajero(gorbag,guerrero(orco,[arma(ballesta,24)])).
viajero(ugluk,guerrero(uruk-hai,[arma(espada,26),arma(arco,22)])).

viajero(barbol,pacifista(ent,5300)).
viajero(frodo,pacifista(hobbit,53)).
viajero(sam,pacifista(hobbit,36)).

%------------------------------------------- Punto 3 ----------------------------------%
% A) Martin Tomkinson
raza(Nombre,Raza) :- viajero(Nombre,Tipo), tipoRaza(Tipo,Raza).

tipoRaza(guerrero(Raza,_),Raza).
tipoRaza(maiar(Raza,_),Raza).
tipoRaza(pacifista(Raza,_),Raza).

% B) Walter Jaldin

armaViajero(Nombre,Arma) :- viajero(Nombre,Tipo), tipoArma(Tipo, Arma).

tipoArma(maiar(_,_),baston).
tipoArma(guerrero(_,ListaArmasFunc),ListaArmas) :- findall(Arma, member(arma(Arma,_),ListaArmasFunc),ListaArmas).
tipoArma(pacifista(hobbit,Edad),espadaCorta) :- Edad > 50.
tipoArma(pacifista(hobbit,Edad),daga) :- Edad < 50.
tipoArma(pacifista(ent,_),fuerza).

% C) Ambos Integrantes
nivel(Nombre,Nivel) :- viajero(Nombre,Tipo), tipoNivel(Tipo, Nivel).

tipoNivel(maiar(_,[Nivel,_]),Nivel).
tipoNivel(guerrero(_,ListaArmasFunc),Nivel) :- findall(Nivel, member(arma(_,Nivel),ListaArmasFunc),ListaNivelArmas), max_member(Nivel,ListaNivelArmas).
tipoNivel(pacifista(hobbit,Edad),Nivel) :- Nivel is Edad/4. 
tipoNivel(pacifista(ent,Edad),Nivel) :- Nivel is Edad/100.


%----------Punto 4-----------------------------------------------%

%----------Parte A-----------------------------------------------%

grupo(GrupoPosible) :- findall(Nombre,viajero(Nombre,_),ListaViajeros),gruposPosibles(ListaViajeros,Subgrupos), 
        permutation(Subgrupos,GrupoPosible), length(GrupoPosible,Cant), Cant > 0.

gruposPosibles([], []).
gruposPosibles([Nombre | Resto], [Nombre | OtroResto]) :- viajero(Nombre,_), gruposPosibles(Resto,OtroResto).
gruposPosibles([_ | Resto], OtroResto) :- gruposPosibles(Resto,OtroResto).

zonaRequerimiento(minasTirith, integrante(maiar, 25)).
zonaRequerimiento(moria, elemento(armaduraMithril, 1)).
zonaRequerimiento(isengard, integrante(maiar, 27)).
zonaRequerimiento(isengard, magia(280)).
zonaRequerimiento(abismoDeHelm, integrante(elfo, 28)).
zonaRequerimiento(abismoDeHelm, integrante(enano, 20)).
zonaRequerimiento(abismoDeHelm, integrante(maiar, 25)).
zonaRequerimiento(abismoDeHelm, magia(200)).
zonaRequerimiento(sagrario, elemento(anduril, 1)).
zonaRequerimiento(minasMorgul, elemento(lembas, 2)).
zonaRequerimiento(minasMorgul, elemento(luzEarendil, 1)).

tiene(sam, lembas).
tiene(sam, lembas).
tiene(sam, lembas).
tiene(gandalf, sombraGris).
tiene(frodo, armaduraMithril).
tiene(frodo, luzEarendil).
tiene(frodo, lembas).
tiene(frodo, capaElfica).
tiene(sam, capaElfica).
tiene(legolas, capaElfica).
tiene(aragorn, capaElfica).
tiene(aragorn, anduril).

%----------Parte B-----------------------------------------------%
cumpleRequerimiento(Grupo,Requerimiento) :- grupo(Grupo), cumpleCondicion(Grupo, Requerimiento).

cumpleCondicion(Grupo, integrante(RazaReq, NivelReq)) :- raza(Nombre, RazaReq), nivel(Nombre, Nivel), Nivel >= NivelReq, member(Nombre,Grupo).
cumpleCondicion(Grupo, elemento(Item, Cantidad)):- cantidadItemGrupo(Grupo, Item, CantidadGrupo), CantidadGrupo >= Cantidad.
cumpleCondicion(Grupo, magia(CantidadReq)) :- cantidadMagiaTotal(Grupo,CantidadMagia), CantidadMagia >= CantidadReq.

cantidadItemGrupo(Grupo, Item,Cantidad) :- findall(Item,(tiene(Nombre,Item),member(Nombre,Grupo)), ListaTieneItem), length(ListaTieneItem, Cantidad).
cantidadMagiaTotal(Grupo, CantidadMagia) :- findall(Magia,(magiaTotal(Nombre,Magia),member(Nombre,Grupo)),ListaMagia), sumlist(ListaMagia,CantidadMagia).

magiaTotal(Nombre,Magia) :- viajero(Nombre,maiar(_,[_,Magia])).
%----------Punto 5-----------------------------------------------%

%----------Parte A: Martin Tomkinson-----------------------------------------------%

puedeAtravesar(Grupo,Zona) :- esZona(Zona), grupo(Grupo),forall(zonaRequerimiento(Zona,Requerimiento),cumpleRequerimiento(Grupo,Requerimiento)).
puedeAtravesar(Grupo,Zona) :- esZona(Zona), grupo(Grupo), not(zonaRequerimiento(Zona,_)).

%----------Parte B: Walter Jaldin-----------------------------------------------%

seSientenComoEnCasa(Grupo,Region) :- esRegion(Region),grupo(Grupo),forall(estaEn(Zona,Region),puedeAtravesar(Grupo,Zona)).
