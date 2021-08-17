import Text.Show.Functions
type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
data Auto = Auto {
patente :: Patente,
desgasteLlantas :: [Desgaste],
rpm :: Int,
temperaturaAgua :: Int,
ultimoArreglo :: Fecha
} deriving Show

-- Variables que se utilizaran en la invocacion para verificar el correcto funcionamiento

autoRpmHigh = Auto "AA555AA" [0.6,0.4,0.4,0.9] 2001 1481 (24,12,2010)
autoRpmLow = Auto "AAA222" [0.6,0.4,0.4,0.8] 1999 90 (24,12,2010)

autoLlantasOk = Auto "DJB005" [0.4,0.4,0.4,0.9] 1950 90 (24,12,2010)
autoLlantasNotOk = Auto "DJB004" [0.6,0.3,0.5,0.9] 2000 90 (24,12,2010)

autoRevision = Auto "AA555AA" [0.6,0.4,0.4,0.9] 3450 90 (24,12,2015)
autoNotRevision = Auto "AA555AA" [0.6,0.4,0.4,0.9] 3450 90 (24,12,2016)

--Punto 1
--Ambos integrantes: Costo de reparación de un auto

calcularPrecioArreglo :: Auto -> Int

calcularPrecioArreglo autoTaller | (length.patente) autoTaller == 7 = 12500
                                | (estaEntre "DJ" "NB".take 2.patente) autoTaller = (calculoPatental.patente) autoTaller
                                | otherwise = 15000

--No se definio el tipo de la funcion "estaEntre" para que pueda aceptar distintos tipos de valor

estaEntre minimo maximo valor = minimo <= valor && maximo >= valor --Los extremos forman parte del intervalo

calculoPatental :: String -> Int
calculoPatental patenteAcalcular | last patenteAcalcular == '4'  = 3000*length patenteAcalcular
                                | otherwise = 20000

--Punto 2
--Parte 1) Auto peligroso (Martin Tomkinson - MartinTomk)

esPeligroso :: Auto -> Bool
esPeligroso autoTaller = (head.desgasteLlantas) autoTaller > 0.5

--Parte 2) Necesita revisión (Walter Jaldin - Wallej)

necesitaRevision :: Auto -> Bool
necesitaRevision autoTaller = (anio.ultimoArreglo) autoTaller <= 2015

--Punto 3: Personal técnico encargado de las reparaciones
type Mecanico = Auto -> Auto
--Parte 1) Martin Tomkinson - MartinTomk

mecanicoAlfa :: Mecanico  -- Si las revoluciones son mayores a maxRpm las fija en maxRpm
mecanicoAlfa autoTaller = autoTaller {rpm = (min.rpm) autoTaller 2000}

mecanicoBravo :: Mecanico -- Cambia las cubiertas eliminando el desgaste
mecanicoBravo autoTaller = autoTaller {desgasteLlantas = [0,0,0,0]}

mecanicoCharly :: Mecanico
mecanicoCharly autoTaller = (mecanicoAlfa.mecanicoBravo) autoTaller

--Parte 2) Walter Jaldin - Wallej

mecanicoLima :: Mecanico
mecanicoLima autoTaller = autoTaller {desgasteLlantas = (cambiarDosPrimeras.desgasteLlantas)autoTaller}

cambiarDosPrimeras :: [Desgaste] -> [Desgaste]
cambiarDosPrimeras (x:y:xs) = (0.0:0.0:xs)

mecanicoZulu :: Mecanico
mecanicoZulu autoTaller = (cambiarTemperatura.mecanicoLima) autoTaller

cambiarTemperatura :: Mecanico
cambiarTemperatura autoTaller = autoTaller {temperaturaAgua = 90}

mecanicoTango :: Mecanico
mecanicoTango autoTaller = autoTaller

-- Comienza la parte II
--Nuevas variables agregadas para la prueba 

fechaHoy :: Fecha --Utilizada para la orden de reparación
fechaHoy = (27,05,2020)

autosOrdenados :: [Auto]
autosOrdenados = [autoRpmHigh,autoRpmLow,autoRpmHigh,autoRpmLow,autoRpmHigh,autoRpmLow]

autosNotOrdenados :: [Auto]
autosNotOrdenados = [autoRpmLow,autoRpmHigh,autoRpmLow,autoRpmHigh,autoRpmLow]

mecanicos :: [Mecanico]
mecanicos = [mecanicoAlfa,mecanicoBravo,mecanicoCharly,mecanicoLima,mecanicoTango,mecanicoZulu]

autosNotRevision :: [Auto]
autosNotRevision = [autoNotRevision,autoNotRevision,autoNotRevision,autoNotRevision,autoNotRevision]
autosMixRevision :: [Auto] -- Tendra autos que necesiten revision y otros que no
autosMixRevision = [autoLlantasNotOk,autoLlantasOk,autoRevision,autoNotRevision,autoRevision,autoRpmLow,autoRpmHigh]
autosPruebaRevision :: [Auto] --Todos necesitan revision, es 1 de cada precio de las patentes 12500,20000,15000,18000 TOTAL = 65500
autosPruebaRevision = [autoRpmHigh,autoRpmLow,autoLlantasNotOk,autoLlantasOk]

--Listas infinitas

tecnicosInfinitos = mecanicoTango:mecanicoTango:mecanicoZulu:tecnicosInfinitos

autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0
autosInfinitos' :: Float -> [Auto]
autosInfinitos' n = Auto {
patente = "AAA000",
desgasteLlantas = [n, 0, 0, 0.3],
rpm = 1500 + round n,
temperaturaAgua = 90,
ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)

--Punto 4
--Orden TOC Ambos integrantes

type AutoPosicion = (Int,Auto)

verificarListaOrdenada :: [Auto] -> Bool
verificarListaOrdenada listaAuto = all estaOrdenado.zip [1..] $ listaAuto
 
estaOrdenado :: AutoPosicion -> Bool
estaOrdenado (posicion,autoTaller) = (even.calculoDesgaste) autoTaller == even posicion

calculoDesgaste :: Auto -> Int
calculoDesgaste autoTaller = (round.(*10).sum.desgasteLlantas) autoTaller

--Punto 5
--Orden reparacion Ambos integrantes

data OrdenReparacion = OrdenReparacion {
    mecanicosLista :: [Mecanico],
    fechaReparacion :: Fecha
} deriving Show

ordenR = OrdenReparacion mecanicos fechaHoy

aplicarOrdenReparacion :: OrdenReparacion -> Auto -> Auto
aplicarOrdenReparacion ordenRep autoTaller =  modificarUltimoArreglo (fechaReparacion ordenRep).asignarMecanicos (mecanicosLista ordenRep) $ autoTaller

asignarMecanicos :: [Mecanico] -> Auto -> Auto
asignarMecanicos listaMecanicos autoTaller = foldl (flip ($)) autoTaller listaMecanicos

modificarUltimoArreglo :: Fecha -> Auto -> Auto
modificarUltimoArreglo fechaArreglo autoTaller = autoTaller {ultimoArreglo = fechaArreglo}

--Punto 6
--Parte 1) Martin Tomkinson : MartinTomk

autoEnCondiciones :: [Mecanico] -> Auto -> [Mecanico]
autoEnCondiciones listaMecanicos autoTaller = (filter (\unMecanico -> (not.esPeligroso.unMecanico) autoTaller)) listaMecanicos

--Parte 2) Walter Jaldin : Wallej

costoReparacion :: [Auto] -> Int
costoReparacion listaAutos = (calcularPrecioArregloTotal.autoArevisar) listaAutos

autoArevisar :: [Auto] -> [Auto]
autoArevisar listaAutos = filter (necesitaRevision) listaAutos

calcularPrecioArregloTotal listaAutos = foldl (\semilla unAuto -> semilla + calcularPrecioArreglo unAuto) 0 listaAutos

--Punto 7
--Parte 1) Martin Tomkinson : MartinTomk
-- Se creo una nueva funcion ya que la original no es capaz de manejar listas infinitas debido a que necesita recorrer toda la lista
-- Si se quisiera se podria cambiar el head por un take y pasarle por argumento la cantidad de mecanicos que se necesite
-- Si, es posible obtener el primer tecnico que deje el auto en condiciones utilizando un head, debido a que haskell utiliza el metodo de "lazy evaluation"
-- el cual difiere las evaluaciones hasta que sean necesarias para otros calculos. Por esto es posible realizar head o take de una lista infinita.
autoEnCondicionesInfinita :: [Mecanico] -> Auto -> Mecanico
autoEnCondicionesInfinita listaMecanicos autoTaller = (head.filter (\unMecanico -> (not.esPeligroso.unMecanico) autoTaller)) listaMecanicos

--Parte 2) Walter Jaldin :: Wallej
--No seria posible, debido a que no terminaria de realizar la operacion de sumatoria del costo de reparacion porque al sumar busca llegar al final de la lista pero no existe la misma
costoReparacion' :: [Auto] -> Int
costoReparacion' listaAutos = (calcularPrecioArregloTotal.take 3 .autoArevisar) listaAutos
