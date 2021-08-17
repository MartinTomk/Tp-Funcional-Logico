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
autoRpmLow = Auto "AAA222" [0.6,0.4,0.4,0.9] 1999 90 (24,12,2010)

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


