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

autoRpmHigh = Auto "AA555AA" [0.6,0.4,0.4,0.9] (maxRpm + 1) 1481 (24,12,2010)
autoRpmLow = Auto "AAA222" [0.6,0.4,0.4,0.9] (maxRpm - 1) 90 (24,12,2010)

autoLlantasOk = Auto "DJB005" [maxDesgaste - 0.1,0.4,0.4,0.9] 1950 90 (24,12,2010)
autoLlantasNotOk = Auto "DJB004" [maxDesgaste + 0.1,0.3,0.5,0.9] 2000 90 (24,12,2010)

autoRevision = Auto "AA555AA" [0.6,0.4,0.4,0.9] 3450 90 (24,12,anioLimite)
autoNotRevision = Auto "AA555AA" [0.6,0.4,0.4,0.9] 3450 90 (24,12,anioLimite + 1)

-- Maximo desgaste aceptado para no ser un vehiculo peligroso

maxDesgaste :: Float
maxDesgaste = 0.5

-- Maximo de revoluciones aceptadas para no ser modificadas

maxRpm :: Int
maxRpm = 2000

-- Año limite del ultimo arreglo para verificar si debe ser revisado

anioLimite :: Int
anioLimite = 2015

--Punto 1
--Ambos integrantes: Costo de reparación de un auto

calcularPrecioPatente :: Auto -> Int

calcularPrecioPatente autoTaller | (length.patente) autoTaller == 7 = 12500
                                | "DJ" < patente autoTaller && "NB" > patente autoTaller = calculoPatental(patente autoTaller)
                                | otherwise = 15000

calculoPatental :: String -> Int
calculoPatental patenteAcalcular | (patenteAcalcular !! 5) == '4'  = 3000*length patenteAcalcular
                                | otherwise = 20000

--Punto 2
--Parte 1) Auto peligroso (Martin Tomkinson - MartinTomk)

esPeligroso :: Auto -> Bool
esPeligroso autoTaller = (head.desgasteLlantas) autoTaller > maxDesgaste

--Parte 2) Necesita revisión (Walter Jaldin - Wallej)

verificarRevision :: Auto -> Bool
verificarRevision autoTaller = (anio.ultimoArreglo) autoTaller <= anioLimite

--Punto 3: Personal técnico encargado de las reparaciones
--Parte 1) Martin Tomkinson - MartinTomk

mecanicoAlfa :: Auto -> Auto  -- Si las revoluciones son mayores a maxRpm las fija en maxRpm
mecanicoAlfa autoTaller | rpm autoTaller > maxRpm = modificarRpm autoTaller
                  | otherwise = autoTaller

modificarRpm :: Auto -> Auto
modificarRpm autoTaller = Auto (patente autoTaller) (desgasteLlantas autoTaller) (maxRpm) (temperaturaAgua autoTaller) (ultimoArreglo autoTaller)

mecanicoBravo :: Auto -> Auto -- Cambia las cubiertas eliminando el desgaste
mecanicoBravo autoTaller = Auto (patente autoTaller) (map (0*) (desgasteLlantas autoTaller)) (rpm autoTaller) (temperaturaAgua autoTaller) (ultimoArreglo autoTaller)

mecanicoCharly :: Auto -> Auto
mecanicoCharly autoTaller = (mecanicoAlfa.mecanicoBravo) autoTaller

--Parte 2) Walter Jaldin - Wallej

mecanicoLima :: Auto -> Auto
mecanicoLima autoTaller = Auto (patente autoTaller) ([0.0,0.0,desgasteLlantas autoTaller !! 2,desgasteLlantas autoTaller !! 3]) (rpm autoTaller) (temperaturaAgua autoTaller) (ultimoArreglo autoTaller)

mecanicoZulu :: Auto -> Auto
mecanicoZulu autoTaller = (cambiarTemperatura.mecanicoLima) autoTaller

cambiarTemperatura :: Auto -> Auto
cambiarTemperatura autoTaller = Auto (patente autoTaller) (desgasteLlantas autoTaller) (rpm autoTaller) 90 (ultimoArreglo autoTaller)

mecanicoTango :: Auto -> Auto
mecanicoTango autoTaller = autoTaller


