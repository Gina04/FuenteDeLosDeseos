import Text.Show.Functions

data Persona = Persona{
    edad :: Int, 
    suenios::Suenios,
    nombre:: String, 
    felicidonios :: Int, 
    habilidades ::[String]
}deriving Show

type Suenios = [Suenio]
type Suenio = Persona -> Persona

personaMuyFeliz:: Persona 
personaMuyFeliz= Persona{
          edad= 25, 
          suenios =[],
          nombre = "Matias", 
          felicidonios = 101, 
          habilidades =[]
} 

personaModeradamenteFeliz:: Persona 
personaModeradamenteFeliz = Persona{
          edad=0, 
          suenios =[viajar ["Barcelona", "Madrid", "Sevilla", "Galicia"], recibirseDeUnaCarrera "quimica"],
          nombre = "Maximiliano", 
          felicidonios = 100, 
          habilidades =[]
}

personaPocoFeliz:: Persona
personaPocoFeliz=Persona{
          edad=0, 
          suenios =[],
          nombre = "Evangelina", 
          felicidonios = 50, 
          habilidades =[]
}


--Punto 1
siFelicidonioCumple :: Persona -> (Persona->Int) -> (Persona->Int) -> (Persona -> Int) -> Int
siFelicidonioCumple unaPersona funcionPrimerCondicion  funcionSegundaCondicion   funcionTerceraCondicion
 | felicidoniosMayorA 100 unaPersona = funcionPrimerCondicion $ unaPersona
 | felicidoniosMayorA 50 unaPersona = funcionSegundaCondicion $ unaPersona
 | otherwise = funcionTerceraCondicion $ unaPersona

--1A
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona = siFelicidonioCumple unaPersona  ((*edad unaPersona).felicidonios )((*felicidonios unaPersona).cantidadSuenios)(flip div 2.felicidonios)

--1B
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona = siFelicidonioCumple unaPersona ((*cantidadSuenios unaPersona).felicidonios) ((*edad unaPersona).cantidadSuenios) ((*2).cantidadSuenios)


--multiplicaFelicidonio:: Int-> Persona -> Int 
--multiplicaFelicidonio unValor = (*unValor).felicidonios 

felicidoniosMayorA:: Int -> Persona ->Bool 
felicidoniosMayorA unValor= (> unValor).felicidonios

cantidadSuenios:: Persona -> Int
cantidadSuenios = length.suenios   



--Punto 2
--2a
tieneNombreLargo :: Persona -> Bool
tieneNombreLargo = (>10).length.nombre


--2b
esSuertuda :: Persona -> Bool
esSuertuda = even.(*3).coeficienteDeSatisfaccion 

--2c
tieneNombreLindo :: Persona -> Bool
tieneNombreLindo = (== 'a').last.nombre


--PUNTO3
--3a
recibirseDeUnaCarrera ::String-> Suenio
recibirseDeUnaCarrera unaCarrera unaPersona = darHabilidad unaCarrera.sumarFelicidonios((*1000).length $ unaCarrera) $ unaPersona 

sumarFelicidonios :: Int->Persona->Persona
sumarFelicidonios cantidadFeli unaPersona = unaPersona {felicidonios= cantidadFeli + felicidonios unaPersona}
darHabilidad :: String->Persona->Persona
darHabilidad carrera unaPersona = unaPersona {habilidades = carrera : habilidades unaPersona}
--3b
type Ciudades = [String] 
viajar :: Ciudades->Suenio
viajar ciudades unaPersona = cumplirAnios.sumarFelicidonios((*100).length$ ciudades) $ unaPersona



cumplirAnios :: Persona -> Persona
cumplirAnios unaPersona = unaPersona {edad = (+1).edad $ unaPersona}

--3c 
enamorarse :: Persona -> Suenio
enamorarse unaPersona otraPersona = sumarFelicidonios(felicidonios unaPersona) otraPersona

--3d  
queTodoSigaIgual :: Suenio
queTodoSigaIgual = id

--3e
comboPerfecto :: Suenio
comboPerfecto unaPersona = recibirseDeUnaCarrera "Medicina" . viajar ["Berazategui", "Paris"].sumarFelicidonios 100 $ unaPersona

--SEGUNDA ENTREGA------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
--PUNTO4

eugenia:: Persona 
eugenia = Persona{
    edad= 22,
    suenios= [recibirseDeUnaCarrera "diseñoDeInteriores", viajar["Paris"], enamorarse manuel],
    nombre = "Eugenia", 
    felicidonios =0, 
    habilidades = []

}

manuel:: Persona 
manuel= Persona{
    edad= 30,
    suenios= [recibirseDeUnaCarrera "Ing.Sistemas"],
    nombre = "Manuel", 
    felicidonios = 15, 
    habilidades = []

}

--ACCESORS--------------------------------------------------------------------------------------------------
mapSuenios:: (Suenios->Suenios)-> Persona ->Persona
mapSuenios f unaPersona = unaPersona{suenios = f.suenios$unaPersona}

--The end accessors-----------------------------------------------------------------------------------------
type Fuente = Persona -> Persona

tieneSuenios::(Int->Bool)->(Persona->Persona)->Persona -> Persona
tieneSuenios  unaCondicion f unaPersona
     |unaCondicion.length.suenios $ unaPersona = f unaPersona
     |otherwise = unaPersona

--4A

fuenteMinimalista:: Fuente
fuenteMinimalista unaPersona = tieneSuenios (>0) (sacaDeListaSuenios. cumplePrimerSuenio) $ unaPersona

sacaDeListaSuenios:: Persona -> Persona
sacaDeListaSuenios unaPersona = mapSuenios (drop 1) unaPersona

cumplePrimerSuenio:: Persona -> Persona
cumplePrimerSuenio unaPersona= head (suenios unaPersona) $ unaPersona


--4B
fuenteCopada:: Fuente 
fuenteCopada unaPersona = seQuedaSinSuenios.cumpleTodosLosSuenios $ unaPersona

seQuedaSinSuenios:: Persona -> Persona 
seQuedaSinSuenios unaPersona = mapSuenios (const[]) unaPersona


cumpleTodosLosSuenios:: Suenio    
cumpleTodosLosSuenios unaPersona = foldr ($) unaPersona (suenios unaPersona)

--4C
fuenteAPedido:: Int -> Fuente
fuenteAPedido enesimo unaPersona = tieneSuenios (>=enesimo)((suenios unaPersona) !! (enesimo - 1) )  unaPersona

--4D
fuenteSorda::Fuente
fuenteSorda = id

--PUNTO 5
type Fuentes = [Fuente]
--5.A
fuenteGanadora:: Persona -> Fuentes ->(Persona->Fuente -> Fuente->Fuente)-> Fuente
fuenteGanadora unaPersona fuentes funcionDiscriminante = foldl (funcionDiscriminante unaPersona) id fuentes 


fuenteCumpleCondicion :: Persona -> Fuente -> Fuente-> (Persona -> Int) -> (Int ->Int ->Bool) ->Fuente
fuenteCumpleCondicion persona fuente otraFuente unValor unaCondicion
 | unaCondicion(unValor.fuente $ persona) (unValor.otraFuente $ persona) = fuente
 | otherwise = otraFuente

otorgaMayorFelicidonio :: Persona -> Fuente -> Fuente -> Fuente
otorgaMayorFelicidonio persona fuente otraFuente = fuenteCumpleCondicion persona fuente otraFuente felicidonios (>)

otorgaMenorFelicidonio :: Persona -> Fuente -> Fuente -> Fuente
otorgaMenorFelicidonio persona fuente otraFuente = fuenteCumpleCondicion persona otraFuente fuente felicidonios (<)

otorgaMayorHabilidad :: Persona -> Fuente -> Fuente -> Fuente
otorgaMayorHabilidad persona fuente otraFuente = fuenteCumpleCondicion persona fuente otraFuente (length.habilidades) (>)

--PUNTO6

comparacionDeFelicidonios:: (Int->Bool)->Persona ->Suenio->Bool
comparacionDeFelicidonios funcion unaPersona unSuenio = funcion . felicidonios.unSuenio$ unaPersona
--6.A
sueniosValiosos::Fuente -> Persona -> Suenios
sueniosValiosos  unaFuente unaPersona = filter (comparacionDeFelicidonios (>100) unaPersona).suenios $ unaPersona

--6.B
suenioRaro:: Persona->Bool
suenioRaro unaPersona = any(comparacionDeFelicidonios (==felicidonios unaPersona) unaPersona ).suenios$ unaPersona

--6.C
felicidadTotal:: [Persona]->Int
felicidadTotal personas = sum . map (felicidonios.fuenteCopada) $ personas 


--Punto7

personaSueniosInfinty::Persona
personaSueniosInfinty= Persona{
    edad= 22,
    suenios = sueniosInfinitos,
    nombre = "", 
    felicidonios =0, 
    habilidades = []


}
 
sueniosInfinitos :: [Suenio]
sueniosInfinitos = cycle.concat $ [recibirseCarreraInfinita,viajarLugarInfinito,enamorarsePersonaInfita]

listaLetras :: [String]
listaLetras = map show ['A'..]

infinitoDe :: (a -> Suenio) -> [a] -> [Suenio]
infinitoDe suenio lista = map suenio lista

recibirseCarreraInfinita :: [Suenio]
recibirseCarreraInfinita = infinitoDe recibirseDeUnaCarrera listaLetras

viajarLugarInfinito :: [Suenio]
viajarLugarInfinito = viajar listaLetras : []

enamorarsePersonaInfita :: [Suenio]
enamorarsePersonaInfita = infinitoDe enamorarse [eugenia, manuel, personaModeradamenteFeliz, personaMuyFeliz, personaPocoFeliz]
--Con la persona con suenios infinitos:
--fuenteMinimalista personaSueniosInfinty
--fuenteCopada personaSueniosInfinty
--fuenteAPedido 2 personaSueniosInfinty
--fuenteSorda personaSueniosInfinty

{-
Suenios infinitos fuenteMinimalista

 --sueniosInfinitos (recibirseDeUnaCarrera "medicina")  personaSueniosInfinty

 --fuenteMinimalista (sueniosInfinitos personaSueniosInfinty)
 
 -- Suenios infinitos para fuentesCopadas
 
 -- fuentesCopadas (sueniosInfinitos personaSueniosInfinty)

 --Suenios infinitos para fuenteAPedido 

 --fuenteAPedido 2 (sueniosInfinitos personaSueniosInfinty)

 --Suenios infinitos para fuenteSorda

 --fuenteSorda (sueniosInfinitos personaSueniosInfinty)
 -}

{-
Modelar a una persona con sueños infinitos. Para cada fuente modelada en el punto 4,
¿es posible que esta pueda satisfacer a esa persona que tiene infinitos sueños? Justifique
su respuesta con un ejemplo concreto: “a esta persona P0 con infinitos sueños S0 y la Fuente F1
la invoco en la consola y... (etc. etc. etc.)” y relacionelo con algún concepto visto en la cursada.

Rta:
No es posible, se cuelga ya que definimos la funcion tieneSuenios con un length y un length de listas infinitas 
no termina de evaluar.


-}







