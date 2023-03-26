{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant ==" #-}
module Tema (Tema, nuevoT, nombreT, datosT, etiquetasT, agregarT, aplicaT)
where
import Tipos

data Tema = Tem Nombre [ Etiqueta ] Datos deriving (Eq, Show, Ord)

nuevoT :: Nombre -> Datos -> Tema
--Dado un nombre y datos crea un Tema con el nombre y datos colocados.
nuevoT nombre datos = Tem nombre [] datos
nombreT :: Tema ->Nombre
--Dado un Tema devuelve su nombre.
nombreT (Tem name etiquetas datos) = name
datosT :: Tema -> Datos
--Dado un Tema devuelve sus datos.
datosT (Tem name etiquetas datos) = datos
etiquetasT :: Tema -> [ Etiqueta ]
--Dado un Tema devuelve su etiqueta.
etiquetasT (Tem name etiquetas datos) = etiquetas
agregarT :: Etiqueta -> Tema -> Tema
--Dado una etiqueta y un tema agrega la etiqueta al tema.
agregarT etiqueta (Tem name etiquetas datos) = Tem name (etiquetas ++ [etiqueta]) datos
aplicaT :: Etiqueta -> Tema -> Bool
--Dado una etiqueta y un Tema devuelve True sí la etiqueta esta en el tema o False en caso contrario.
aplicaT etiqueta (Tem name etiquetas datos) = etiqueta `elem` etiquetas

testTe = [ nuevoT "poison heart" "ramones" == Tem "poison heart" [] "ramones",
      nombreT (Tem "poison heart" ["rock"] "ramones") == "poison heart",
      datosT (Tem "poison heart" ["rock"] "ramones") == "ramones",
      etiquetasT (Tem "poison heart" ["rock"] "ramones") == ["rock"],
      agregarT "reggaeton" (Tem "poison heart" ["rock"] "ramones") == Tem "poison heart" ["rock", "reggaeton"] "ramones",
      aplicaT "reggaeton" (Tem "poison heart" ["rock", "reggaeton"] "ramones") == True,
      aplicaT "reggaeton" (Tem "poison heart" ["rock"] "ramones") == False]