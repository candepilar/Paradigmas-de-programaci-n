{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
module FileSystem ( FileSystem, nuevoF, etiquetasF, temasF, agregarF, filtrarF )
where
import Tipos
import Tema
data FileSystem = FS [Etiqueta] [Tema] deriving (Eq, Show)

nuevoF :: FileSystem
--Crea un nuevo FileSystem con sus listas vacıas.
nuevoF = FS [] []

etiquetasF :: FileSystem -> [ Etiqueta ]
--Dado un FileSystem devuelve la etiqueta.
etiquetasF (FS etiqueta tema) = etiqueta

temasF :: FileSystem -> [ Tema ]
--Dado un FileSystem devuelve el tema.
temasF (FS etiqueta tema) = tema

agregarF :: Tema -> Etiqueta -> FileSystem ->FileSystem
--Agrega el tema y sus etiquetas de ser necesario.
agregarF nuevotema nuevaetiqueta (FS etiquetas temas) = FS (etiquetas++[nuevaetiqueta]) (temas++[nuevotema])

filtrarF :: Etiqueta -> FileSystem ->[ Tema ]
--Dado una etiqueta y un FileSystem devuelve la lista correspondiente a la etiqueta.
filtrarF etiqueta (FS etiquetas tema)= [x | x <- tema, aplicaT etiqueta x == False]
      
testF = [ nuevoF == FS [] [],
      etiquetasF (FS ["rock"] [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"]) == ["rock"],
      temasF (FS ["rock"] [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"]) == [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"],
      agregarF (nuevoT "beautiful" "tan bionica") "pop" (FS ["rock"] [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"]) == FS ["rock", "pop"] [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones", nuevoT "beautiful" "tan bionica" ],
      filtrarF "rock" (FS ["rock"] [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"]) == [nuevoT "torero" "chayanne", nuevoT "poison heart" "ramones"]]