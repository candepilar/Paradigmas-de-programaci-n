{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant ==" #-}
module Reproductor ( Reproductor, nuevoR, archivosR, listaParaR, temasR, playR, actualR, avanzarR, retrocederR,
reiniciarR )
where
import Tipos
import Tema
import Playlist
import FileSystem
import Data.Binary.Get (skip)
data Reproductor = RP FileSystem Playlist deriving (Eq, Show)

nuevoR :: FileSystem -> Playlist -> Reproductor
--Crea un nuevo reproductor a partir de un FileSystem y una nueva Playlist con su lista de temas vacıa.
nuevoR filesSystem playlist = RP filesSystem playlist

archivosR :: Reproductor -> FileSystem
--Dado un reproductor devuelve su FileSystem.
archivosR (RP fileSystem playlist) = fileSystem

listaParaR :: Etiqueta -> Reproductor -> [Tema]
--Dado una etiqueta y un reproductor devuelve la lista de temas del FileSystem (correspondiente a la etiqueta).
listaParaR etiqueta (RP fileSystem playlist) = [x | x <- filtrarF etiqueta fileSystem, aplicaT etiqueta x == False ]

temasR :: Reproductor -> [Tema]
--Dado un reproductor devuelve la lista de temas.
temasR (RP fileSystem playlist) = temasF fileSystem

playR :: Reproductor ->Etiqueta ->Reproductor
--Dado un reproductor y una etiqueta coloca los temas a reproducir en la playlist.
playR (RP fileSystem playlist) etiqueta = RP fileSystem (nuevaP (filtrarF etiqueta fileSystem))

actualR :: Reproductor ->Tema
--Dado un reproductor devuelve el tema que esta actualmente de la playlist.
actualR (RP fileSystem playlist) = actualP playlist

avanzarR :: Reproductor ->Reproductor
--Dado un reproductor devuelve un reproductor pero con el tema siguiente.
avanzarR (RP fileSystem playlist) = RP fileSystem (skipP playlist)

retrocederR :: Reproductor ->Reproductor
--Dado un reproductor devuelve un reproductor pero con el tema anterior.
retrocederR (RP fileSystem playlist) = RP fileSystem (backP playlist)

reiniciarR :: Reproductor ->Reproductor
--Dado un reproductor devuelve un reproductor pero con el tema del inicio (vuelve a resetear).
reiniciarR (RP fileSystem playlist)  = RP fileSystem (resetP playlist)

testR = [nuevoR nuevoF (nuevaP [nuevoT "torero" "chayanne"]) == RP nuevoF (nuevaP [nuevoT "torero" "chayanne"]),
        archivosR(RP nuevoF (nuevaP [nuevoT "torero" "chayanne"])) == nuevoF,
        listaParaR "rock" (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (nuevaP [nuevoT "torero" "chayanne"])) == [nuevoT "beautiful" "tan bionica" ],
        temasR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (nuevaP [nuevoT "torero" "chayanne"])) == [nuevoT "beautiful" "tan bionica"],
        playR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (nuevaP [nuevoT "torero" "chayanne"])) "rock" == RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (nuevaP [nuevoT "beautiful" "tan bionica" ]),
        actualR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (skipP (nuevaP [nuevoT "beautiful" "tan bionica"]))) == nuevoT "beautiful" "tan bionica",
        avanzarR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"])))  == RP  (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (skipP (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"]))),
        retrocederR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"])))  == RP  (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (backP (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"]))),
        reiniciarR (RP (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"])))  == RP  (agregarF (nuevoT "beautiful" "tan bionica") "rock" nuevoF) (resetP (skipP (nuevaP [nuevoT "beautiful" "tan bionica", nuevoT "poison heart" "ramones"])))]