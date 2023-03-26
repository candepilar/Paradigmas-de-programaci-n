{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Playlist ( Playlist, nuevaP, actualP, skipP, backP, resetP )
where
import Tipos
import Tema 

data Playlist = Play Int [ Tema ] deriving (Eq, Show)

nuevaP :: [ Tema ] -> Playlist
--A partir de una lista de temas crea una nueva Playlist con su indice en cero.
nuevaP newtema = Play 0 newtema

actualP :: Playlist -> Tema
--Dada una Playlist devuelve el tema en la posicion indicada por el ındice.
actualP (Play indice newtema)=   newtema!!(indice-1)

skipP :: Playlist -> Playlist
--Devuelve una Playlist con su ındice aumentado en uno.
skipP (Play indice newtema) = Play (indice+1) newtema

backP :: Playlist -> Playlist
--Idem anterior pero con el ındice decrementado en uno.
backP (Play indice newtema) = Play (indice-1) newtema

resetP :: Playlist -> Playlist
--Dada una Playlist crea una nueva con la lista de temas de la original.
resetP (Play indice newtema) = nuevaP newtema

testP = [nuevaP [nuevoT "torero" "chayanne"] == Play 0 [nuevoT "torero" "chayanne"],
        actualP (Play 3 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones"]) == nuevoT "poison heart" "ramones",
        skipP (Play 3 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"]) == Play 4 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"],
        backP (Play 3 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"]) == Play 2 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"],
        resetP (Play 3 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"]) == Play 0 [nuevoT "torero" "chayanne", nuevoT "playa del ingles" "quevedo", nuevoT "poison heart" "ramones", nuevoT "ella" "tan bionica"]]