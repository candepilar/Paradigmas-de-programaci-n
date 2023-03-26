module Tipos where
import Data.List
type Datos = String
type Etiqueta = String
type Nombre = String

insertar :: Ord a => a -> [ a ] -> [ a ]
--Entrega una nueva lista con los elementos originales y el agregado ordenados naturalmente.
insertar x xs = sort (x:xs)

testT = [ insertar "la" ["ganamos", "tercera"] == ["ganamos", "la", "tercera"]]        
