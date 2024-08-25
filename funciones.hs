-- 1. Distancia entre dos puntos en el plano cartesiano:
distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x_1, y_1) (x_2, y_2) = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2)

-- 2. Hipotenusa de un triangulo rectángulo
hipotenusa :: (Float, Float) -> Float
hipotenusa (b, h) = sqrt (b^2 + h^2)

-- 3. Pendiente de la recta que pasa por dos puntos
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x_1, y_1) (x_2, y_2) = (y_2 - y_1)/(x_2 - x_1)

-- 4. Raíces de una ecuación cuadrática
quadraticFormula :: Float -> Float -> Float -> (Float, Float)
quadraticFormula a b c =
    let discriminant = sqrt (b^2 - 4 * a * c)
    in ((-b - discriminant)/(2*a), (-b + discriminant)/(2*a))

-- 5. Área de un triángulo por medio de la fórmula de Herón
area :: Float -> Float -> Float -> Float
area a b c =
    let s = (a + b + c)/2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- 6. Función comparador
comparar :: Int -> Int -> Int
comparar x y 
  | x == y = 0
  | x < y = -1
  | otherwise = 1

-- 7. Máximo entre tres números
maximo :: Int -> Int -> Int -> Int
maximo x y z = max x (max y z)

-- 8. Números ordenados de forma descendente
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = x > y && y > z && z >  w
