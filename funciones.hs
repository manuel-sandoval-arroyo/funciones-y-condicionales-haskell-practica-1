-- 1. Distancia entre dos puntos en el plano cartesiano:
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x_1, y_1) (x_2, y_2) = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2)

-- 2. Hipotenusa de un triangulo rectángulo
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (b^2 + h^2)

-- 3. Pendiente de la recta que pasa por dos puntos
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x_1, y_1) (x_2, y_2) = (y_2 - y_1)/(x_2 - x_1)

-- 4. Raíces de una ecuación cuadrática
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c =
    let discriminante = sqrt (b^2 - 4 * a * c)
    in ((-b - discriminante)/(2*a), (-b + discriminante)/(2*a))

-- 5. Área de un triángulo por medio de la fórmula de Herón
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c =
    let s = (a + b + c)/2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- 6. Función comparador
comparador :: Int -> Int -> Int
comparador x y =
  if x == y then 0
  else if x < y then -1
  else 1

-- 7. Máximo entre tres números
maximo :: Int -> Int -> Int -> Int
maximo x y z = 
  if x >= y && x >= z then x
  else if y >= x && y >= z then y
  else z

-- 8. Números ordenados de forma descendente
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = x > y && y > z && z >  w
