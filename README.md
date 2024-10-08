# Introducción a funciones en Haskell

## Materia

Estructuras discretas

## Integrantes

- Manuel Sandoval Arroyo

## Documentación

Este archivo README documenta varias funciones de Haskell. Cada función se describe a continuación:

- **Distancia entre dos puntos en el plano cartesiano:**

  ```haskell
  distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
  distanciaPuntos (x_1, y_1) (x_2, y_2) = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2)
  ```

  Esta función calcula la distancia entre dos puntos en un plano cartesiano.

- **Hipotenusa de un triángulo rectángulo:**

  ```haskell
  hipotenusa :: (Float, Float) -> Float
  hipotenusa (b, h) = sqrt (b^2 + h^2)
  ```

  Esta función calcula la hipotenusa de un triángulo rectángulo.

- **Pendiente de la recta que pasa por dos puntos:**

  ```haskell
  pendiente :: (Float, Float) -> (Float, Float) -> Float
  pendiente (x_1, y_1) (x_2, y_2) = (y_2 - y_1)/(x_2 - x_1)
  ```

  Esta función calcula la pendiente de la recta que pasa por dos puntos.

- **Raíces de una ecuación cuadrática:**

  ```haskell
  raices :: Float -> Float -> Float -> (Float, Float)
  raices a b c =
          let discriminant = sqrt (b^2 - 4 * a * c)
          in ((-b - discriminant)/(2*a), (-b + discriminant)/(2*a))
  ```

  Esta función calcula las raíces de una ecuación cuadrática.

- **Área de un triángulo por medio de la fórmula de Herón:**

  ```haskell
  areaTriangulo :: Float -> Float -> Float -> Float
  areaTriangulo a b c =
          let s = (a + b + c)/2
          in sqrt (s * (s - a) * (s - b) * (s - c))
  ```

  Esta función calcula el área de un triángulo utilizando la fórmula de Herón.

- **Función comparador:**

  ```haskell
  comparador :: Int -> Int -> Int
  comparador x y
      if x == then 0
      if x < y then -1| x < y = -1
      else 1
  ```

  Esta función compara dos enteros y devuelve 0 si son iguales, -1 si el primer entero es menor y 1 si el primer entero es mayor.

- **Máximo entre tres números:**

  ```haskell
  maximo :: Int -> Int -> Int -> Int
  maximo x y z = max x (max y z)
  ```

  Esta función devuelve el valor máximo entre tres enteros.

- **Números ordenados de forma descendente:**
  ```haskell
  esDescendente :: Int -> Int -> Int -> Int -> Bool
  esDescendente x y z w = x > y && y > z && z >  w
  ```
  Esta función verifica si cuatro enteros están en orden descendente.
