open System

// Метод дихотомии
let dichotomy f a b accuracy =
    let rec loop a b =
        let c = (a + b) / 2.0
        if abs(f c) < accuracy || (b - a) / 2.0 < accuracy then c
        else if f a * f c < 0.0 then loop a c
        else if f c * f b < 0.0 then loop c b
        else if abs(f a) < abs(f c) then loop a c
        else loop c b
    loop a b

// Метод итераций
let iteration_method g x0 accuracy maxIter =
    let rec loop x i =
        let x1 = g x
        if abs(x1 - x) < accuracy || i >= maxIter then x1
        else loop x1 (i + 1)
    loop x0 0

// Метод Ньютона
let newton_method f f' x0 accuracy maxIter =
    let rec loop x i =
        let x1 = x - f x / f' x
        if abs(x1 - x) < accuracy || i >= maxIter then x1
        else loop x1 (i + 1)
    loop x0 0

// arctan(x) = x
let Eq1 x = Math.Atan(x) - x
let Eq1' x = 1.0 / (1.0 + x * x) - 1.0

// e ** (x ** 2) = x
let Eq2 x = Math.Exp(x ** 2.0) - x
let Eq2' x = 2.0 * x * Math.Exp(x ** 2.0) - 1.0

// 1 / (4 - x ^ 4) = x
let Eq3 x = 1.0 / (4.0 - x ** 4.0) - x
let Eq3' x = (4.0 * x ** 3.0) / (4.0 - x ** 4.0) ** 2.0 - 1.0

let accuracy = 1e-6
let maxIter = 1000

// Применяем функции для 1-го уравнения
let x1_dichotomy = dichotomy Eq1 0.0 0.5 accuracy
let x1_fixedPoint = iteration_method (fun x -> Math.Atan(x)) 0.5 accuracy maxIter
let x1_newton = newton_method Eq1 Eq1' 0.5 accuracy maxIter

// Применяем функции для 2-го уравнения
let x2_dichotomy = dichotomy Eq2 0.0 1.0 accuracy
let x2_fixedPoint = iteration_method (fun x -> Math.Exp(x ** 2.0)) 1.0 accuracy maxIter
let x2_newton = newton_method Eq2 Eq2' 1.0 accuracy maxIter

// Применяем функции для 3-го уравнения
let x3_dichotomy = dichotomy Eq3 0.0 1.0 accuracy
let x3_fixedPoint = iteration_method (fun x -> 1.0 / (4.0 - x ** 4.0)) 1.0 accuracy maxIter
let x3_newton = newton_method Eq3 Eq3' 1.0 accuracy maxIter

// Вывод
printfn "%-20s %-20s %-20s %-20s" "Метод" "Корень 1" "Корень 2" "Корень 3"
printfn "%-20s %-20.10f %-20.10f %-20.10f" "Дихотомия" x1_dichotomy x2_dichotomy x3_dichotomy
printfn "%-20s %-20.10f %-20.10f %-20.10f" "Итерации" x1_fixedPoint x2_fixedPoint x3_fixedPoint
printfn "%-20s %-20.10f %-20.10f %-20.10f" "Ньютон" x1_newton x2_newton x3_newton

// Значения получаются:
// Method               Root for Eq1         Root for Eq2         Root for Eq3        
// dichotomy            0.0078125000         0.5000009537         0.2502460480        
// Fixed Point          0.0386096654         Infinity             0.2502453410        
// Newton               0.0000016027         2.6022142604         0.2502453409  

// Для второго уравнения методы разнятся, ни один из ответов не верный,
// так как функция x = e ** (x ** 2) не имеет решений.
// Для первого и третьего уравнения вывод верный. 

