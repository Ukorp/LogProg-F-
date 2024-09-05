open System

let a = 0.0
let b = 0.5
let step = 0.1
let eps = 1e-10

// Dumb Taylor
let naiveTaylor x eps =
    let rec loop n term sum =
        if abs(term) < eps then sum, n
        else
            let next = (-1.0)**(float (n)) * x**(float (2*n+1)) / float (2*n+1)
            loop (n+1) next (sum + next)
    loop 1 x x

// Smart Taylor
let smartTaylor x eps =
    let rec loop n term sum =
        if abs(term) < eps then sum, n
        else
            let next = -term * x * x * float (2*n-1) / float (2*n+1)
            loop (n+1) next (sum + next)
    loop 1 x x

// Вывод
printfn "%-6s %-10s %-12s %-8s %-12s %-10s" "x" "Встроенная" "Smart Taylor" "Итерации" "Dumb Taylor" "Итерации"
let rec loop x =
    if x <= b then
        let builtin = Math.Atan(x)
        let (smartVal, smartTerms) = smartTaylor x eps
        let (naiveVal, naiveTerms) = naiveTaylor x eps
        printfn "%-6.2f %-10.8f %-12.8f %-8d %-12.8f %-10d" x builtin smartVal smartTerms naiveVal naiveTerms
        loop (x + step)
loop a
