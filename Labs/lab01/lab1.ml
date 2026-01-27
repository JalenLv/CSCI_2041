open List ;;

let rec howMany e l =
    if l = [] then
        0
    else
        if hd l = e then
            1 + (howMany e (tl l))
        else
            howMany e (tl l) ;;

let rec delete e l =
    if l = [] then
        []
    else
        let head = hd l in
            if head = e then
                delete e (tl l)
            else
                head :: (delete e (tl l)) ;;

let mean l =
    let rec sum l =
        if l = [] then
            0.0
        else
            (hd l) +. (sum (tl l)) in
    let rec len l =
        if l = [] then
            0
        else
            1 + (len (tl l)) in
    let sum = sum l in
    let len = len l in
        sum /. (float_of_int len) ;;

