# Return an exit code of integer value after adding two integers.
# A few type casts are done to get accustomed to them.

function w $add(w %a, w %b) {
@start
    %sum = w add %a, %b
    ret %sum
}

function w $addLong(l %a, l %b) {
@start
    %sum = l add %a, %b
    ret %sum
}

function w $addMixed(w %a, l %b) {
@start
    %A = l extuw %a
    %sum = l add %A, %b
    %low = w copy %sum
    ret %low
}

export function w $main() {
@start
    %sum1 = w call $add(w 0, w 0)
    %sum2 = l call $addLong(l 0, l 0)
    %sum  = w call $addMixed(w %sum1, l %sum2)
    ret %sum
}
