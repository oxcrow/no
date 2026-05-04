# Write a string to terminal
data $str = { b "Executing: 001C.q", b 0 }

export function w $main () {
@start
    # Execute puts C function to print to terminal
    %status = w call $puts(l $str)
    ret 0
}
